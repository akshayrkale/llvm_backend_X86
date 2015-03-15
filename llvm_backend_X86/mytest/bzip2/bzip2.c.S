	.file	"bzip2.c.bc"
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
	.file	1 "bzip2.c"
	.file	2 "../musl-1.0.4/include/bits/alltypes.h"
	.file	3 "../musl-1.0.4/include/bits/stat.h"
	.text
	.globl	fopen_output_safely
	.align	16, 0x90
	.type	fopen_output_safely,@function
fopen_output_safely:                    # @fopen_output_safely
.Lfunc_begin0:
	.loc	1 1103 0                # bzip2.c:1103:0
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
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 1107 0 prologue_end   # bzip2.c:1107:0
.Ltmp5:
	movq	-16(%rbp), %rdi
	movq	%rsi, -24(%rbp)
	movq	$193, %rsi
	movq	$384, %rdx              # imm = 0x180
	xorq	%rax, %rax
	callq	open
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	%rcx, %rdx
	movl	%eax, -36(%rbp)
	je	.LBB0_1
# BB#2:                                 # %if.end
	.loc	1 1109 0                # bzip2.c:1109:0
	movq	-24(%rbp), %rsi
	movl	-36(%rbp), %edi
	callq	fdopen
	testq	%rax, %rax
	movq	%rax, -32(%rbp)
	jne	.LBB0_4
# BB#3:                                 # %if.then3
	.loc	1 1110 0                # bzip2.c:1110:0
.Ltmp6:
	movl	-36(%rbp), %edi
	callq	close
.Ltmp7:
.LBB0_4:                                # %if.end5
	.loc	1 1111 0                # bzip2.c:1111:0
	movq	-32(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB0_5
.LBB0_1:                                # %if.then
	.loc	1 1108 0                # bzip2.c:1108:0
.Ltmp8:
	movq	$0, -8(%rbp)
.Ltmp9:
.LBB0_5:                                # %return
	.loc	1 1115 0                # bzip2.c:1115:0
	movq	-8(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	retq
.Ltmp10:
.Ltmp11:
	.size	fopen_output_safely, .Ltmp11-fopen_output_safely
.Lfunc_end0:
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
.Lfunc_begin1:
	.loc	1 1910 0                # bzip2.c:1910:0
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
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
.Ltmp18:
	.cfi_offset %rbx, -56
.Ltmp19:
	.cfi_offset %r12, -48
.Ltmp20:
	.cfi_offset %r13, -40
.Ltmp21:
	.cfi_offset %r14, -32
.Ltmp22:
	.cfi_offset %r15, -24
	xorq	%rax, %rax
	movl	%eax, -44(%rbp)
	movl	%edi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	.loc	1 1924 0 prologue_end   # bzip2.c:1924:0
.Ltmp23:
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 1925 0                # bzip2.c:1925:0
	movb	%al, smallMode(%rip)
	.loc	1 1926 0                # bzip2.c:1926:0
	movb	%al, keepInputFiles(%rip)
	.loc	1 1927 0                # bzip2.c:1927:0
	movb	%al, forceOverwrite(%rip)
	movq	$1, %rcx
	.loc	1 1928 0                # bzip2.c:1928:0
	movb	%cl, noisy(%rip)
	.loc	1 1929 0                # bzip2.c:1929:0
	movl	%eax, verbosity(%rip)
	movq	$9, %rcx
	.loc	1 1930 0                # bzip2.c:1930:0
	movl	%ecx, blockSize100k(%rip)
	.loc	1 1931 0                # bzip2.c:1931:0
	movb	%al, testFailsExist(%rip)
	.loc	1 1932 0                # bzip2.c:1932:0
	movb	%al, unzFailsExist(%rip)
	.loc	1 1933 0                # bzip2.c:1933:0
	movl	%eax, numFileNames(%rip)
	.loc	1 1934 0                # bzip2.c:1934:0
	movl	%eax, numFilesProcessed(%rip)
	movq	$30, %rcx
	.loc	1 1935 0                # bzip2.c:1935:0
	movl	%ecx, workFactor(%rip)
	.loc	1 1936 0                # bzip2.c:1936:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	.loc	1 1937 0                # bzip2.c:1937:0
	movl	%eax, exitValue(%rip)
	.loc	1 1938 0                # bzip2.c:1938:0
	movl	%eax, -64(%rbp)
	movl	%eax, -60(%rbp)
	.loc	1 1941 0                # bzip2.c:1941:0
	movabsq	$mySIGSEGVorSIGBUScatcher, %rbx
	movq	$11, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	$7, %rdi
	.loc	1 1944 0                # bzip2.c:1944:0
	movq	%rbx, %rsi
	callq	signal
	.loc	1 1948 0                # bzip2.c:1948:0
	movabsq	$inName, %rdi
	movabsq	$.L.str6, %rbx
	movq	%rbx, %rsi
	callq	copyFileName
	.loc	1 1949 0                # bzip2.c:1949:0
	movabsq	$outName, %rdi
	movq	%rbx, %rsi
	callq	copyFileName
	.loc	1 1951 0                # bzip2.c:1951:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rsi
	movabsq	$progNameReally, %rdi
	callq	copyFileName
	.loc	1 1952 0                # bzip2.c:1952:0
	movq	$progNameReally, progName(%rip)
	.loc	1 1953 0                # bzip2.c:1953:0
.Ltmp24:
	movq	$progNameReally, -72(%rbp)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_4:                                # %for.inc
                                        #   in Loop: Header=BB1_1 Depth=1
	incq	-72(%rbp)
.LBB1_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	-72(%rbp), %rax
	movsbq	(%rax), %rax
	testq	%r13, %rax
	je	.LBB1_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 1954 0                # bzip2.c:1954:0
.Ltmp25:
	movq	-72(%rbp), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$47, %rax
	jne	.LBB1_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	-72(%rbp), %rax
	incq	%rax
	movq	%rax, progName(%rip)
	jmp	.LBB1_4
.Ltmp26:
.LBB1_5:                                # %for.end
	.loc	1 1960 0                # bzip2.c:1960:0
	movq	$0, -80(%rbp)
	.loc	1 1961 0                # bzip2.c:1961:0
	movabsq	$.L.str7, %rsi
	leaq	-80(%rbp), %rbx
	movq	%rbx, %rdi
	callq	addFlagsFromEnvVar
	.loc	1 1962 0                # bzip2.c:1962:0
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdi
	callq	addFlagsFromEnvVar
	movq	$1, %rax
	.loc	1 1963 0                # bzip2.c:1963:0
.Ltmp27:
	movl	%eax, -60(%rbp)
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_7:                                # %for.inc15
                                        #   in Loop: Header=BB1_6 Depth=1
	.loc	1 1964 0                # bzip2.c:1964:0
	movslq	-60(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	-80(%rbp), %rdi
	movq	(%rcx,%rax,8), %rsi
	callq	snocString
	.loc	1 1963 0                # bzip2.c:1963:0
	movl	-60(%rbp), %ecx
	.loc	1 1964 0                # bzip2.c:1964:0
	movq	%rax, -80(%rbp)
	.loc	1 1963 0                # bzip2.c:1963:0
	incq	%rcx
	movl	%ecx, -60(%rbp)
.LBB1_6:                                # %for.cond9
                                        # =>This Inner Loop Header: Depth=1
	movl	-48(%rbp), %eax
	movslq	-60(%rbp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jle	.LBB1_7
.Ltmp28:
# BB#8:                                 # %for.end16
	movq	$7, %rax
	.loc	1 1968 0                # bzip2.c:1968:0
	movl	%eax, longestFileName(%rip)
	movq	$1, %rax
	.loc	1 1970 0                # bzip2.c:1970:0
	movb	%al, -89(%rbp)
	.loc	1 1971 0                # bzip2.c:1971:0
.Ltmp29:
	movq	-80(%rbp), %rax
	xorq	%r14, %r14
.Ltmp30:
	.loc	1 1969 0                # bzip2.c:1969:0
	movl	%r14d, numFileNames(%rip)
	.loc	1 1971 0                # bzip2.c:1971:0
.Ltmp31:
	movq	%rax, -88(%rbp)
	.loc	1 1972 0                # bzip2.c:1972:0
.Ltmp32:
	movabsq	$.L.str9, %r15
	jmp	.LBB1_9
.Ltmp33:
	.align	16, 0x90
.LBB1_16:                               # %for.inc45
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1971 0                # bzip2.c:1971:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_9:                                # %for.cond17
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, -88(%rbp)
	je	.LBB1_17
# BB#10:                                # %for.body20
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1972 0                # bzip2.c:1972:0
.Ltmp34:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_12
# BB#11:                                # %if.then24
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1972 0                # bzip2.c:1972:0
.Ltmp35:
	movb	%r14b, -89(%rbp)
	jmp	.LBB1_16
.Ltmp36:
	.align	16, 0x90
.LBB1_12:                               # %if.end25
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1973 0                # bzip2.c:1973:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	jne	.LBB1_14
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	-89(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB1_16
.Ltmp37:
.LBB1_14:                               # %if.end33
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1974 0                # bzip2.c:1974:0
	movl	numFileNames(%rip), %eax
	.loc	1 1975 0                # bzip2.c:1975:0
.Ltmp38:
	movq	-88(%rbp), %rcx
.Ltmp39:
	.loc	1 1974 0                # bzip2.c:1974:0
	incq	%rax
	movl	%eax, numFileNames(%rip)
	.loc	1 1975 0                # bzip2.c:1975:0
.Ltmp40:
	movslq	longestFileName(%rip), %rbx
	movq	(%rcx), %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB1_16
# BB#15:                                # %if.then40
                                        #   in Loop: Header=BB1_9 Depth=1
	.loc	1 1976 0                # bzip2.c:1976:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	callq	strlen
	movl	%eax, longestFileName(%rip)
	jmp	.LBB1_16
.Ltmp41:
.LBB1_17:                               # %for.end46
	.loc	1 1981 0                # bzip2.c:1981:0
	movl	numFileNames(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_18
# BB#19:                                # %if.else
	movq	$3, %rax
	jmp	.LBB1_20
.LBB1_18:                               # %if.then49
	movq	$1, %rax
.LBB1_20:                               # %if.end50
	.loc	1 1982 0                # bzip2.c:1982:0
	movl	%eax, srcMode(%rip)
.Ltmp42:
	.loc	1 1989 0                # bzip2.c:1989:0
	movq	progName(%rip), %rdi
	movq	$1, %rax
.Ltmp43:
	.loc	1 1987 0                # bzip2.c:1987:0
	movl	%eax, opMode(%rip)
	.loc	1 1989 0                # bzip2.c:1989:0
.Ltmp44:
	movabsq	$.L.str10, %rsi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB1_22
# BB#21:                                # %lor.lhs.false
	.loc	1 1990 0                # bzip2.c:1990:0
	movq	progName(%rip), %rdi
	movabsq	$.L.str11, %rsi
	callq	strstr
	testq	%rax, %rax
	je	.LBB1_23
.LBB1_22:                               # %if.then57
	movq	$2, %rax
	.loc	1 1991 0                # bzip2.c:1991:0
	movl	%eax, opMode(%rip)
.Ltmp45:
.LBB1_23:                               # %if.end58
	.loc	1 1993 0                # bzip2.c:1993:0
	movq	progName(%rip), %rdi
	movabsq	$.L.str12, %rsi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB1_27
# BB#24:                                # %lor.lhs.false62
	.loc	1 1994 0                # bzip2.c:1994:0
	movq	progName(%rip), %rdi
	movabsq	$.L.str13, %rsi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB1_27
# BB#25:                                # %lor.lhs.false66
	.loc	1 1995 0                # bzip2.c:1995:0
	movq	progName(%rip), %rdi
	movabsq	$.L.str14, %rsi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB1_27
# BB#26:                                # %lor.lhs.false70
	.loc	1 1996 0                # bzip2.c:1996:0
	movq	progName(%rip), %rdi
	movabsq	$.L.str15, %rsi
	callq	strstr
	testq	%rax, %rax
	je	.LBB1_30
.LBB1_27:                               # %if.then74
	.loc	1 1998 0                # bzip2.c:1998:0
.Ltmp46:
	movl	numFileNames(%rip), %eax
	movq	$2, %rcx
	.loc	1 1997 0                # bzip2.c:1997:0
	movl	%ecx, opMode(%rip)
	.loc	1 1998 0                # bzip2.c:1998:0
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB1_29
# BB#28:                                # %select.mid
	movq	$2, %rax
.LBB1_29:                               # %select.end
	movl	%eax, srcMode(%rip)
.Ltmp47:
.LBB1_30:                               # %if.end77
	.loc	1 2003 0                # bzip2.c:2003:0
	movq	-80(%rbp), %rax
	movq	%rax, -88(%rbp)
	.loc	1 2004 0                # bzip2.c:2004:0
.Ltmp48:
	movabsq	$.L.str9, %rbx
	movq	$1, %r14
	movq	$2, %r15
	movq	$3, %r12
	jmp	.LBB1_31
.Ltmp49:
	.align	16, 0x90
.LBB1_68:                               # %for.inc138
                                        #   in Loop: Header=BB1_31 Depth=1
	.loc	1 2003 0                # bzip2.c:2003:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_31:                               # %for.cond78
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_36 Depth 2
	cmpq	$0, -88(%rbp)
	je	.LBB1_69
# BB#32:                                # %for.body81
                                        #   in Loop: Header=BB1_31 Depth=1
	.loc	1 2004 0                # bzip2.c:2004:0
.Ltmp50:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB1_69
.Ltmp51:
# BB#33:                                # %if.end87
                                        #   in Loop: Header=BB1_31 Depth=1
	.loc	1 2005 0                # bzip2.c:2005:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	jne	.LBB1_68
# BB#34:                                # %land.lhs.true93
                                        #   in Loop: Header=BB1_31 Depth=1
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	1(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	je	.LBB1_68
# BB#35:                                # %if.then99
                                        #   in Loop: Header=BB1_31 Depth=1
	.loc	1 2006 0                # bzip2.c:2006:0
.Ltmp52:
	movl	%r14d, -64(%rbp)
	jmp	.LBB1_36
	.align	16, 0x90
.LBB1_67:                               # %for.inc134
                                        #   in Loop: Header=BB1_36 Depth=2
	movl	-64(%rbp), %eax
	incq	%rax
	movl	%eax, -64(%rbp)
.LBB1_36:                               # %for.cond100
                                        #   Parent Loop BB1_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-88(%rbp), %rax
	movslq	-64(%rbp), %rcx
	movq	(%rax), %rax
	movsbq	(%rax,%rcx), %rax
	testq	%r13, %rax
	je	.LBB1_68
# BB#37:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2007 0                # bzip2.c:2007:0
.Ltmp53:
	movq	-88(%rbp), %rax
	movslq	-64(%rbp), %rcx
	movq	(%rax), %rax
	movsbq	(%rax,%rcx), %rax
	cmpq	$75, %rax
	jg	.LBB1_40
# BB#38:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	addq	$-49, %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	ja	.LBB1_42
# BB#39:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	jmpq	*.LJTI1_1(,%rax,8)
.LBB1_56:                               # %sw.bb119
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2016 0                # bzip2.c:2016:0
.Ltmp54:
	movl	%r14d, blockSize100k(%rip)
	jmp	.LBB1_67
	.align	16, 0x90
.LBB1_40:                               # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	cmpq	$85, %rax
	jg	.LBB1_44
# BB#41:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	andq	%r13, %rax
	cmpq	$76, %rax
	je	.LBB1_46
	jmp	.LBB1_42
	.align	16, 0x90
.LBB1_44:                               # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	cmpq	$98, %rax
	jg	.LBB1_47
# BB#45:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	andq	%r13, %rax
	cmpq	$86, %rax
	jne	.LBB1_42
.LBB1_46:                               # %sw.bb128
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2026 0                # bzip2.c:2026:0
	callq	license
	jmp	.LBB1_67
.LBB1_47:                               # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	addq	$-99, %rax
	andq	%r13, %rax
	cmpq	$23, %rax
	ja	.LBB1_42
# BB#48:                                # %for.body107
                                        #   in Loop: Header=BB1_36 Depth=2
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_66:                               # %sw.bb
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2008 0                # bzip2.c:2008:0
	movl	%r15d, srcMode(%rip)
	jmp	.LBB1_67
.LBB1_57:                               # %sw.bb120
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2017 0                # bzip2.c:2017:0
	movl	%r15d, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_58:                               # %sw.bb121
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2018 0                # bzip2.c:2018:0
	movl	%r12d, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_59:                               # %sw.bb122
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$4, %rax
	.loc	1 2019 0                # bzip2.c:2019:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_60:                               # %sw.bb123
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$5, %rax
	.loc	1 2020 0                # bzip2.c:2020:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_61:                               # %sw.bb124
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$6, %rax
	.loc	1 2021 0                # bzip2.c:2021:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_62:                               # %sw.bb125
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$7, %rax
	.loc	1 2022 0                # bzip2.c:2022:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_63:                               # %sw.bb126
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$8, %rax
	.loc	1 2023 0                # bzip2.c:2023:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_64:                               # %sw.bb127
                                        #   in Loop: Header=BB1_36 Depth=2
	movq	$9, %rax
	.loc	1 2024 0                # bzip2.c:2024:0
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_67
.LBB1_49:                               # %sw.bb112
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2009 0                # bzip2.c:2009:0
	movl	%r15d, opMode(%rip)
	jmp	.LBB1_67
.LBB1_51:                               # %sw.bb114
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2011 0                # bzip2.c:2011:0
	movb	%r14b, forceOverwrite(%rip)
	jmp	.LBB1_67
.LBB1_53:                               # %sw.bb116
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2013 0                # bzip2.c:2013:0
	movb	%r14b, keepInputFiles(%rip)
	jmp	.LBB1_67
.LBB1_55:                               # %sw.bb118
                                        #   in Loop: Header=BB1_36 Depth=2
	xorq	%rax, %rax
	.loc	1 2015 0                # bzip2.c:2015:0
	movb	%al, noisy(%rip)
	jmp	.LBB1_67
.LBB1_54:                               # %sw.bb117
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2014 0                # bzip2.c:2014:0
	movb	%r14b, smallMode(%rip)
	jmp	.LBB1_67
.LBB1_52:                               # %sw.bb115
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2012 0                # bzip2.c:2012:0
	movl	%r12d, opMode(%rip)
	jmp	.LBB1_67
.LBB1_65:                               # %sw.bb129
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2027 0                # bzip2.c:2027:0
	movl	verbosity(%rip), %eax
	incq	%rax
	movl	%eax, verbosity(%rip)
	jmp	.LBB1_67
.LBB1_50:                               # %sw.bb113
                                        #   in Loop: Header=BB1_36 Depth=2
	.loc	1 2010 0                # bzip2.c:2010:0
	movl	%r14d, opMode(%rip)
	jmp	.LBB1_67
.LBB1_42:                               # %sw.default
	.loc	1 2031 0                # bzip2.c:2031:0
	movq	-88(%rbp), %rax
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movq	(%rax), %rcx
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	callq	fprintf
	.loc	1 2033 0                # bzip2.c:2033:0
	movq	progName(%rip), %rdi
.LBB1_43:                               # %sw.default
	callq	usage
	movq	$1, %rdi
	.loc	1 2034 0                # bzip2.c:2034:0
	callq	exit
.Ltmp55:
.LBB1_69:                               # %for.end140
	.loc	1 2042 0                # bzip2.c:2042:0
	movq	-80(%rbp), %rax
	movq	%rax, -88(%rbp)
	.loc	1 2043 0                # bzip2.c:2043:0
.Ltmp56:
	movabsq	$.L.str9, %rbx
.Ltmp57:
	.loc	1 2044 0                # bzip2.c:2044:0
	movabsq	$.L.str17, %r14
	movq	$2, %r12
	.loc	1 2045 0                # bzip2.c:2045:0
.Ltmp58:
	movabsq	$.L.str18, %r15
	jmp	.LBB1_70
.Ltmp59:
	.align	16, 0x90
.LBB1_103:                              # %for.inc281
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2042 0                # bzip2.c:2042:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_70:                               # %for.cond141
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, -88(%rbp)
	je	.LBB1_107
# BB#71:                                # %for.body144
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2043 0                # bzip2.c:2043:0
.Ltmp60:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB1_107
.Ltmp61:
# BB#72:                                # %if.end150
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2044 0                # bzip2.c:2044:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%r14, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_74
# BB#73:                                # %if.then155
                                        #   in Loop: Header=BB1_70 Depth=1
	movl	%r12d, srcMode(%rip)
	jmp	.LBB1_103
	.align	16, 0x90
.LBB1_74:                               # %if.else156
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2045 0                # bzip2.c:2045:0
.Ltmp62:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_76
# BB#75:                                # %if.then161
                                        #   in Loop: Header=BB1_70 Depth=1
	movl	%r12d, opMode(%rip)
	jmp	.LBB1_103
	.align	16, 0x90
.LBB1_76:                               # %if.else162
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2046 0                # bzip2.c:2046:0
.Ltmp63:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str19, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_78
# BB#77:                                # %if.then167
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movl	%eax, opMode(%rip)
	jmp	.LBB1_103
.LBB1_78:                               # %if.else168
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2047 0                # bzip2.c:2047:0
.Ltmp64:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str20, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_80
# BB#79:                                # %if.then173
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movb	%al, forceOverwrite(%rip)
	jmp	.LBB1_103
.LBB1_80:                               # %if.else174
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2048 0                # bzip2.c:2048:0
.Ltmp65:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str21, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_82
# BB#81:                                # %if.then179
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$3, %rax
	movl	%eax, opMode(%rip)
	jmp	.LBB1_103
.LBB1_82:                               # %if.else180
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2049 0                # bzip2.c:2049:0
.Ltmp66:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str22, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_84
# BB#83:                                # %if.then185
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movb	%al, keepInputFiles(%rip)
	jmp	.LBB1_103
.LBB1_84:                               # %if.else186
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2050 0                # bzip2.c:2050:0
.Ltmp67:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str23, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_86
# BB#85:                                # %if.then191
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movb	%al, smallMode(%rip)
	jmp	.LBB1_103
.LBB1_86:                               # %if.else192
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2051 0                # bzip2.c:2051:0
.Ltmp68:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str24, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_88
# BB#87:                                # %if.then197
                                        #   in Loop: Header=BB1_70 Depth=1
	xorq	%rax, %rax
	movb	%al, noisy(%rip)
	jmp	.LBB1_103
.LBB1_88:                               # %if.else198
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2052 0                # bzip2.c:2052:0
.Ltmp69:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str25, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_90
# BB#89:                                # %if.then203
                                        #   in Loop: Header=BB1_70 Depth=1
	callq	license
	jmp	.LBB1_103
.LBB1_90:                               # %if.else204
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2053 0                # bzip2.c:2053:0
.Ltmp70:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str26, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_92
# BB#91:                                # %if.then209
                                        #   in Loop: Header=BB1_70 Depth=1
	callq	license
	jmp	.LBB1_103
.LBB1_92:                               # %if.else210
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2054 0                # bzip2.c:2054:0
.Ltmp71:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str27, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_94
# BB#93:                                # %if.then215
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movl	%eax, workFactor(%rip)
	jmp	.LBB1_103
.LBB1_94:                               # %if.else216
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2055 0                # bzip2.c:2055:0
.Ltmp72:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str28, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB1_95
# BB#96:                                # %if.else223
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2056 0                # bzip2.c:2056:0
.Ltmp73:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str29, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_97
.Ltmp74:
.LBB1_95:                               # %if.then221
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2055 0                # bzip2.c:2055:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	callq	redundant
	jmp	.LBB1_103
.LBB1_97:                               # %if.else230
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2057 0                # bzip2.c:2057:0
.Ltmp75:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str30, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_99
# BB#98:                                # %if.then235
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$1, %rax
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_103
.LBB1_99:                               # %if.else236
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2058 0                # bzip2.c:2058:0
.Ltmp76:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str31, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_101
# BB#100:                               # %if.then241
                                        #   in Loop: Header=BB1_70 Depth=1
	movq	$9, %rax
	movl	%eax, blockSize100k(%rip)
	jmp	.LBB1_103
.LBB1_101:                              # %if.else242
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2059 0                # bzip2.c:2059:0
.Ltmp77:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str32, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_104
# BB#102:                               # %if.then247
                                        #   in Loop: Header=BB1_70 Depth=1
	movl	verbosity(%rip), %eax
	incq	%rax
	movl	%eax, verbosity(%rip)
	jmp	.LBB1_103
.LBB1_104:                              # %if.else249
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2060 0                # bzip2.c:2060:0
.Ltmp78:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movabsq	$.L.str33, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB1_165
# BB#105:                               # %if.else255
                                        #   in Loop: Header=BB1_70 Depth=1
	.loc	1 2062 0                # bzip2.c:2062:0
.Ltmp79:
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movq	$2, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	strncmp
	testq	%r13, %rax
	jne	.LBB1_103
# BB#106:                               # %if.then260
	.loc	1 2063 0                # bzip2.c:2063:0
.Ltmp80:
	movq	-88(%rbp), %rax
	movq	stderr(%rip), %rcx
	movq	progName(%rip), %rdx
	movq	(%rax), %rbx
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rcx
	callq	fprintf
	.loc	1 2064 0                # bzip2.c:2064:0
	movq	progName(%rip), %rax
	movq	%rax, %rdi
	jmp	.LBB1_43
.Ltmp81:
.LBB1_107:                              # %for.end283
	.loc	1 2069 0                # bzip2.c:2069:0
	movslq	verbosity(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB1_109
# BB#108:                               # %if.then286
	movq	$4, %rax
	movl	%eax, verbosity(%rip)
.Ltmp82:
.LBB1_109:                              # %if.end287
	.loc	1 2070 0                # bzip2.c:2070:0
	movl	opMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB1_113
# BB#110:                               # %land.lhs.true290
	movzbl	smallMode(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_113
# BB#111:                               # %land.lhs.true293
	movslq	blockSize100k(%rip), %rax
	cmpq	$3, %rax
	jl	.LBB1_113
# BB#112:                               # %if.then296
	movq	$2, %rax
	.loc	1 2071 0                # bzip2.c:2071:0
	movl	%eax, blockSize100k(%rip)
.Ltmp83:
.LBB1_113:                              # %if.end297
	.loc	1 2073 0                # bzip2.c:2073:0
	movl	opMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB1_115
# BB#114:                               # %land.lhs.true300
	movl	srcMode(%rip), %eax
	cmpq	$2, %rax
	je	.LBB1_166
.Ltmp84:
.LBB1_115:                              # %if.end305
	.loc	1 2079 0                # bzip2.c:2079:0
	movl	srcMode(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB1_118
# BB#116:                               # %land.lhs.true308
	movl	numFileNames(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_118
# BB#117:                               # %if.then311
	movq	$1, %rax
	.loc	1 2080 0                # bzip2.c:2080:0
	movl	%eax, srcMode(%rip)
.Ltmp85:
.LBB1_118:                              # %if.end312
	.loc	1 2082 0                # bzip2.c:2082:0
	movl	opMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB1_120
# BB#119:                               # %if.then315
	xorq	%rax, %rax
	movl	%eax, blockSize100k(%rip)
.Ltmp86:
.LBB1_120:                              # %if.end316
	.loc	1 2084 0                # bzip2.c:2084:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB1_122
# BB#121:                               # %if.then319
	.loc	1 2085 0                # bzip2.c:2085:0
.Ltmp87:
	movabsq	$mySignalCatcher, %rbx
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	$15, %rdi
	.loc	1 2086 0                # bzip2.c:2086:0
	movq	%rbx, %rsi
	callq	signal
	movq	$1, %rdi
	.loc	1 2088 0                # bzip2.c:2088:0
	movq	%rbx, %rsi
	callq	signal
.Ltmp88:
.LBB1_122:                              # %if.end323
	.loc	1 2092 0                # bzip2.c:2092:0
	movl	opMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB1_138
# BB#123:                               # %if.then326
	.loc	1 2093 0                # bzip2.c:2093:0
.Ltmp89:
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB1_130
# BB#124:                               # %if.then329
	xorq	%rdi, %rdi
	.loc	1 2094 0                # bzip2.c:2094:0
.Ltmp90:
	callq	compress
	jmp	.LBB1_125
.Ltmp91:
.LBB1_138:                              # %if.else357
	.loc	1 2107 0                # bzip2.c:2107:0
	movl	opMode(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB1_151
# BB#139:                               # %if.then360
	.loc	1 2109 0                # bzip2.c:2109:0
.Ltmp92:
	movl	srcMode(%rip), %eax
	xorq	%r14, %r14
.Ltmp93:
	.loc	1 2108 0                # bzip2.c:2108:0
	movb	%r14b, unzFailsExist(%rip)
	.loc	1 2109 0                # bzip2.c:2109:0
.Ltmp94:
	cmpq	$1, %rax
	jne	.LBB1_143
# BB#140:                               # %if.then363
	xorq	%rdi, %rdi
	.loc	1 2110 0                # bzip2.c:2110:0
.Ltmp95:
	callq	uncompress
	jmp	.LBB1_141
.Ltmp96:
.LBB1_130:                              # %if.else330
	movq	$1, %rax
	.loc	1 2097 0                # bzip2.c:2097:0
.Ltmp97:
	movq	-80(%rbp), %rcx
.Ltmp98:
	.loc	1 2096 0                # bzip2.c:2096:0
	movb	%al, -89(%rbp)
	.loc	1 2097 0                # bzip2.c:2097:0
.Ltmp99:
	movq	%rcx, -88(%rbp)
	.loc	1 2098 0                # bzip2.c:2098:0
.Ltmp100:
	movabsq	$.L.str9, %rbx
	xorq	%r14, %r14
	jmp	.LBB1_131
.Ltmp101:
	.align	16, 0x90
.LBB1_137:                              # %for.inc353
                                        #   in Loop: Header=BB1_131 Depth=1
	.loc	1 2097 0                # bzip2.c:2097:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_131:                              # %for.cond331
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, -88(%rbp)
	je	.LBB1_125
# BB#132:                               # %for.body334
                                        #   in Loop: Header=BB1_131 Depth=1
	.loc	1 2098 0                # bzip2.c:2098:0
.Ltmp102:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_134
# BB#133:                               # %if.then339
                                        #   in Loop: Header=BB1_131 Depth=1
	.loc	1 2098 0                # bzip2.c:2098:0
.Ltmp103:
	movb	%r14b, -89(%rbp)
	jmp	.LBB1_137
.Ltmp104:
	.align	16, 0x90
.LBB1_134:                              # %if.end340
                                        #   in Loop: Header=BB1_131 Depth=1
	.loc	1 2099 0                # bzip2.c:2099:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	jne	.LBB1_136
# BB#135:                               # %land.lhs.true346
                                        #   in Loop: Header=BB1_131 Depth=1
	movzbl	-89(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB1_137
.Ltmp105:
.LBB1_136:                              # %if.end350
                                        #   in Loop: Header=BB1_131 Depth=1
	.loc	1 2100 0                # bzip2.c:2100:0
	movl	numFilesProcessed(%rip), %eax
	.loc	1 2101 0                # bzip2.c:2101:0
	movq	-88(%rbp), %rcx
	.loc	1 2100 0                # bzip2.c:2100:0
	incq	%rax
	movl	%eax, numFilesProcessed(%rip)
	.loc	1 2101 0                # bzip2.c:2101:0
	movq	(%rcx), %rdi
	callq	compress
	jmp	.LBB1_137
.Ltmp106:
.LBB1_151:                              # %if.else394
	.loc	1 2128 0                # bzip2.c:2128:0
	movl	srcMode(%rip), %eax
	xorq	%r14, %r14
.Ltmp107:
	.loc	1 2127 0                # bzip2.c:2127:0
	movb	%r14b, testFailsExist(%rip)
	.loc	1 2128 0                # bzip2.c:2128:0
.Ltmp108:
	cmpq	$1, %rax
	jne	.LBB1_156
# BB#152:                               # %if.then397
	xorq	%rdi, %rdi
	.loc	1 2129 0                # bzip2.c:2129:0
.Ltmp109:
	callq	testf
	jmp	.LBB1_153
.Ltmp110:
.LBB1_143:                              # %if.else364
	.loc	1 2113 0                # bzip2.c:2113:0
	movq	-80(%rbp), %rax
	movq	$1, %rcx
.Ltmp111:
	.loc	1 2112 0                # bzip2.c:2112:0
	movb	%cl, -89(%rbp)
	.loc	1 2113 0                # bzip2.c:2113:0
.Ltmp112:
	movq	%rax, -88(%rbp)
	.loc	1 2114 0                # bzip2.c:2114:0
.Ltmp113:
	movabsq	$.L.str9, %rbx
	jmp	.LBB1_144
.Ltmp114:
	.align	16, 0x90
.LBB1_150:                              # %for.inc387
                                        #   in Loop: Header=BB1_144 Depth=1
	.loc	1 2113 0                # bzip2.c:2113:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_144:                              # %for.cond365
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, -88(%rbp)
	je	.LBB1_141
# BB#145:                               # %for.body368
                                        #   in Loop: Header=BB1_144 Depth=1
	.loc	1 2114 0                # bzip2.c:2114:0
.Ltmp115:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_147
# BB#146:                               # %if.then373
                                        #   in Loop: Header=BB1_144 Depth=1
	.loc	1 2114 0                # bzip2.c:2114:0
.Ltmp116:
	movb	%r14b, -89(%rbp)
	jmp	.LBB1_150
.Ltmp117:
	.align	16, 0x90
.LBB1_147:                              # %if.end374
                                        #   in Loop: Header=BB1_144 Depth=1
	.loc	1 2115 0                # bzip2.c:2115:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	jne	.LBB1_149
# BB#148:                               # %land.lhs.true380
                                        #   in Loop: Header=BB1_144 Depth=1
	movzbl	-89(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB1_150
.Ltmp118:
.LBB1_149:                              # %if.end384
                                        #   in Loop: Header=BB1_144 Depth=1
	.loc	1 2116 0                # bzip2.c:2116:0
	movl	numFilesProcessed(%rip), %eax
	.loc	1 2117 0                # bzip2.c:2117:0
	movq	-88(%rbp), %rcx
	.loc	1 2116 0                # bzip2.c:2116:0
	incq	%rax
	movl	%eax, numFilesProcessed(%rip)
	.loc	1 2117 0                # bzip2.c:2117:0
	movq	(%rcx), %rdi
	callq	uncompress
	jmp	.LBB1_150
.Ltmp119:
.LBB1_141:                              # %if.end390
	.loc	1 2120 0                # bzip2.c:2120:0
	movzbl	unzFailsExist(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_125
	jmp	.LBB1_142
.Ltmp120:
.LBB1_156:                              # %if.else398
	.loc	1 2132 0                # bzip2.c:2132:0
	movq	-80(%rbp), %rax
	movq	$1, %rcx
.Ltmp121:
	.loc	1 2131 0                # bzip2.c:2131:0
	movb	%cl, -89(%rbp)
	.loc	1 2132 0                # bzip2.c:2132:0
.Ltmp122:
	movq	%rax, -88(%rbp)
	.loc	1 2133 0                # bzip2.c:2133:0
.Ltmp123:
	movabsq	$.L.str9, %rbx
	jmp	.LBB1_157
.Ltmp124:
	.align	16, 0x90
.LBB1_163:                              # %for.inc421
                                        #   in Loop: Header=BB1_157 Depth=1
	.loc	1 2132 0                # bzip2.c:2132:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -88(%rbp)
.LBB1_157:                              # %for.cond399
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, -88(%rbp)
	je	.LBB1_153
# BB#158:                               # %for.body402
                                        #   in Loop: Header=BB1_157 Depth=1
	.loc	1 2133 0                # bzip2.c:2133:0
.Ltmp125:
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB1_160
# BB#159:                               # %if.then407
                                        #   in Loop: Header=BB1_157 Depth=1
	.loc	1 2133 0                # bzip2.c:2133:0
.Ltmp126:
	movb	%r14b, -89(%rbp)
	jmp	.LBB1_163
.Ltmp127:
	.align	16, 0x90
.LBB1_160:                              # %if.end408
                                        #   in Loop: Header=BB1_157 Depth=1
	.loc	1 2134 0                # bzip2.c:2134:0
	movq	-88(%rbp), %rax
	movq	(%rax), %rax
	movsbq	(%rax), %rax
	andq	%r13, %rax
	cmpq	$45, %rax
	jne	.LBB1_162
# BB#161:                               # %land.lhs.true414
                                        #   in Loop: Header=BB1_157 Depth=1
	movzbl	-89(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB1_163
.Ltmp128:
.LBB1_162:                              # %if.end418
                                        #   in Loop: Header=BB1_157 Depth=1
	.loc	1 2135 0                # bzip2.c:2135:0
	movl	numFilesProcessed(%rip), %eax
	.loc	1 2136 0                # bzip2.c:2136:0
	movq	-88(%rbp), %rcx
	.loc	1 2135 0                # bzip2.c:2135:0
	incq	%rax
	movl	%eax, numFilesProcessed(%rip)
	.loc	1 2136 0                # bzip2.c:2136:0
	movq	(%rcx), %rdi
	callq	testf
	jmp	.LBB1_163
.Ltmp129:
.LBB1_153:                              # %if.end424
	.loc	1 2139 0                # bzip2.c:2139:0
	movzbl	testFailsExist(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_125
# BB#154:                               # %land.lhs.true427
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_155
.Ltmp130:
.LBB1_125:                              # %if.end434
	.loc	1 2153 0                # bzip2.c:2153:0
	movq	-80(%rbp), %rax
	jmp	.LBB1_126
	.align	16, 0x90
.LBB1_129:                              # %if.end444
                                        #   in Loop: Header=BB1_126 Depth=1
.Ltmp131:
	#DEBUG_VALUE: aa2 <- [RBP+-104]
	.loc	1 2157 0                # bzip2.c:2157:0
	movq	-88(%rbp), %rdi
	callq	free
	.loc	1 2158 0                # bzip2.c:2158:0
	movq	-104(%rbp), %rax
.Ltmp132:
.LBB1_126:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 2153 0                # bzip2.c:2153:0
	movq	%rax, -88(%rbp)
	.loc	1 2154 0                # bzip2.c:2154:0
	cmpq	$0, -88(%rbp)
	je	.LBB1_164
# BB#127:                               # %while.body
                                        #   in Loop: Header=BB1_126 Depth=1
.Ltmp133:
	#DEBUG_VALUE: aa2 <- [RBP+-104]
	.loc	1 2155 0                # bzip2.c:2155:0
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	.loc	1 2156 0                # bzip2.c:2156:0
.Ltmp134:
	movq	-88(%rbp), %rcx
.Ltmp135:
	.loc	1 2155 0                # bzip2.c:2155:0
	movq	%rax, -104(%rbp)
	.loc	1 2156 0                # bzip2.c:2156:0
.Ltmp136:
	cmpq	$0, (%rcx)
	je	.LBB1_129
# BB#128:                               # %if.then442
                                        #   in Loop: Header=BB1_126 Depth=1
.Ltmp137:
	#DEBUG_VALUE: aa2 <- [RBP+-104]
	movq	-88(%rbp), %rax
	movq	(%rax), %rdi
	callq	free
	jmp	.LBB1_129
.Ltmp138:
.LBB1_164:                              # %while.end
	.loc	1 2161 0                # bzip2.c:2161:0
	movl	exitValue(%rip), %eax
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB1_165:                              # %sw.bb131
	.loc	1 2028 0                # bzip2.c:2028:0
.Ltmp139:
	movq	progName(%rip), %rax
	movq	%rax, %rdi
	callq	usage
	xorq	%rdi, %rdi
	.loc	1 2029 0                # bzip2.c:2029:0
	callq	exit
.Ltmp140:
.LBB1_166:                              # %if.then303
	.loc	1 2074 0                # bzip2.c:2074:0
	movq	stderr(%rip), %rcx
	movq	progName(%rip), %rdx
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	movq	$1, %rdi
	.loc	1 2076 0                # bzip2.c:2076:0
	callq	exit
.Ltmp141:
.LBB1_155:                              # %if.then430
	.loc	1 2140 0                # bzip2.c:2140:0
	movq	stderr(%rip), %rcx
	movabsq	$.L.str35, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	fprintf
.Ltmp142:
.LBB1_142:                              # %if.then392
	movq	$2, %rdi
	.loc	1 2121 0                # bzip2.c:2121:0
.Ltmp143:
	callq	setExit
	.loc	1 2122 0                # bzip2.c:2122:0
	movl	exitValue(%rip), %eax
	movq	%rax, %rdi
	callq	exit
.Ltmp144:
.Ltmp145:
	.size	main, .Ltmp145-main
.Lfunc_end1:
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_66
	.quad	.LBB1_49
	.quad	.LBB1_42
	.quad	.LBB1_51
	.quad	.LBB1_42
	.quad	.LBB1_165
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_53
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_55
	.quad	.LBB1_42
	.quad	.LBB1_54
	.quad	.LBB1_52
	.quad	.LBB1_42
	.quad	.LBB1_65
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_42
	.quad	.LBB1_50
.LJTI1_1:
	.quad	.LBB1_56
	.quad	.LBB1_57
	.quad	.LBB1_58
	.quad	.LBB1_59
	.quad	.LBB1_60
	.quad	.LBB1_61
	.quad	.LBB1_62
	.quad	.LBB1_63
	.quad	.LBB1_64

	.text
	.align	16, 0x90
	.type	mySIGSEGVorSIGBUScatcher,@function
mySIGSEGVorSIGBUScatcher:               # @mySIGSEGVorSIGBUScatcher
.Lfunc_begin2:
	.loc	1 955 0                 # bzip2.c:955:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp148:
	.cfi_def_cfa_offset 16
.Ltmp149:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp150:
	.cfi_def_cfa_register %rbp
	.loc	1 956 0 prologue_end    # bzip2.c:956:0
.Ltmp151:
	subq	$16, %rsp
	movl	opMode(%rip), %eax
	movl	%edi, -4(%rbp)
	cmpq	$1, %rax
	jne	.LBB2_2
# BB#1:                                 # %if.then
	.loc	1 957 0                 # bzip2.c:957:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str103, %rsi
	jmp	.LBB2_3
.LBB2_2:                                # %if.else
	.loc	1 980 0                 # bzip2.c:980:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str104, %rsi
.LBB2_3:                                # %if.end
	xorq	%rax, %rax
	callq	fprintf
.Ltmp152:
	.loc	1 1005 0                # bzip2.c:1005:0
	callq	showFileNames
	.loc	1 1006 0                # bzip2.c:1006:0
.Ltmp153:
	movl	opMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB2_5
# BB#4:                                 # %if.then3
	movq	$3, %rdi
	.loc	1 1007 0                # bzip2.c:1007:0
	callq	cleanUpAndFail
.LBB2_5:                                # %if.else4
	.loc	1 1008 0                # bzip2.c:1008:0
.Ltmp154:
	callq	cadvise
	movq	$2, %rdi
	callq	cleanUpAndFail
.Ltmp155:
.Ltmp156:
	.size	mySIGSEGVorSIGBUScatcher, .Ltmp156-mySIGSEGVorSIGBUScatcher
.Lfunc_end2:
	.cfi_endproc

	.align	16, 0x90
	.type	copyFileName,@function
copyFileName:                           # @copyFileName
.Lfunc_begin3:
	.loc	1 1063 0                # bzip2.c:1063:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp159:
	.cfi_def_cfa_offset 16
.Ltmp160:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp161:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	.loc	1 1064 0 prologue_end   # bzip2.c:1064:0
.Ltmp162:
	movq	%rsi, %rdi
	callq	strlen
	cmpq	$1025, %rax             # imm = 0x401
	jae	.LBB3_2
.Ltmp163:
# BB#1:                                 # %if.end
	.loc	1 1076 0                # bzip2.c:1076:0
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	$1024, %rdx             # imm = 0x400
	callq	strncpy
	.loc	1 1077 0                # bzip2.c:1077:0
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movb	%cl, 1024(%rax)
	.loc	1 1078 0                # bzip2.c:1078:0
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB3_2:                                # %if.then
	.loc	1 1065 0                # bzip2.c:1065:0
.Ltmp164:
	movq	stderr(%rip), %rdx
	movq	-16(%rbp), %r8
	movabsq	$.L.str102, %rsi
	movq	$1024, %rcx             # imm = 0x400
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%r8, %rdx
	callq	fprintf
	movq	$1, %rdi
	.loc	1 1072 0                # bzip2.c:1072:0
	callq	setExit
	.loc	1 1073 0                # bzip2.c:1073:0
	movl	exitValue(%rip), %eax
	movq	%rax, %rdi
	callq	exit
.Ltmp165:
.Ltmp166:
	.size	copyFileName, .Ltmp166-copyFileName
.Lfunc_end3:
	.cfi_endproc

	.align	16, 0x90
	.type	addFlagsFromEnvVar,@function
addFlagsFromEnvVar:                     # @addFlagsFromEnvVar
.Lfunc_begin4:
	.loc	1 1881 0                # bzip2.c:1881:0
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
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp173:
	.cfi_offset %rbx, -56
.Ltmp174:
	.cfi_offset %r12, -48
.Ltmp175:
	.cfi_offset %r13, -40
.Ltmp176:
	.cfi_offset %r14, -32
.Ltmp177:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	.loc	1 1885 0 prologue_end   # bzip2.c:1885:0
.Ltmp178:
	movq	%rsi, %rdi
	callq	getenv
	testq	%rax, %rax
	movq	%rax, -80(%rbp)
	je	.LBB4_18
# BB#1:                                 # %if.then
	.loc	1 1887 0                # bzip2.c:1887:0
.Ltmp179:
	movq	-80(%rbp), %rax
	movq	%rax, -88(%rbp)
	xorq	%r15, %r15
	.loc	1 1888 0                # bzip2.c:1888:0
	movl	%r15d, -60(%rbp)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.loc	1 1894 0                # bzip2.c:1894:0
.Ltmp180:
	movabsq	$0, %r12
	movabsq	$1, %r13
	.loc	1 1898 0                # bzip2.c:1898:0
.Ltmp181:
	movabsq	$tmpName, %r14
	jmp	.LBB4_2
	.align	16, 0x90
.LBB4_17:                               # %for.end
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	-68(%rbp), %rax
	.loc	1 1899 0                # bzip2.c:1899:0
	movq	-48(%rbp), %rcx
	.loc	1 1898 0                # bzip2.c:1898:0
	movb	%r15b, tmpName(%rax)
	.loc	1 1899 0                # bzip2.c:1899:0
	movq	(%rcx), %rdi
	movq	%r14, %rsi
	callq	snocString
	movq	-48(%rbp), %rcx
	movq	%rax, (%rcx)
.Ltmp182:
.LBB4_2:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_4 Depth 2
                                        #     Child Loop BB4_6 Depth 2
                                        #     Child Loop BB4_15 Depth 2
	.loc	1 1890 0                # bzip2.c:1890:0
	movslq	-60(%rbp), %rax
	movq	-88(%rbp), %rcx
	movsbq	(%rcx,%rax), %rax
	testq	%rbx, %rax
	je	.LBB4_18
.Ltmp183:
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB4_2 Depth=1
	.loc	1 1891 0                # bzip2.c:1891:0
	movslq	-60(%rbp), %rax
	addq	%rax, -88(%rbp)
	.loc	1 1892 0                # bzip2.c:1892:0
	movl	%r15d, -60(%rbp)
	jmp	.LBB4_4
	.align	16, 0x90
.LBB4_5:                                # %while.body8
                                        #   in Loop: Header=BB4_4 Depth=2
	.loc	1 1893 0                # bzip2.c:1893:0
	incq	-88(%rbp)
.LBB4_4:                                # %while.cond4
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-88(%rbp), %rax
	movsbq	(%rax), %rdi
	callq	isspace
	testq	%rbx, %rax
	jne	.LBB4_5
	jmp	.LBB4_6
	.align	16, 0x90
.LBB4_10:                               # %while.body20
                                        #   in Loop: Header=BB4_6 Depth=2
	.loc	1 1894 0                # bzip2.c:1894:0
	movl	-60(%rbp), %eax
	incq	%rax
	movl	%eax, -60(%rbp)
.LBB4_6:                                # %while.cond9
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-60(%rbp), %rax
	movq	-88(%rbp), %rcx
	movsbq	(%rcx,%rax), %rcx
	xorq	%rax, %rax
	testq	%rbx, %rcx
	je	.LBB4_9
# BB#7:                                 # %land.rhs
                                        #   in Loop: Header=BB4_6 Depth=2
	movslq	-60(%rbp), %rax
	movq	-88(%rbp), %rcx
	movsbq	(%rcx,%rax), %rdi
	callq	isspace
	testq	%rbx, %rax
	movq	%r12, %rax
	jne	.LBB4_9
# BB#8:                                 # %land.rhs
                                        #   in Loop: Header=BB4_6 Depth=2
	movq	%r13, %rax
.LBB4_9:                                # %land.end
                                        #   in Loop: Header=BB4_6 Depth=2
	testq	%rax, %rax
	jne	.LBB4_10
# BB#11:                                # %while.end21
                                        #   in Loop: Header=BB4_2 Depth=1
	.loc	1 1895 0                # bzip2.c:1895:0
.Ltmp184:
	movslq	-60(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB4_2
# BB#12:                                # %if.then24
                                        #   in Loop: Header=BB4_2 Depth=1
	.loc	1 1896 0                # bzip2.c:1896:0
.Ltmp185:
	movslq	-60(%rbp), %rax
	cmpq	$1025, %rax             # imm = 0x401
	movl	%eax, -68(%rbp)
	jl	.LBB4_14
# BB#13:                                # %if.then27
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$1024, %rax             # imm = 0x400
	.loc	1 1896 0                # bzip2.c:1896:0
.Ltmp186:
	movl	%eax, -68(%rbp)
.Ltmp187:
.LBB4_14:                               # %if.end28
                                        #   in Loop: Header=BB4_2 Depth=1
	.loc	1 1897 0                # bzip2.c:1897:0
	movl	%r15d, -64(%rbp)
	jmp	.LBB4_15
	.align	16, 0x90
.LBB4_16:                               # %for.inc
                                        #   in Loop: Header=BB4_15 Depth=2
	movslq	-64(%rbp), %rax
	movq	-88(%rbp), %rcx
	movl	-64(%rbp), %edx
	movzbl	(%rcx,%rax), %ecx
	movb	%cl, tmpName(%rax)
	incq	%rdx
	movl	%edx, -64(%rbp)
.LBB4_15:                               # %for.cond
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-64(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB4_16
	jmp	.LBB4_17
.Ltmp188:
.LBB4_18:                               # %if.end41
	.loc	1 1903 0                # bzip2.c:1903:0
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp189:
.Ltmp190:
	.size	addFlagsFromEnvVar, .Ltmp190-addFlagsFromEnvVar
.Lfunc_end4:
	.cfi_endproc

	.align	16, 0x90
	.type	snocString,@function
snocString:                             # @snocString
.Lfunc_begin5:
	.loc	1 1863 0                # bzip2.c:1863:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp193:
	.cfi_def_cfa_offset 16
.Ltmp194:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp195:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 1864 0 prologue_end   # bzip2.c:1864:0
.Ltmp196:
	cmpq	$0, -16(%rbp)
	movq	%rsi, -24(%rbp)
	je	.LBB5_1
# BB#2:                                 # %if.else
.Ltmp197:
	#DEBUG_VALUE: tmp <- [RBP+-40]
	.loc	1 1870 0                # bzip2.c:1870:0
	movq	-16(%rbp), %rax
	jmp	.LBB5_3
	.align	16, 0x90
.LBB5_4:                                # %while.body
                                        #   in Loop: Header=BB5_3 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-40]
	.loc	1 1871 0                # bzip2.c:1871:0
	movq	-40(%rbp), %rax
	movq	8(%rax), %rax
.Ltmp198:
.LBB5_3:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1870 0                # bzip2.c:1870:0
	movq	%rax, -40(%rbp)
.Ltmp199:
	#DEBUG_VALUE: tmp <- [RBP+-40]
	.loc	1 1871 0                # bzip2.c:1871:0
	movq	-40(%rbp), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB5_4
# BB#5:                                 # %while.end
.Ltmp200:
	#DEBUG_VALUE: tmp <- [RBP+-40]
	.loc	1 1872 0                # bzip2.c:1872:0
	movq	-40(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	snocString
	movq	-40(%rbp), %rcx
	movq	%rax, 8(%rcx)
	.loc	1 1873 0                # bzip2.c:1873:0
	movq	-16(%rbp), %rax
	jmp	.LBB5_6
.Ltmp201:
.LBB5_1:                                # %if.then
	#DEBUG_VALUE: tmp <- [RBP+-32]
	.loc	1 1865 0                # bzip2.c:1865:0
	callq	mkCell
	.loc	1 1866 0                # bzip2.c:1866:0
	movq	-24(%rbp), %rdi
	.loc	1 1865 0                # bzip2.c:1865:0
	movq	%rax, -32(%rbp)
	.loc	1 1866 0                # bzip2.c:1866:0
	callq	strlen
	leaq	5(%rax), %rdi
	callq	myMalloc
	movq	-32(%rbp), %rcx
	movq	%rax, (%rcx)
	.loc	1 1867 0                # bzip2.c:1867:0
	movq	-32(%rbp), %rax
	movq	(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	strcpy
	.loc	1 1868 0                # bzip2.c:1868:0
	movq	-32(%rbp), %rax
.Ltmp202:
.LBB5_6:                                # %return
	.loc	1 1873 0                # bzip2.c:1873:0
	movq	%rax, -8(%rbp)
.Ltmp203:
	.loc	1 1875 0                # bzip2.c:1875:0
	movq	-8(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	retq
.Ltmp204:
.Ltmp205:
	.size	snocString, .Ltmp205-snocString
.Lfunc_end5:
	.cfi_endproc

	.align	16, 0x90
	.type	license,@function
license:                                # @license
.Lfunc_begin6:
	.loc	1 1735 0                # bzip2.c:1735:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp209:
	.cfi_def_cfa_offset 16
.Ltmp210:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp211:
	.cfi_def_cfa_register %rbp
	.loc	1 1736 0 prologue_end   # bzip2.c:1736:0
.Ltmp212:
	pushq	%rbx
	pushq	%rax
.Ltmp213:
	.cfi_offset %rbx, -24
	movq	stderr(%rip), %rbx
	.loc	1 1752 0                # bzip2.c:1752:0
	callq	BZ2_bzlibVersion
	movq	%rax, %rcx
	.loc	1 1736 0                # bzip2.c:1736:0
	movabsq	$.L.str101, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	.loc	1 1754 0                # bzip2.c:1754:0
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp214:
.Ltmp215:
	.size	license, .Ltmp215-license
.Lfunc_end6:
	.cfi_endproc

	.align	16, 0x90
	.type	usage,@function
usage:                                  # @usage
.Lfunc_begin7:
	.loc	1 1760 0                # bzip2.c:1760:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp219:
	.cfi_def_cfa_offset 16
.Ltmp220:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp221:
	.cfi_def_cfa_register %rbp
	.loc	1 1761 0 prologue_end   # bzip2.c:1761:0
.Ltmp222:
	pushq	%rbx
	pushq	%rax
.Ltmp223:
	.cfi_offset %rbx, -24
	movq	stderr(%rip), %rbx
	movq	%rdi, -16(%rbp)
	.loc	1 1795 0                # bzip2.c:1795:0
	callq	BZ2_bzlibVersion
	.loc	1 1761 0                # bzip2.c:1761:0
	movq	-16(%rbp), %rcx
	.loc	1 1795 0                # bzip2.c:1795:0
	movq	%rax, %rdx
	.loc	1 1761 0                # bzip2.c:1761:0
	movabsq	$.L.str100, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 1798 0                # bzip2.c:1798:0
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp224:
.Ltmp225:
	.size	usage, .Ltmp225-usage
.Lfunc_end7:
	.cfi_endproc

	.align	16, 0x90
	.type	redundant,@function
redundant:                              # @redundant
.Lfunc_begin8:
	.loc	1 1804 0                # bzip2.c:1804:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp228:
	.cfi_def_cfa_offset 16
.Ltmp229:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp230:
	.cfi_def_cfa_register %rbp
	.loc	1 1805 0 prologue_end   # bzip2.c:1805:0
.Ltmp231:
	subq	$16, %rsp
	movq	stderr(%rip), %r8
	movq	progName(%rip), %rdx
	movq	%rdi, %rcx
	movq	%rcx, -8(%rbp)
	movabsq	$.L.str99, %rsi
	xorq	%rax, %rax
	movq	%r8, %rdi
	callq	fprintf
	.loc	1 1809 0                # bzip2.c:1809:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp232:
.Ltmp233:
	.size	redundant, .Ltmp233-redundant
.Lfunc_end8:
	.cfi_endproc

	.align	16, 0x90
	.type	mySignalCatcher,@function
mySignalCatcher:                        # @mySignalCatcher
.Lfunc_begin9:
	.loc	1 944 0                 # bzip2.c:944:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp236:
	.cfi_def_cfa_offset 16
.Ltmp237:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp238:
	.cfi_def_cfa_register %rbp
	.loc	1 945 0 prologue_end    # bzip2.c:945:0
.Ltmp239:
	subq	$16, %rsp
	movq	stderr(%rip), %rcx
	movq	progName(%rip), %rdx
	movl	%edi, -4(%rbp)
	movabsq	$.L.str98, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	movq	$1, %rdi
	.loc	1 948 0                 # bzip2.c:948:0
	callq	cleanUpAndFail
.Ltmp240:
.Ltmp241:
	.size	mySignalCatcher, .Ltmp241-mySignalCatcher
.Lfunc_end9:
	.cfi_endproc

	.align	16, 0x90
	.type	compress,@function
compress:                               # @compress
.Lfunc_begin10:
	.loc	1 1267 0                # bzip2.c:1267:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp245:
	.cfi_def_cfa_offset 16
.Ltmp246:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp247:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$184, %rsp
.Ltmp248:
	.cfi_offset %rbx, -40
.Ltmp249:
	.cfi_offset %r14, -32
.Ltmp250:
	.cfi_offset %r15, -24
	movq	%rdi, -32(%rbp)
	xorq	%rax, %rax
	.loc	1 1275 0 prologue_end   # bzip2.c:1275:0
.Ltmp251:
	cmpq	$0, -32(%rbp)
.Ltmp252:
	.loc	1 1273 0                # bzip2.c:1273:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	jne	.LBB10_2
# BB#1:                                 # %land.lhs.true
	.loc	1 1275 0                # bzip2.c:1275:0
.Ltmp253:
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB10_72
.Ltmp254:
.LBB10_2:                               # %if.end
	.loc	1 1278 0                # bzip2.c:1278:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB10_6
# BB#3:                                 # %if.end
	cmpq	$2, %rax
	jne	.LBB10_4
# BB#7:                                 # %sw.bb3
	.loc	1 1289 0                # bzip2.c:1289:0
.Ltmp255:
	movq	-32(%rbp), %rsi
	movabsq	$inName, %rdi
	jmp	.LBB10_8
.LBB10_6:                               # %sw.bb2
	.loc	1 1284 0                # bzip2.c:1284:0
	movq	-32(%rbp), %rsi
	movabsq	$inName, %rdi
	callq	copyFileName
	.loc	1 1285 0                # bzip2.c:1285:0
	movq	-32(%rbp), %rsi
	movabsq	$outName, %rbx
	movq	%rbx, %rdi
	callq	copyFileName
	.loc	1 1286 0                # bzip2.c:1286:0
	movabsq	$.L.str, %rsi
	movq	%rbx, %rdi
	callq	strcat
	jmp	.LBB10_9
.LBB10_4:                               # %if.end
	cmpq	$1, %rax
	jne	.LBB10_9
# BB#5:                                 # %sw.bb
	.loc	1 1280 0                # bzip2.c:1280:0
	movabsq	$inName, %rdi
	movabsq	$.L.str37, %rsi
.LBB10_8:                               # %sw.epilog
	.loc	1 1289 0                # bzip2.c:1289:0
	callq	copyFileName
	.loc	1 1290 0                # bzip2.c:1290:0
	movabsq	$outName, %rdi
	movabsq	$.L.str70, %rsi
	callq	copyFileName
.Ltmp256:
.LBB10_9:                               # %sw.epilog
	.loc	1 1294 0                # bzip2.c:1294:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB10_17
# BB#10:                                # %land.lhs.true5
	movabsq	$inName, %rdi
	callq	containsDubiousChars
	testq	%rax, %rax
	je	.LBB10_17
# BB#11:                                # %if.then7
	.loc	1 1295 0                # bzip2.c:1295:0
.Ltmp257:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_16
# BB#12:                                # %if.then9
	.loc	1 1296 0                # bzip2.c:1296:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str38, %rsi
	jmp	.LBB10_13
.Ltmp258:
.LBB10_17:                              # %if.end12
	.loc	1 1301 0                # bzip2.c:1301:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB10_20
# BB#18:                                # %land.lhs.true15
	movabsq	$inName, %rdi
	callq	fileExists
	testq	%rax, %rax
	je	.LBB10_19
.Ltmp259:
.LBB10_20:                              # %if.end22
	xorq	%rax, %rax
	.loc	1 1307 0                # bzip2.c:1307:0
.Ltmp260:
	movl	%eax, -56(%rbp)
	.loc	1 1308 0                # bzip2.c:1308:0
.Ltmp261:
	movabsq	$inName, %rbx
	jmp	.LBB10_21
.Ltmp262:
	.align	16, 0x90
.LBB10_25:                              # %for.inc
                                        #   in Loop: Header=BB10_21 Depth=1
	.loc	1 1307 0                # bzip2.c:1307:0
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB10_21:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-56(%rbp), %rax
	cmpq	$3, %rax
	jg	.LBB10_26
# BB#22:                                # %for.body
                                        #   in Loop: Header=BB10_21 Depth=1
	.loc	1 1308 0                # bzip2.c:1308:0
.Ltmp263:
	movslq	-56(%rbp), %rax
	movq	zSuffix(,%rax,8), %rsi
	movq	%rbx, %rdi
	callq	hasSuffix
	testq	%rax, %rax
	je	.LBB10_25
# BB#23:                                # %if.then27
	.loc	1 1309 0                # bzip2.c:1309:0
.Ltmp264:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_16
# BB#24:                                # %if.then29
	.loc	1 1310 0                # bzip2.c:1310:0
	movslq	-56(%rbp), %rax
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movq	zSuffix(,%rax,8), %r8
	movabsq	$.L.str91, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB10_16
.Ltmp265:
.LBB10_26:                              # %for.end
	.loc	1 1317 0                # bzip2.c:1317:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB10_28
# BB#27:                                # %lor.lhs.false
	movl	srcMode(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB10_30
.LBB10_28:                              # %if.then39
	.loc	1 1318 0                # bzip2.c:1318:0
.Ltmp266:
	movabsq	$inName, %rdi
	leaq	-200(%rbp), %rsi
	callq	stat
	.loc	1 1319 0                # bzip2.c:1319:0
.Ltmp267:
	movl	-176(%rbp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB10_30
# BB#29:                                # %if.then43
	.loc	1 1320 0                # bzip2.c:1320:0
.Ltmp268:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str40, %rsi
.Ltmp269:
.LBB10_13:                              # %if.end11
	.loc	1 1296 0                # bzip2.c:1296:0
	movabsq	$inName, %rcx
.LBB10_14:                              # %if.end11
	xorq	%rax, %rax
.LBB10_15:                              # %if.end11
	callq	fprintf
	jmp	.LBB10_16
.Ltmp270:
.LBB10_30:                              # %if.end46
	.loc	1 1327 0                # bzip2.c:1327:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB10_35
# BB#31:                                # %land.lhs.true49
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	jne	.LBB10_35
# BB#32:                                # %land.lhs.true51
	movabsq	$inName, %rdi
	callq	notAStandardFile
	testq	%rax, %rax
	je	.LBB10_35
# BB#33:                                # %if.then55
	.loc	1 1328 0                # bzip2.c:1328:0
.Ltmp271:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_16
# BB#34:                                # %if.then57
	.loc	1 1329 0                # bzip2.c:1329:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str73, %rsi
	jmp	.LBB10_13
.Ltmp272:
.LBB10_35:                              # %if.end60
	.loc	1 1334 0                # bzip2.c:1334:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB10_39
# BB#36:                                # %land.lhs.true63
	movabsq	$outName, %rdi
	callq	fileExists
	testq	%rax, %rax
	je	.LBB10_39
# BB#37:                                # %if.then67
	.loc	1 1335 0                # bzip2.c:1335:0
.Ltmp273:
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_73
# BB#38:                                # %if.then69
	.loc	1 1336 0                # bzip2.c:1336:0
.Ltmp274:
	movabsq	$outName, %rdi
	callq	remove
.Ltmp275:
.LBB10_39:                              # %if.end73
	.loc	1 1344 0                # bzip2.c:1344:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB10_46
# BB#40:                                # %land.lhs.true76
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	jne	.LBB10_46
# BB#41:                                # %land.lhs.true78
	.loc	1 1345 0                # bzip2.c:1345:0
	movabsq	$inName, %rdi
	callq	countHardLinks
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -52(%rbp)
	jle	.LBB10_46
# BB#42:                                # %if.then82
	.loc	1 1346 0                # bzip2.c:1346:0
.Ltmp276:
	movq	stderr(%rip), %rdi
	movslq	-52(%rbp), %r8
	movq	progName(%rip), %rdx
	cmpq	$1, %r8
	jle	.LBB10_44
.Ltmp277:
# BB#43:
	movabsq	$.L.str77, %r9
	jmp	.LBB10_45
.LBB10_46:                              # %if.end86
	.loc	1 1352 0                # bzip2.c:1352:0
.Ltmp278:
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB10_48
# BB#47:                                # %if.then89
	.loc	1 1355 0                # bzip2.c:1355:0
.Ltmp279:
	movabsq	$inName, %rdi
	callq	saveInputFileMetaInfo
.Ltmp280:
.LBB10_48:                              # %if.end90
	.loc	1 1358 0                # bzip2.c:1358:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB10_64
# BB#49:                                # %if.end90
	cmpq	$2, %rax
	jne	.LBB10_50
# BB#53:                                # %sw.bb99
	.loc	1 1375 0                # bzip2.c:1375:0
.Ltmp281:
	movabsq	$inName, %rdi
	movabsq	$.L.str43, %rsi
	callq	fopen
	.loc	1 1376 0                # bzip2.c:1376:0
	movq	stdout(%rip), %rdi
	.loc	1 1375 0                # bzip2.c:1375:0
	movq	%rax, -40(%rbp)
	.loc	1 1376 0                # bzip2.c:1376:0
	movq	%rdi, -48(%rbp)
	.loc	1 1377 0                # bzip2.c:1377:0
.Ltmp282:
	callq	fileno
	movq	%rax, %rdi
	callq	isatty
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB10_57
# BB#54:                                # %if.then104
	.loc	1 1378 0                # bzip2.c:1378:0
.Ltmp283:
	movq	stderr(%rip), %rbx
	movq	progName(%rip), %rdx
	movabsq	$.L.str92, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 1381 0                # bzip2.c:1381:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rcx
	callq	fprintf
	jmp	.LBB10_55
.Ltmp284:
.LBB10_64:                              # %sw.bb120
	.loc	1 1396 0                # bzip2.c:1396:0
	movabsq	$inName, %rdi
	movabsq	$.L.str43, %rsi
	callq	fopen
	movq	%rax, -40(%rbp)
	.loc	1 1397 0                # bzip2.c:1397:0
	movabsq	$outName, %rdi
	movabsq	$.L.str78, %rsi
	callq	fopen_output_safely
	testq	%rax, %rax
	movq	%rax, -48(%rbp)
	je	.LBB10_65
# BB#66:                                # %if.end134
	.loc	1 1405 0                # bzip2.c:1405:0
.Ltmp285:
	cmpq	$0, -40(%rbp)
	jne	.LBB10_58
# BB#67:                                # %if.then137
	.loc	1 1406 0                # bzip2.c:1406:0
.Ltmp286:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1407 0                # bzip2.c:1407:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1406 0                # bzip2.c:1406:0
	movabsq	$.L.str72, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
	.loc	1 1408 0                # bzip2.c:1408:0
.Ltmp287:
	cmpq	$0, -48(%rbp)
	je	.LBB10_16
# BB#68:                                # %if.then143
	movq	-48(%rbp), %rdi
	callq	fclose
	jmp	.LBB10_16
.Ltmp288:
.LBB10_50:                              # %if.end90
	cmpq	$1, %rax
	jne	.LBB10_69
# BB#51:                                # %sw.bb91
	.loc	1 1361 0                # bzip2.c:1361:0
	movq	stdin(%rip), %rax
	.loc	1 1362 0                # bzip2.c:1362:0
	movq	stdout(%rip), %rdi
	.loc	1 1361 0                # bzip2.c:1361:0
	movq	%rax, -40(%rbp)
	.loc	1 1362 0                # bzip2.c:1362:0
	movq	%rdi, -48(%rbp)
	.loc	1 1363 0                # bzip2.c:1363:0
.Ltmp289:
	callq	fileno
	movq	%rax, %rdi
	callq	isatty
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB10_58
# BB#52:                                # %if.then95
	.loc	1 1364 0                # bzip2.c:1364:0
.Ltmp290:
	movq	stderr(%rip), %rbx
	movq	progName(%rip), %rdx
	movabsq	$.L.str92, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 1367 0                # bzip2.c:1367:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rcx
	jmp	.LBB10_15
.Ltmp291:
.LBB10_73:                              # %if.else
	.loc	1 1338 0                # bzip2.c:1338:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str75, %rsi
	movabsq	$outName, %rcx
	jmp	.LBB10_14
.Ltmp292:
.LBB10_57:                              # %if.end112
	.loc	1 1387 0                # bzip2.c:1387:0
	cmpq	$0, -40(%rbp)
	je	.LBB10_19
.Ltmp293:
.LBB10_58:                              # %sw.epilog147
	.loc	1 1419 0                # bzip2.c:1419:0
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB10_60
# BB#59:                                # %if.then150
	.loc	1 1420 0                # bzip2.c:1420:0
.Ltmp294:
	movq	stderr(%rip), %r14
	movabsq	$.L.str46, %rsi
	movabsq	$inName, %rbx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	.loc	1 1421 0                # bzip2.c:1421:0
	movq	%rbx, %rdi
	callq	pad
	.loc	1 1422 0                # bzip2.c:1422:0
	movq	%r14, %rdi
	callq	fflush
.Ltmp295:
.LBB10_60:                              # %if.end153
	.loc	1 1426 0                # bzip2.c:1426:0
	movq	-48(%rbp), %rax
	.loc	1 1428 0                # bzip2.c:1428:0
	movq	-40(%rbp), %rdi
	movq	-48(%rbp), %rsi
	.loc	1 1426 0                # bzip2.c:1426:0
	movq	%rax, outputHandleJustInCase(%rip)
	movq	$1, %rax
	.loc	1 1427 0                # bzip2.c:1427:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	.loc	1 1428 0                # bzip2.c:1428:0
	callq	compressStream
	.loc	1 1432 0                # bzip2.c:1432:0
.Ltmp296:
	movl	srcMode(%rip), %eax
.Ltmp297:
	.loc	1 1429 0                # bzip2.c:1429:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 1432 0                # bzip2.c:1432:0
.Ltmp298:
	cmpq	$3, %rax
	jne	.LBB10_70
# BB#61:                                # %if.then156
	.loc	1 1433 0                # bzip2.c:1433:0
.Ltmp299:
	movabsq	$outName, %rdi
	callq	applySavedMetaInfoToOutputFile
	.loc	1 1435 0                # bzip2.c:1435:0
.Ltmp300:
	movzbl	keepInputFiles(%rip), %eax
	xorq	%rcx, %rcx
.Ltmp301:
	.loc	1 1434 0                # bzip2.c:1434:0
	movb	%cl, deleteOutputOnInterrupt(%rip)
	.loc	1 1435 0                # bzip2.c:1435:0
.Ltmp302:
	testq	%rax, %rax
	jne	.LBB10_70
# BB#62:                                # %if.then158
.Ltmp303:
	#DEBUG_VALUE: retVal <- [RBP+-204]
	.loc	1 1436 0                # bzip2.c:1436:0
	movabsq	$inName, %rdi
	callq	remove
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -204(%rbp)
	jne	.LBB10_63
.Ltmp304:
.LBB10_70:                              # %if.end165
	xorq	%rax, %rax
	.loc	1 1441 0                # bzip2.c:1441:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	jmp	.LBB10_71
.LBB10_19:                              # %if.then18
	.loc	1 1302 0                # bzip2.c:1302:0
.Ltmp305:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1303 0                # bzip2.c:1303:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1302 0                # bzip2.c:1302:0
	movabsq	$.L.str72, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
.Ltmp306:
.LBB10_16:                              # %if.end11
	movq	$1, %rdi
	.loc	1 1298 0                # bzip2.c:1298:0
.Ltmp307:
	callq	setExit
.Ltmp308:
.LBB10_71:                              # %return
	.loc	1 1442 0                # bzip2.c:1442:0
	addq	$184, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB10_65:                              # %if.then125
	.loc	1 1399 0                # bzip2.c:1399:0
.Ltmp309:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1400 0                # bzip2.c:1400:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1399 0                # bzip2.c:1399:0
	movabsq	$.L.str79, %rsi
	movabsq	$outName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
.Ltmp310:
.LBB10_55:                              # %if.then104
	.loc	1 1383 0                # bzip2.c:1383:0
	cmpq	$0, -40(%rbp)
	je	.LBB10_16
# BB#56:                                # %if.then109
	movq	-40(%rbp), %rdi
	callq	fclose
	jmp	.LBB10_16
.Ltmp311:
.LBB10_44:                              # %select.mid
	movabsq	$.L.str4, %r9
.LBB10_45:                              # %select.end
	.loc	1 1346 0                # bzip2.c:1346:0
.Ltmp312:
	movabsq	$.L.str76, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB10_16
.Ltmp313:
.LBB10_72:                              # %if.then
	.loc	1 1276 0                # bzip2.c:1276:0
	movabsq	$.L.str90, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp314:
.LBB10_69:                              # %sw.default
	.loc	1 1415 0                # bzip2.c:1415:0
	movabsq	$.L.str93, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp315:
.LBB10_63:                              # %if.then162
	#DEBUG_VALUE: retVal <- [RBP+-204]
	.loc	1 1437 0                # bzip2.c:1437:0
	callq	ioError
.Ltmp316:
.Ltmp317:
	.size	compress, .Ltmp317-compress
.Lfunc_end10:
	.cfi_endproc

	.align	16, 0x90
	.type	uncompress,@function
uncompress:                             # @uncompress
.Lfunc_begin11:
	.loc	1 1448 0                # bzip2.c:1448:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp321:
	.cfi_def_cfa_offset 16
.Ltmp322:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp323:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$200, %rsp
.Ltmp324:
	.cfi_offset %rbx, -40
.Ltmp325:
	.cfi_offset %r14, -32
.Ltmp326:
	.cfi_offset %r15, -24
	movq	%rdi, -32(%rbp)
	xorq	%rax, %rax
	.loc	1 1458 0 prologue_end   # bzip2.c:1458:0
.Ltmp327:
	cmpq	$0, -32(%rbp)
.Ltmp328:
	.loc	1 1456 0                # bzip2.c:1456:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	jne	.LBB11_2
# BB#1:                                 # %land.lhs.true
	.loc	1 1458 0                # bzip2.c:1458:0
.Ltmp329:
	movl	srcMode(%rip), %ecx
	cmpq	$1, %rcx
	jne	.LBB11_81
.Ltmp330:
.LBB11_2:                               # %if.end
	.loc	1 1462 0                # bzip2.c:1462:0
	movl	srcMode(%rip), %ecx
	.loc	1 1461 0                # bzip2.c:1461:0
	movb	%al, -58(%rbp)
	.loc	1 1462 0                # bzip2.c:1462:0
	cmpq	$3, %rcx
	je	.LBB11_6
# BB#3:                                 # %if.end
	cmpq	$2, %rcx
	jne	.LBB11_4
# BB#11:                                # %sw.bb9
	.loc	1 1477 0                # bzip2.c:1477:0
.Ltmp331:
	movq	-32(%rbp), %rsi
	movabsq	$inName, %rdi
	jmp	.LBB11_12
.LBB11_6:                               # %sw.bb2
	.loc	1 1468 0                # bzip2.c:1468:0
	movq	-32(%rbp), %rsi
	movabsq	$inName, %rdi
	callq	copyFileName
	.loc	1 1469 0                # bzip2.c:1469:0
	movq	-32(%rbp), %rsi
	movabsq	$outName, %rbx
	movq	%rbx, %rdi
	callq	copyFileName
	xorq	%rax, %rax
	jmp	.LBB11_7
	.align	16, 0x90
.LBB11_9:                               # %for.inc
                                        #   in Loop: Header=BB11_7 Depth=1
	.loc	1 1470 0                # bzip2.c:1470:0
.Ltmp332:
	movl	-56(%rbp), %eax
	incq	%rax
.LBB11_7:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -56(%rbp)
	movslq	-56(%rbp), %rax
	cmpq	$3, %rax
	jg	.LBB11_10
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB11_7 Depth=1
	.loc	1 1471 0                # bzip2.c:1471:0
.Ltmp333:
	movslq	-56(%rbp), %rax
	movq	zSuffix(,%rax,8), %rsi
	movq	unzSuffix(,%rax,8), %rdx
	movq	%rbx, %rdi
	callq	mapSuffix
	testq	%rax, %rax
	je	.LBB11_9
	jmp	.LBB11_13
.Ltmp334:
.LBB11_4:                               # %if.end
	cmpq	$1, %rcx
	jne	.LBB11_13
# BB#5:                                 # %sw.bb
	.loc	1 1464 0                # bzip2.c:1464:0
	movabsq	$inName, %rdi
	movabsq	$.L.str37, %rsi
.LBB11_12:                              # %zzz
	.loc	1 1477 0                # bzip2.c:1477:0
	callq	copyFileName
	.loc	1 1478 0                # bzip2.c:1478:0
	movabsq	$outName, %rdi
	movabsq	$.L.str70, %rsi
	callq	copyFileName
	jmp	.LBB11_13
.LBB11_10:                              # %for.end
	movq	$1, %rax
	.loc	1 1473 0                # bzip2.c:1473:0
	movb	%al, -58(%rbp)
	.loc	1 1474 0                # bzip2.c:1474:0
	movabsq	$outName, %rdi
	movabsq	$.L.str71, %rsi
	callq	strcat
.Ltmp335:
.LBB11_13:                              # %zzz
	.loc	1 1483 0                # bzip2.c:1483:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB11_21
# BB#14:                                # %land.lhs.true11
	movabsq	$inName, %rdi
	callq	containsDubiousChars
	testq	%rax, %rax
	je	.LBB11_21
# BB#15:                                # %if.then14
	.loc	1 1484 0                # bzip2.c:1484:0
.Ltmp336:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_20
# BB#16:                                # %if.then16
	.loc	1 1485 0                # bzip2.c:1485:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str38, %rsi
	jmp	.LBB11_17
.Ltmp337:
.LBB11_21:                              # %if.end19
	.loc	1 1490 0                # bzip2.c:1490:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB11_24
# BB#22:                                # %land.lhs.true22
	movabsq	$inName, %rdi
	callq	fileExists
	testq	%rax, %rax
	je	.LBB11_23
.Ltmp338:
.LBB11_24:                              # %if.end29
	.loc	1 1496 0                # bzip2.c:1496:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB11_26
# BB#25:                                # %lor.lhs.false
	movl	srcMode(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB11_28
.LBB11_26:                              # %if.then34
	.loc	1 1497 0                # bzip2.c:1497:0
.Ltmp339:
	movabsq	$inName, %rdi
	leaq	-208(%rbp), %rsi
	callq	stat
	.loc	1 1498 0                # bzip2.c:1498:0
.Ltmp340:
	movl	-184(%rbp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB11_28
# BB#27:                                # %if.then38
	.loc	1 1499 0                # bzip2.c:1499:0
.Ltmp341:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str40, %rsi
.Ltmp342:
.LBB11_17:                              # %if.end18
	.loc	1 1485 0                # bzip2.c:1485:0
	movabsq	$inName, %rcx
.LBB11_18:                              # %if.end18
	xorq	%rax, %rax
.LBB11_19:                              # %if.end18
	callq	fprintf
	jmp	.LBB11_20
.Ltmp343:
.LBB11_28:                              # %if.end41
	.loc	1 1506 0                # bzip2.c:1506:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB11_33
# BB#29:                                # %land.lhs.true44
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	jne	.LBB11_33
# BB#30:                                # %land.lhs.true46
	movabsq	$inName, %rdi
	callq	notAStandardFile
	testq	%rax, %rax
	je	.LBB11_33
# BB#31:                                # %if.then50
	.loc	1 1507 0                # bzip2.c:1507:0
.Ltmp344:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_20
# BB#32:                                # %if.then52
	.loc	1 1508 0                # bzip2.c:1508:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str73, %rsi
	jmp	.LBB11_17
.Ltmp345:
.LBB11_33:                              # %if.end55
	.loc	1 1513 0                # bzip2.c:1513:0
	movzbl	-58(%rbp), %eax
	testq	%rax, %rax
	je	.LBB11_36
# BB#34:                                # %if.then57
	.loc	1 1514 0                # bzip2.c:1514:0
.Ltmp346:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_36
# BB#35:                                # %if.then59
	.loc	1 1515 0                # bzip2.c:1515:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str74, %rsi
	movabsq	$inName, %rcx
	movabsq	$outName, %r8
	xorq	%rax, %rax
	callq	fprintf
.Ltmp347:
.LBB11_36:                              # %if.end62
	.loc	1 1520 0                # bzip2.c:1520:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB11_40
# BB#37:                                # %land.lhs.true65
	movabsq	$outName, %rdi
	callq	fileExists
	testq	%rax, %rax
	je	.LBB11_40
# BB#38:                                # %if.then69
	.loc	1 1521 0                # bzip2.c:1521:0
.Ltmp348:
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_82
# BB#39:                                # %if.then71
	.loc	1 1522 0                # bzip2.c:1522:0
.Ltmp349:
	movabsq	$outName, %rdi
	callq	remove
.Ltmp350:
.LBB11_40:                              # %if.end75
	.loc	1 1530 0                # bzip2.c:1530:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB11_47
# BB#41:                                # %land.lhs.true78
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	jne	.LBB11_47
# BB#42:                                # %land.lhs.true80
	.loc	1 1531 0                # bzip2.c:1531:0
	movabsq	$inName, %rdi
	callq	countHardLinks
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -52(%rbp)
	jle	.LBB11_47
# BB#43:                                # %if.then84
	.loc	1 1532 0                # bzip2.c:1532:0
.Ltmp351:
	movq	stderr(%rip), %rdi
	movslq	-52(%rbp), %r8
	movq	progName(%rip), %rdx
	cmpq	$1, %r8
	jle	.LBB11_45
.Ltmp352:
# BB#44:
	movabsq	$.L.str77, %r9
	jmp	.LBB11_46
.LBB11_47:                              # %if.end88
	.loc	1 1538 0                # bzip2.c:1538:0
.Ltmp353:
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB11_49
# BB#48:                                # %if.then91
	.loc	1 1541 0                # bzip2.c:1541:0
.Ltmp354:
	movabsq	$inName, %rdi
	callq	saveInputFileMetaInfo
.Ltmp355:
.LBB11_49:                              # %if.end92
	.loc	1 1544 0                # bzip2.c:1544:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB11_58
# BB#50:                                # %if.end92
	cmpq	$2, %rax
	jne	.LBB11_51
# BB#54:                                # %sw.bb101
	.loc	1 1561 0                # bzip2.c:1561:0
.Ltmp356:
	movabsq	$inName, %rdi
	movabsq	$.L.str43, %rsi
	callq	fopen
	movq	%rax, -40(%rbp)
	.loc	1 1562 0                # bzip2.c:1562:0
	movq	stdout(%rip), %rax
	.loc	1 1563 0                # bzip2.c:1563:0
.Ltmp357:
	cmpq	$0, -40(%rbp)
.Ltmp358:
	.loc	1 1562 0                # bzip2.c:1562:0
	movq	%rax, -48(%rbp)
	jne	.LBB11_64
# BB#55:                                # %if.then105
	.loc	1 1564 0                # bzip2.c:1564:0
.Ltmp359:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1565 0                # bzip2.c:1565:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1564 0                # bzip2.c:1564:0
	movabsq	$.L.str44, %rsi
	movabsq	$inName, %rcx
	jmp	.LBB11_56
.Ltmp360:
.LBB11_23:                              # %if.then25
	.loc	1 1491 0                # bzip2.c:1491:0
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1492 0                # bzip2.c:1492:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1491 0                # bzip2.c:1491:0
	movabsq	$.L.str72, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
	jmp	.LBB11_20
.Ltmp361:
.LBB11_58:                              # %sw.bb115
	.loc	1 1573 0                # bzip2.c:1573:0
	movabsq	$inName, %rdi
	movabsq	$.L.str43, %rsi
	callq	fopen
	movq	%rax, -40(%rbp)
	.loc	1 1574 0                # bzip2.c:1574:0
	movabsq	$outName, %rdi
	movabsq	$.L.str78, %rsi
	callq	fopen_output_safely
	testq	%rax, %rax
	movq	%rax, -48(%rbp)
	je	.LBB11_59
# BB#60:                                # %if.end129
	.loc	1 1582 0                # bzip2.c:1582:0
.Ltmp362:
	cmpq	$0, -40(%rbp)
	jne	.LBB11_64
# BB#61:                                # %if.then132
	.loc	1 1583 0                # bzip2.c:1583:0
.Ltmp363:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1584 0                # bzip2.c:1584:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1583 0                # bzip2.c:1583:0
	movabsq	$.L.str72, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
	.loc	1 1585 0                # bzip2.c:1585:0
.Ltmp364:
	cmpq	$0, -48(%rbp)
	je	.LBB11_20
# BB#62:                                # %if.then138
	movq	-48(%rbp), %rdi
	callq	fclose
	jmp	.LBB11_20
.Ltmp365:
.LBB11_51:                              # %if.end92
	cmpq	$1, %rax
	jne	.LBB11_63
# BB#52:                                # %sw.bb93
	.loc	1 1547 0                # bzip2.c:1547:0
	movq	stdin(%rip), %rdi
	.loc	1 1548 0                # bzip2.c:1548:0
	movq	stdout(%rip), %rax
	.loc	1 1547 0                # bzip2.c:1547:0
	movq	%rdi, -40(%rbp)
	.loc	1 1548 0                # bzip2.c:1548:0
	movq	%rax, -48(%rbp)
	.loc	1 1549 0                # bzip2.c:1549:0
.Ltmp366:
	callq	fileno
	movq	%rax, %rdi
	callq	isatty
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_64
# BB#53:                                # %if.then97
	.loc	1 1550 0                # bzip2.c:1550:0
.Ltmp367:
	movq	stderr(%rip), %rbx
	movq	progName(%rip), %rdx
	movabsq	$.L.str41, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 1553 0                # bzip2.c:1553:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rcx
	jmp	.LBB11_19
.Ltmp368:
.LBB11_64:                              # %sw.epilog142
	.loc	1 1596 0                # bzip2.c:1596:0
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB11_66
# BB#65:                                # %if.then145
	.loc	1 1597 0                # bzip2.c:1597:0
.Ltmp369:
	movq	stderr(%rip), %r14
	movabsq	$.L.str46, %rsi
	movabsq	$inName, %rbx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	.loc	1 1598 0                # bzip2.c:1598:0
	movq	%rbx, %rdi
	callq	pad
	.loc	1 1599 0                # bzip2.c:1599:0
	movq	%r14, %rdi
	callq	fflush
.Ltmp370:
.LBB11_66:                              # %if.end148
	.loc	1 1603 0                # bzip2.c:1603:0
	movq	-48(%rbp), %rax
	.loc	1 1605 0                # bzip2.c:1605:0
	movq	-40(%rbp), %rdi
	movq	-48(%rbp), %rsi
	.loc	1 1603 0                # bzip2.c:1603:0
	movq	%rax, outputHandleJustInCase(%rip)
	movq	$1, %rbx
	.loc	1 1604 0                # bzip2.c:1604:0
	movb	%bl, deleteOutputOnInterrupt(%rip)
	.loc	1 1605 0                # bzip2.c:1605:0
	callq	uncompressStream
	movb	%al, -57(%rbp)
	.loc	1 1609 0                # bzip2.c:1609:0
.Ltmp371:
	movzbl	-57(%rbp), %eax
.Ltmp372:
	.loc	1 1606 0                # bzip2.c:1606:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 1609 0                # bzip2.c:1609:0
.Ltmp373:
	testq	%rax, %rax
	je	.LBB11_71
# BB#67:                                # %if.then151
	.loc	1 1610 0                # bzip2.c:1610:0
.Ltmp374:
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	jne	.LBB11_73
# BB#68:                                # %if.then154
	.loc	1 1611 0                # bzip2.c:1611:0
.Ltmp375:
	movabsq	$outName, %rdi
	callq	applySavedMetaInfoToOutputFile
	.loc	1 1613 0                # bzip2.c:1613:0
.Ltmp376:
	movzbl	keepInputFiles(%rip), %eax
	xorq	%rcx, %rcx
.Ltmp377:
	.loc	1 1612 0                # bzip2.c:1612:0
	movb	%cl, deleteOutputOnInterrupt(%rip)
	.loc	1 1613 0                # bzip2.c:1613:0
.Ltmp378:
	testq	%rax, %rax
	jne	.LBB11_73
# BB#69:                                # %if.then156
.Ltmp379:
	#DEBUG_VALUE: retVal <- [RBP+-212]
	.loc	1 1614 0                # bzip2.c:1614:0
	movabsq	$inName, %rdi
	callq	remove
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -212(%rbp)
	je	.LBB11_73
# BB#70:                                # %if.then160
.Ltmp380:
	#DEBUG_VALUE: retVal <- [RBP+-212]
	.loc	1 1615 0                # bzip2.c:1615:0
	callq	ioError
.Ltmp381:
.LBB11_71:                              # %if.else164
	.loc	1 1621 0                # bzip2.c:1621:0
	movl	srcMode(%rip), %eax
.Ltmp382:
	.loc	1 1619 0                # bzip2.c:1619:0
	movb	%bl, unzFailsExist(%rip)
	xorq	%rcx, %rcx
	.loc	1 1620 0                # bzip2.c:1620:0
	movb	%cl, deleteOutputOnInterrupt(%rip)
	.loc	1 1621 0                # bzip2.c:1621:0
.Ltmp383:
	cmpq	$3, %rax
	jne	.LBB11_73
# BB#72:                                # %if.then167
.Ltmp384:
	#DEBUG_VALUE: retVal <- [RBP+-216]
	.loc	1 1622 0                # bzip2.c:1622:0
	movabsq	$outName, %rdi
	callq	remove
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -216(%rbp)
	jne	.LBB11_83
.Ltmp385:
.LBB11_73:                              # %if.end175
	.loc	1 1628 0                # bzip2.c:1628:0
	movzbl	-57(%rbp), %eax
	xorq	%rcx, %rcx
.Ltmp386:
	.loc	1 1626 0                # bzip2.c:1626:0
	movb	%cl, deleteOutputOnInterrupt(%rip)
	.loc	1 1628 0                # bzip2.c:1628:0
.Ltmp387:
	testq	%rax, %rax
	je	.LBB11_77
# BB#74:                                # %if.then177
	.loc	1 1629 0                # bzip2.c:1629:0
.Ltmp388:
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB11_80
# BB#75:                                # %if.then180
	.loc	1 1630 0                # bzip2.c:1630:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str81, %rsi
	jmp	.LBB11_76
.Ltmp389:
.LBB11_77:                              # %if.else183
	movq	$2, %rdi
	.loc	1 1632 0                # bzip2.c:1632:0
.Ltmp390:
	callq	setExit
	.loc	1 1633 0                # bzip2.c:1633:0
.Ltmp391:
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB11_79
# BB#78:                                # %if.then186
	.loc	1 1634 0                # bzip2.c:1634:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str82, %rsi
.Ltmp392:
.LBB11_76:                              # %if.end191
	xorq	%rax, %rax
	.loc	1 1630 0                # bzip2.c:1630:0
.Ltmp393:
	callq	fprintf
	jmp	.LBB11_80
.Ltmp394:
.LBB11_82:                              # %if.else
	.loc	1 1524 0                # bzip2.c:1524:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str75, %rsi
	movabsq	$outName, %rcx
	jmp	.LBB11_18
.Ltmp395:
.LBB11_59:                              # %if.then120
	.loc	1 1576 0                # bzip2.c:1576:0
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1577 0                # bzip2.c:1577:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1576 0                # bzip2.c:1576:0
	movabsq	$.L.str79, %rsi
	movabsq	$outName, %rcx
.Ltmp396:
.LBB11_56:                              # %if.then105
	xorq	%rax, %rax
	.loc	1 1564 0                # bzip2.c:1564:0
.Ltmp397:
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
	.loc	1 1566 0                # bzip2.c:1566:0
.Ltmp398:
	cmpq	$0, -40(%rbp)
	je	.LBB11_20
# BB#57:                                # %if.then111
	movq	-40(%rbp), %rdi
	callq	fclose
	jmp	.LBB11_20
.Ltmp399:
.LBB11_79:                              # %if.else188
	.loc	1 1635 0                # bzip2.c:1635:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str83, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB11_80
.Ltmp400:
.LBB11_45:                              # %select.mid
	movabsq	$.L.str4, %r9
.LBB11_46:                              # %select.end
	.loc	1 1532 0                # bzip2.c:1532:0
.Ltmp401:
	movabsq	$.L.str76, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
.Ltmp402:
.LBB11_20:                              # %if.end18
	movq	$1, %rdi
	.loc	1 1487 0                # bzip2.c:1487:0
.Ltmp403:
	callq	setExit
.Ltmp404:
.LBB11_80:                              # %if.end191
	.loc	1 1640 0                # bzip2.c:1640:0
	addq	$200, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB11_81:                              # %if.then
	.loc	1 1459 0                # bzip2.c:1459:0
.Ltmp405:
	movabsq	$.L.str69, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp406:
.LBB11_63:                              # %sw.default
	.loc	1 1592 0                # bzip2.c:1592:0
	movabsq	$.L.str80, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp407:
.LBB11_83:                              # %if.then172
	#DEBUG_VALUE: retVal <- [RBP+-216]
	.loc	1 1623 0                # bzip2.c:1623:0
	callq	ioError
.Ltmp408:
.Ltmp409:
	.size	uncompress, .Ltmp409-uncompress
.Lfunc_end11:
	.cfi_endproc

	.align	16, 0x90
	.type	setExit,@function
setExit:                                # @setExit
.Lfunc_begin12:
	.loc	1 779 0                 # bzip2.c:779:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp412:
	.cfi_def_cfa_offset 16
.Ltmp413:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp414:
	.cfi_def_cfa_register %rbp
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	.loc	1 783 0 prologue_end    # bzip2.c:783:0
.Ltmp415:
	movslq	exitValue(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%edi, -4(%rbp)
	cmpq	%rdx, %rax
	jle	.LBB12_2
# BB#1:                                 # %if.then
	movl	-4(%rbp), %eax
	movl	%eax, exitValue(%rip)
.Ltmp416:
.LBB12_2:                               # %if.end
	.loc	1 785 0                 # bzip2.c:785:0
	popq	%rbp
	retq
.Ltmp417:
.Ltmp418:
	.size	setExit, .Ltmp418-setExit
.Lfunc_end12:
	.cfi_endproc

	.align	16, 0x90
	.type	testf,@function
testf:                                  # @testf
.Lfunc_begin13:
	.loc	1 1646 0                # bzip2.c:1646:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp422:
	.cfi_def_cfa_offset 16
.Ltmp423:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp424:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$168, %rsp
.Ltmp425:
	.cfi_offset %rbx, -40
.Ltmp426:
	.cfi_offset %r14, -32
.Ltmp427:
	.cfi_offset %r15, -24
	movq	%rdi, -32(%rbp)
	xorq	%rax, %rax
	.loc	1 1653 0 prologue_end   # bzip2.c:1653:0
.Ltmp428:
	cmpq	$0, -32(%rbp)
.Ltmp429:
	.loc	1 1651 0                # bzip2.c:1651:0
	movb	%al, deleteOutputOnInterrupt(%rip)
	jne	.LBB13_2
# BB#1:                                 # %land.lhs.true
	.loc	1 1653 0                # bzip2.c:1653:0
.Ltmp430:
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB13_38
.Ltmp431:
.LBB13_2:                               # %if.end
	.loc	1 1656 0                # bzip2.c:1656:0
	movabsq	$outName, %rdi
	movabsq	$.L.str6, %rsi
	callq	copyFileName
	.loc	1 1657 0                # bzip2.c:1657:0
	movl	srcMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB13_6
# BB#3:                                 # %if.end
	cmpq	$2, %rax
	jne	.LBB13_4
.LBB13_6:                               # %sw.bb3
	.loc	1 1660 0                # bzip2.c:1660:0
.Ltmp432:
	movq	-32(%rbp), %rsi
	movabsq	$inName, %rdi
	jmp	.LBB13_7
.LBB13_4:                               # %if.end
	cmpq	$1, %rax
	jne	.LBB13_8
# BB#5:                                 # %sw.bb
	.loc	1 1658 0                # bzip2.c:1658:0
	movabsq	$inName, %rdi
	movabsq	$.L.str37, %rsi
.LBB13_7:                               # %sw.epilog
	.loc	1 1660 0                # bzip2.c:1660:0
	callq	copyFileName
.Ltmp433:
.LBB13_8:                               # %sw.epilog
	.loc	1 1663 0                # bzip2.c:1663:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB13_15
# BB#9:                                 # %land.lhs.true5
	movabsq	$inName, %rdi
	callq	containsDubiousChars
	testq	%rax, %rax
	je	.LBB13_15
# BB#10:                                # %if.then6
	.loc	1 1664 0                # bzip2.c:1664:0
.Ltmp434:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_14
# BB#11:                                # %if.then8
	.loc	1 1665 0                # bzip2.c:1665:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str38, %rsi
	jmp	.LBB13_12
.Ltmp435:
.LBB13_15:                              # %if.end11
	.loc	1 1670 0                # bzip2.c:1670:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB13_19
# BB#16:                                # %land.lhs.true14
	movabsq	$inName, %rdi
	callq	fileExists
	testq	%rax, %rax
	je	.LBB13_17
.Ltmp436:
.LBB13_19:                              # %if.end21
	.loc	1 1676 0                # bzip2.c:1676:0
	movl	srcMode(%rip), %eax
	cmpq	$1, %rax
	je	.LBB13_22
# BB#20:                                # %if.then24
	.loc	1 1677 0                # bzip2.c:1677:0
.Ltmp437:
	movabsq	$inName, %rdi
	leaq	-192(%rbp), %rsi
	callq	stat
	.loc	1 1678 0                # bzip2.c:1678:0
.Ltmp438:
	movl	-168(%rbp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB13_22
# BB#21:                                # %if.then28
	.loc	1 1679 0                # bzip2.c:1679:0
.Ltmp439:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str40, %rsi
.Ltmp440:
.LBB13_12:                              # %if.end10
	.loc	1 1665 0                # bzip2.c:1665:0
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	jmp	.LBB13_13
.Ltmp441:
.LBB13_22:                              # %if.end31
	.loc	1 1687 0                # bzip2.c:1687:0
	movl	srcMode(%rip), %eax
	leaq	-2(%rax), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jae	.LBB13_23
# BB#26:                                # %sw.bb40
	.loc	1 1703 0                # bzip2.c:1703:0
.Ltmp442:
	movabsq	$inName, %rdi
	movabsq	$.L.str43, %rsi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, -40(%rbp)
	jne	.LBB13_30
# BB#27:                                # %if.then44
	.loc	1 1705 0                # bzip2.c:1705:0
.Ltmp443:
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1706 0                # bzip2.c:1706:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1705 0                # bzip2.c:1705:0
	movabsq	$.L.str44, %rsi
	jmp	.LBB13_18
.Ltmp444:
.LBB13_23:                              # %if.end31
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB13_28
# BB#24:                                # %sw.bb32
	.loc	1 1690 0                # bzip2.c:1690:0
.Ltmp445:
	movq	stdin(%rip), %r14
	movq	%r14, %rdi
	callq	fileno
	movq	%rax, %rdi
	callq	isatty
	testq	%rbx, %rax
	je	.LBB13_29
# BB#25:                                # %if.then36
	.loc	1 1691 0                # bzip2.c:1691:0
.Ltmp446:
	movq	stderr(%rip), %rbx
	movq	progName(%rip), %rdx
	movabsq	$.L.str41, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 1694 0                # bzip2.c:1694:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rcx
.Ltmp447:
.LBB13_13:                              # %if.end10
	.loc	1 1665 0                # bzip2.c:1665:0
	callq	fprintf
	jmp	.LBB13_14
.Ltmp448:
.LBB13_17:                              # %if.then17
	.loc	1 1671 0                # bzip2.c:1671:0
	movq	stderr(%rip), %r14
	movq	progName(%rip), %r15
	.loc	1 1672 0                # bzip2.c:1672:0
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	.loc	1 1671 0                # bzip2.c:1671:0
	movabsq	$.L.str39, %rsi
.LBB13_18:                              # %if.end67
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	fprintf
.Ltmp449:
.LBB13_14:                              # %if.end10
	movq	$1, %rdi
	.loc	1 1667 0                # bzip2.c:1667:0
.Ltmp450:
	callq	setExit
.Ltmp451:
.LBB13_37:                              # %if.end67
	.loc	1 1729 0                # bzip2.c:1729:0
	addq	$168, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB13_29:                              # %if.end39
	.loc	1 1699 0                # bzip2.c:1699:0
.Ltmp452:
	movq	%r14, -40(%rbp)
.Ltmp453:
.LBB13_30:                              # %sw.epilog49
	.loc	1 1717 0                # bzip2.c:1717:0
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB13_32
# BB#31:                                # %if.then52
	.loc	1 1718 0                # bzip2.c:1718:0
.Ltmp454:
	movq	stderr(%rip), %r14
	movabsq	$.L.str46, %rsi
	movabsq	$inName, %rbx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	.loc	1 1719 0                # bzip2.c:1719:0
	movq	%rbx, %rdi
	callq	pad
	.loc	1 1720 0                # bzip2.c:1720:0
	movq	%r14, %rdi
	callq	fflush
.Ltmp455:
.LBB13_32:                              # %if.end55
	.loc	1 1725 0                # bzip2.c:1725:0
	movq	-40(%rbp), %rdi
	.loc	1 1724 0                # bzip2.c:1724:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 1725 0                # bzip2.c:1725:0
	callq	testStream
	movb	%al, -41(%rbp)
	.loc	1 1727 0                # bzip2.c:1727:0
.Ltmp456:
	movzbl	-41(%rbp), %eax
	testq	%rax, %rax
	je	.LBB13_35
# BB#33:                                # %land.lhs.true59
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB13_35
# BB#34:                                # %if.then62
	movq	stderr(%rip), %rdi
	movabsq	$.L.str47, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp457:
.LBB13_35:                              # %if.end64
	.loc	1 1728 0                # bzip2.c:1728:0
	movzbl	-41(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB13_37
# BB#36:                                # %if.then66
	movq	$1, %rax
	movb	%al, testFailsExist(%rip)
	jmp	.LBB13_37
.Ltmp458:
.LBB13_38:                              # %if.then
	.loc	1 1654 0                # bzip2.c:1654:0
	movabsq	$.L.str36, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp459:
.LBB13_28:                              # %sw.default
	.loc	1 1713 0                # bzip2.c:1713:0
	movabsq	$.L.str45, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp460:
.Ltmp461:
	.size	testf, .Ltmp461-testf
.Lfunc_end13:
	.cfi_endproc

	.align	16, 0x90
	.type	panic,@function
panic:                                  # @panic
.Lfunc_begin14:
	.loc	1 880 0                 # bzip2.c:880:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp464:
	.cfi_def_cfa_offset 16
.Ltmp465:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp466:
	.cfi_def_cfa_register %rbp
	.loc	1 881 0 prologue_end    # bzip2.c:881:0
.Ltmp467:
	subq	$16, %rsp
	movq	stderr(%rip), %r8
	movq	progName(%rip), %rdx
	movq	%rdi, %rcx
	movq	%rcx, -8(%rbp)
	movabsq	$.L.str68, %rsi
	xorq	%rax, %rax
	movq	%r8, %rdi
	callq	fprintf
	.loc	1 892 0                 # bzip2.c:892:0
	callq	showFileNames
	movq	$3, %rdi
	.loc	1 893 0                 # bzip2.c:893:0
	callq	cleanUpAndFail
.Ltmp468:
.Ltmp469:
	.size	panic, .Ltmp469-panic
.Lfunc_end14:
	.cfi_endproc

	.align	16, 0x90
	.type	containsDubiousChars,@function
containsDubiousChars:                   # @containsDubiousChars
.Lfunc_begin15:
	.loc	1 1218 0                # bzip2.c:1218:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp472:
	.cfi_def_cfa_offset 16
.Ltmp473:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp474:
	.cfi_def_cfa_register %rbp
	xorq	%rax, %rax
	movq	%rdi, -8(%rbp)
	.loc	1 1223 0 prologue_end   # bzip2.c:1223:0
.Ltmp475:
	popq	%rbp
	retq
.Ltmp476:
.Ltmp477:
	.size	containsDubiousChars, .Ltmp477-containsDubiousChars
.Lfunc_end15:
	.cfi_endproc

	.align	16, 0x90
	.type	fileExists,@function
fileExists:                             # @fileExists
.Lfunc_begin16:
	.loc	1 1084 0                # bzip2.c:1084:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp480:
	.cfi_def_cfa_offset 16
.Ltmp481:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp482:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 1085 0 prologue_end   # bzip2.c:1085:0
.Ltmp483:
	movabsq	$.L.str43, %rsi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, -16(%rbp)
	je	.LBB16_1
# BB#2:                                 # %entry
	.loc	1 1086 0                # bzip2.c:1086:0
	movabsq	$1, %rax
	jmp	.LBB16_3
.LBB16_1:
	movabsq	$0, %rax
.LBB16_3:                               # %entry
	.loc	1 1087 0                # bzip2.c:1087:0
.Ltmp484:
	cmpq	$0, -16(%rbp)
.Ltmp485:
	.loc	1 1086 0                # bzip2.c:1086:0
	movb	%al, -17(%rbp)
	je	.LBB16_5
# BB#4:                                 # %if.then
	.loc	1 1087 0                # bzip2.c:1087:0
.Ltmp486:
	movq	-16(%rbp), %rdi
	callq	fclose
.Ltmp487:
.LBB16_5:                               # %if.end
	.loc	1 1088 0                # bzip2.c:1088:0
	movzbl	-17(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp488:
.Ltmp489:
	.size	fileExists, .Ltmp489-fileExists
.Lfunc_end16:
	.cfi_endproc

	.align	16, 0x90
	.type	pad,@function
pad:                                    # @pad
.Lfunc_begin17:
	.loc	1 1052 0                # bzip2.c:1052:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp493:
	.cfi_def_cfa_offset 16
.Ltmp494:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp495:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$24, %rsp
.Ltmp496:
	.cfi_offset %rbx, -40
.Ltmp497:
	.cfi_offset %r14, -32
.Ltmp498:
	.cfi_offset %r15, -24
	movq	%rdi, -32(%rbp)
	.loc	1 1054 0 prologue_end   # bzip2.c:1054:0
.Ltmp499:
	callq	strlen
	movq	$32, %rcx
	movslq	longestFileName(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jge	.LBB17_4
.Ltmp500:
# BB#1:                                 # %if.end
	movq	$1, %rax
	.loc	1 1055 0                # bzip2.c:1055:0
.Ltmp501:
	movl	%eax, -36(%rbp)
	.loc	1 1056 0                # bzip2.c:1056:0
	movabsq	$.L.str67, %r14
	jmp	.LBB17_2
	.align	16, 0x90
.LBB17_3:                               # %for.inc
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	stderr(%rip), %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	.loc	1 1055 0                # bzip2.c:1055:0
	movl	-36(%rbp), %eax
	incq	%rax
	movl	%eax, -36(%rbp)
.LBB17_2:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-36(%rbp), %r15
	movl	longestFileName(%rip), %ebx
	movq	-32(%rbp), %rdi
	callq	strlen
	subq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %r15
	jle	.LBB17_3
.Ltmp502:
.LBB17_4:                               # %for.end
	.loc	1 1057 0                # bzip2.c:1057:0
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp503:
.Ltmp504:
	.size	pad, .Ltmp504-pad
.Lfunc_end17:
	.cfi_endproc

	.align	16, 0x90
	.type	testStream,@function
testStream:                             # @testStream
.Lfunc_begin18:
	.loc	1 678 0                 # bzip2.c:678:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp508:
	.cfi_def_cfa_offset 16
.Ltmp509:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp510:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$10088, %rsp            # imm = 0x2768
.Ltmp511:
	.cfi_offset %rbx, -56
.Ltmp512:
	.cfi_offset %r12, -48
.Ltmp513:
	.cfi_offset %r13, -40
.Ltmp514:
	.cfi_offset %r14, -32
.Ltmp515:
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	.loc	1 679 0 prologue_end    # bzip2.c:679:0
.Ltmp516:
	movq	$0, -64(%rbp)
	.loc	1 691 0                 # bzip2.c:691:0
.Ltmp517:
	movq	-56(%rbp), %rdi
	xorq	%r13, %r13
.Ltmp518:
	.loc	1 687 0                 # bzip2.c:687:0
	movl	%r13d, -10100(%rbp)
	.loc	1 688 0                 # bzip2.c:688:0
	movl	%r13d, -84(%rbp)
	.loc	1 691 0                 # bzip2.c:691:0
.Ltmp519:
	callq	ferror
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB18_27
.Ltmp520:
# BB#1:
	leaq	-68(%rbp), %rbx
	leaq	-5088(%rbp), %r15
	movabsq	$4294967291, %r12       # imm = 0xFFFFFFFB
.LBB18_2:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_5 Depth 2
                                        #     Child Loop BB18_14 Depth 2
	.loc	1 695 0                 # bzip2.c:695:0
.Ltmp521:
	movq	-56(%rbp), %rsi
	movzbl	smallMode(%rip), %ecx
	movl	-10100(%rbp), %r9d
	movl	verbosity(%rip), %edx
	movq	%rbx, %rdi
	leaq	-10096(%rbp), %r8
	callq	BZ2_bzReadOpen
	testq	%rax, %rax
	movq	%rax, -64(%rbp)
	je	.LBB18_7
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 699 0                 # bzip2.c:699:0
.Ltmp522:
	movl	-68(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_7
.Ltmp523:
# BB#4:                                 # %if.end6
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 700 0                 # bzip2.c:700:0
	movl	-84(%rbp), %eax
	incq	%rax
	movl	%eax, -84(%rbp)
	.align	16, 0x90
.LBB18_5:                               # %while.cond7
                                        #   Parent Loop BB18_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	.loc	1 702 0                 # bzip2.c:702:0
	movl	-68(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_11
# BB#6:                                 # %while.body10
                                        #   in Loop: Header=BB18_5 Depth=2
	.loc	1 703 0                 # bzip2.c:703:0
.Ltmp524:
	movq	-64(%rbp), %rsi
	movq	$5000, %rcx             # imm = 0x1388
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	BZ2_bzRead
	.loc	1 704 0                 # bzip2.c:704:0
.Ltmp525:
	movl	-68(%rbp), %ecx
.Ltmp526:
	.loc	1 703 0                 # bzip2.c:703:0
	movl	%eax, -80(%rbp)
	.loc	1 704 0                 # bzip2.c:704:0
.Ltmp527:
	cmpq	%r12, %rcx
	jne	.LBB18_5
	jmp	.LBB18_7
.Ltmp528:
	.align	16, 0x90
.LBB18_11:                              # %while.end
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 706 0                 # bzip2.c:706:0
	movl	-68(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB18_7
.Ltmp529:
# BB#12:                                # %if.end20
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 708 0                 # bzip2.c:708:0
	movq	-64(%rbp), %rsi
	movq	%rbx, %rdi
	leaq	-10112(%rbp), %rdx
	leaq	-10100(%rbp), %rcx
	callq	BZ2_bzReadGetUnused
	.loc	1 709 0                 # bzip2.c:709:0
.Ltmp530:
	movl	-68(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_39
.Ltmp531:
# BB#13:                                # %if.end24
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 711 0                 # bzip2.c:711:0
	movq	-10112(%rbp), %rax
	movq	%rax, -10120(%rbp)
	.loc	1 712 0                 # bzip2.c:712:0
.Ltmp532:
	movl	%r13d, -88(%rbp)
	jmp	.LBB18_14
	.align	16, 0x90
.LBB18_15:                              # %for.inc
                                        #   in Loop: Header=BB18_14 Depth=2
	movslq	-88(%rbp), %rax
	movq	-10120(%rbp), %rcx
	movl	-88(%rbp), %edx
	movzbl	(%rcx,%rax), %ecx
	movb	%cl, -10096(%rbp,%rax)
	incq	%rdx
	movl	%edx, -88(%rbp)
.LBB18_14:                              # %for.cond
                                        #   Parent Loop BB18_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-88(%rbp), %rax
	movslq	-10100(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB18_15
.Ltmp533:
# BB#16:                                # %for.end
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 714 0                 # bzip2.c:714:0
	movq	-64(%rbp), %rsi
	movq	%rbx, %rdi
	callq	BZ2_bzReadClose
	.loc	1 715 0                 # bzip2.c:715:0
.Ltmp534:
	movl	-68(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_39
.Ltmp535:
# BB#17:                                # %if.end33
                                        #   in Loop: Header=BB18_2 Depth=1
	.loc	1 716 0                 # bzip2.c:716:0
	movl	-10100(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_2
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	-56(%rbp), %rdi
	callq	myfeof
	testq	%rax, %rax
	je	.LBB18_2
.Ltmp536:
# BB#19:                                # %while.end41
	.loc	1 720 0                 # bzip2.c:720:0
	movq	-56(%rbp), %rdi
	callq	ferror
	testq	%r14, %rax
	jne	.LBB18_27
.Ltmp537:
# BB#20:                                # %if.end45
	.loc	1 721 0                 # bzip2.c:721:0
	movq	-56(%rbp), %rdi
	callq	fclose
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	%r14, %rcx
	movl	%eax, -76(%rbp)
	je	.LBB18_27
# BB#21:                                # %if.end50
	.loc	1 724 0                 # bzip2.c:724:0
.Ltmp538:
	movslq	verbosity(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB18_24
# BB#22:                                # %if.then53
	movq	stderr(%rip), %rdi
	movabsq	$.L.str49, %rsi
	jmp	.LBB18_23
.Ltmp539:
.LBB18_7:                               # %errhandler
	.loc	1 728 0                 # bzip2.c:728:0
	movq	-64(%rbp), %rsi
	leaq	-72(%rbp), %rdi
	callq	BZ2_bzReadClose
	.loc	1 729 0                 # bzip2.c:729:0
.Ltmp540:
	movl	verbosity(%rip), %eax
	testq	%rax, %rax
	jne	.LBB18_9
# BB#8:                                 # %if.then58
	.loc	1 730 0                 # bzip2.c:730:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str50, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
.Ltmp541:
.LBB18_9:                               # %if.end60
	.loc	1 731 0                 # bzip2.c:731:0
	movl	-68(%rbp), %eax
	addq	$9, %rax
	andq	%r14, %rax
	cmpq	$6, %rax
	ja	.LBB18_38
# BB#10:                                # %if.end60
	jmpq	*.LJTI18_0(,%rax,8)
.LBB18_31:                              # %sw.bb65
	.loc	1 744 0                 # bzip2.c:744:0
.Ltmp542:
	movq	stderr(%rip), %rdi
	movabsq	$.L.str52, %rsi
	jmp	.LBB18_29
.LBB18_32:                              # %sw.bb67
	.loc	1 751 0                 # bzip2.c:751:0
.Ltmp543:
	movq	-56(%rbp), %rax
	movq	stdin(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB18_34
# BB#33:                                # %if.then70
	movq	-56(%rbp), %rdi
	callq	fclose
.Ltmp544:
.LBB18_34:                              # %if.end72
	.loc	1 753 0                 # bzip2.c:753:0
	movl	-84(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB18_36
# BB#35:                                # %if.then75
	.loc	1 754 0                 # bzip2.c:754:0
.Ltmp545:
	movq	stderr(%rip), %rdi
	movabsq	$.L.str53, %rsi
	jmp	.LBB18_29
.Ltmp546:
.LBB18_28:                              # %sw.bb62
	.loc	1 738 0                 # bzip2.c:738:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str51, %rsi
.LBB18_29:                              # %return
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	fprintf
	.loc	1 740 0                 # bzip2.c:740:0
	movb	%bl, -41(%rbp)
	jmp	.LBB18_25
.LBB18_36:                              # %if.else
	.loc	1 758 0                 # bzip2.c:758:0
.Ltmp547:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_24
# BB#37:                                # %if.then78
	.loc	1 759 0                 # bzip2.c:759:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str54, %rsi
.Ltmp548:
.LBB18_23:                              # %if.end55
	xorq	%rax, %rax
	.loc	1 724 0                 # bzip2.c:724:0
.Ltmp549:
	callq	fprintf
.Ltmp550:
.LBB18_24:                              # %if.end55
	movq	$1, %rax
	.loc	1 725 0                 # bzip2.c:725:0
	movb	%al, -41(%rbp)
.LBB18_25:                              # %return
	.loc	1 769 0                 # bzip2.c:769:0
	movzbl	-41(%rbp), %eax
	addq	$10088, %rsp            # imm = 0x2768
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB18_38:                              # %sw.default
	.loc	1 764 0                 # bzip2.c:764:0
.Ltmp551:
	movabsq	$.L.str55, %rdi
	callq	panic
.Ltmp552:
.LBB18_39:                              # %if.then23
	.loc	1 709 0                 # bzip2.c:709:0
	movabsq	$.L.str48, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp553:
.LBB18_26:                              # %sw.bb
	.loc	1 733 0                 # bzip2.c:733:0
	callq	configError
.LBB18_27:                              # %errhandler_io
	.loc	1 736 0                 # bzip2.c:736:0
	callq	ioError
.LBB18_30:                              # %sw.bb64
	.loc	1 742 0                 # bzip2.c:742:0
	callq	outOfMemory
.Ltmp554:
.Ltmp555:
	.size	testStream, .Ltmp555-testStream
.Lfunc_end18:
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_26
	.quad	.LBB18_38
	.quad	.LBB18_31
	.quad	.LBB18_27
	.quad	.LBB18_32
	.quad	.LBB18_28
	.quad	.LBB18_30

	.text
	.align	16, 0x90
	.type	myfeof,@function
myfeof:                                 # @myfeof
.Lfunc_begin19:
	.loc	1 425 0                 # bzip2.c:425:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp558:
	.cfi_def_cfa_offset 16
.Ltmp559:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp560:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 426 0 prologue_end    # bzip2.c:426:0
.Ltmp561:
	callq	fgetc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	%rcx, %rdx
	movl	%eax, -20(%rbp)
	je	.LBB19_1
# BB#2:                                 # %if.end
	.loc	1 428 0                 # bzip2.c:428:0
	movq	-16(%rbp), %rsi
	movl	-20(%rbp), %edi
	callq	ungetc
	xorq	%rax, %rax
	jmp	.LBB19_3
.LBB19_1:                               # %if.then
	movq	$1, %rax
.LBB19_3:                               # %return
	.loc	1 429 0                 # bzip2.c:429:0
	movb	%al, -1(%rbp)
	.loc	1 430 0                 # bzip2.c:430:0
	movzbl	-1(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp562:
.Ltmp563:
	.size	myfeof, .Ltmp563-myfeof
.Lfunc_end19:
	.cfi_endproc

	.align	16, 0x90
	.type	configError,@function
configError:                            # @configError
.Lfunc_begin20:
	.loc	1 1027 0                # bzip2.c:1027:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp566:
	.cfi_def_cfa_offset 16
.Ltmp567:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp568:
	.cfi_def_cfa_register %rbp
	.loc	1 1028 0 prologue_end   # bzip2.c:1028:0
.Ltmp569:
	movq	stderr(%rip), %rdi
	movabsq	$.L.str66, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	$3, %rdi
	.loc	1 1034 0                # bzip2.c:1034:0
	callq	setExit
	.loc	1 1035 0                # bzip2.c:1035:0
	movl	exitValue(%rip), %edi
	callq	exit
.Ltmp570:
.Ltmp571:
	.size	configError, .Ltmp571-configError
.Lfunc_end20:
	.cfi_endproc

	.align	16, 0x90
	.type	ioError,@function
ioError:                                # @ioError
.Lfunc_begin21:
	.loc	1 930 0                 # bzip2.c:930:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp574:
	.cfi_def_cfa_offset 16
.Ltmp575:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp576:
	.cfi_def_cfa_register %rbp
	.loc	1 931 0 prologue_end    # bzip2.c:931:0
.Ltmp577:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str65, %rsi
	xorq	%rax, %rax
	callq	fprintf
	.loc	1 935 0                 # bzip2.c:935:0
	movq	progName(%rip), %rdi
	callq	perror
	.loc	1 936 0                 # bzip2.c:936:0
	callq	showFileNames
	movq	$1, %rdi
	.loc	1 937 0                 # bzip2.c:937:0
	callq	cleanUpAndFail
.Ltmp578:
.Ltmp579:
	.size	ioError, .Ltmp579-ioError
.Lfunc_end21:
	.cfi_endproc

	.align	16, 0x90
	.type	outOfMemory,@function
outOfMemory:                            # @outOfMemory
.Lfunc_begin22:
	.loc	1 1015 0                # bzip2.c:1015:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp582:
	.cfi_def_cfa_offset 16
.Ltmp583:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp584:
	.cfi_def_cfa_register %rbp
	.loc	1 1016 0 prologue_end   # bzip2.c:1016:0
.Ltmp585:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str56, %rsi
	xorq	%rax, %rax
	callq	fprintf
	.loc	1 1019 0                # bzip2.c:1019:0
	callq	showFileNames
	movq	$1, %rdi
	.loc	1 1020 0                # bzip2.c:1020:0
	callq	cleanUpAndFail
.Ltmp586:
.Ltmp587:
	.size	outOfMemory, .Ltmp587-outOfMemory
.Lfunc_end22:
	.cfi_endproc

	.align	16, 0x90
	.type	showFileNames,@function
showFileNames:                          # @showFileNames
.Lfunc_begin23:
	.loc	1 806 0                 # bzip2.c:806:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp590:
	.cfi_def_cfa_offset 16
.Ltmp591:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp592:
	.cfi_def_cfa_register %rbp
	.loc	1 807 0 prologue_end    # bzip2.c:807:0
.Ltmp593:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_2
# BB#1:                                 # %if.then
	.loc	1 808 0                 # bzip2.c:808:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str64, %rsi
	movabsq	$inName, %rdx
	movabsq	$outName, %rcx
	xorq	%rax, %rax
	callq	fprintf
.Ltmp594:
.LBB23_2:                               # %if.end
	.loc	1 813 0                 # bzip2.c:813:0
	popq	%rbp
	retq
.Ltmp595:
.Ltmp596:
	.size	showFileNames, .Ltmp596-showFileNames
.Lfunc_end23:
	.cfi_endproc

	.align	16, 0x90
	.type	cleanUpAndFail,@function
cleanUpAndFail:                         # @cleanUpAndFail
.Lfunc_begin24:
	.loc	1 819 0                 # bzip2.c:819:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp600:
	.cfi_def_cfa_offset 16
.Ltmp601:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp602:
	.cfi_def_cfa_register %rbp
	.loc	1 824 0 prologue_end    # bzip2.c:824:0
.Ltmp603:
	pushq	%rbx
	subq	$152, %rsp
.Ltmp604:
	.cfi_offset %rbx, -24
	movl	srcMode(%rip), %eax
	movl	%edi, -12(%rbp)
	cmpq	$3, %rax
	jne	.LBB24_12
# BB#1:                                 # %land.lhs.true
	movl	opMode(%rip), %eax
	cmpq	$3, %rax
	je	.LBB24_12
# BB#2:                                 # %land.lhs.true2
	movzbl	deleteOutputOnInterrupt(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_12
# BB#3:                                 # %if.then
	.loc	1 833 0                 # bzip2.c:833:0
.Ltmp605:
	movabsq	$inName, %rdi
	leaq	-160(%rbp), %rsi
	callq	stat
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movl	%eax, -16(%rbp)
	.loc	1 834 0                 # bzip2.c:834:0
.Ltmp606:
	jne	.LBB24_10
# BB#4:                                 # %if.then5
	.loc	1 835 0                 # bzip2.c:835:0
.Ltmp607:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_6
# BB#5:                                 # %if.then7
	.loc	1 836 0                 # bzip2.c:836:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str57, %rsi
	movabsq	$outName, %rcx
	xorq	%rax, %rax
	callq	fprintf
.Ltmp608:
.LBB24_6:                               # %if.end
	.loc	1 839 0                 # bzip2.c:839:0
	cmpq	$0, outputHandleJustInCase(%rip)
	je	.LBB24_8
# BB#7:                                 # %if.then11
	.loc	1 840 0                 # bzip2.c:840:0
	movq	outputHandleJustInCase(%rip), %rdi
	callq	fclose
.Ltmp609:
.LBB24_8:                               # %if.end13
	.loc	1 841 0                 # bzip2.c:841:0
	movabsq	$outName, %rdi
	callq	remove
	testq	%rbx, %rax
	movl	%eax, -16(%rbp)
	je	.LBB24_12
# BB#9:                                 # %if.then17
	.loc	1 843 0                 # bzip2.c:843:0
.Ltmp610:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str58, %rsi
	xorq	%rax, %rax
	jmp	.LBB24_11
.Ltmp611:
.LBB24_10:                              # %if.else
	.loc	1 848 0                 # bzip2.c:848:0
	movq	stderr(%rip), %rbx
	movq	progName(%rip), %rdx
	movabsq	$.L.str59, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 851 0                 # bzip2.c:851:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str60, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 854 0                 # bzip2.c:854:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str61, %rsi
	movabsq	$outName, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.loc	1 857 0                 # bzip2.c:857:0
	movq	progName(%rip), %rdx
	movabsq	$.L.str62, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
.LBB24_11:                              # %if.end25
	callq	fprintf
.Ltmp612:
.LBB24_12:                              # %if.end25
	.loc	1 864 0                 # bzip2.c:864:0
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_16
# BB#13:                                # %land.lhs.true28
	movslq	numFileNames(%rip), %rax
	testq	%rax, %rax
	jle	.LBB24_16
# BB#14:                                # %land.lhs.true31
	movslq	numFilesProcessed(%rip), %rax
	movslq	numFileNames(%rip), %rcx
	cmpq	%rcx, %rax
	jge	.LBB24_16
# BB#15:                                # %if.then34
	.loc	1 865 0                 # bzip2.c:865:0
.Ltmp613:
	movq	stderr(%rip), %rdi
	movl	numFileNames(%rip), %r8d
	movl	numFilesProcessed(%rip), %eax
	movq	progName(%rip), %rdx
	movq	%r8, %r9
	subq	%rax, %r9
	movabsq	$.L.str63, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rcx
	callq	fprintf
.Ltmp614:
.LBB24_16:                              # %if.end36
	.loc	1 872 0                 # bzip2.c:872:0
	movl	-12(%rbp), %edi
	callq	setExit
	.loc	1 873 0                 # bzip2.c:873:0
	movl	exitValue(%rip), %edi
	callq	exit
.Ltmp615:
.Ltmp616:
	.size	cleanUpAndFail, .Ltmp616-cleanUpAndFail
.Lfunc_end24:
	.cfi_endproc

	.align	16, 0x90
	.type	mapSuffix,@function
mapSuffix:                              # @mapSuffix
.Lfunc_begin25:
	.loc	1 1256 0                # bzip2.c:1256:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp620:
	.cfi_def_cfa_offset 16
.Ltmp621:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp622:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
.Ltmp623:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	.loc	1 1257 0 prologue_end   # bzip2.c:1257:0
.Ltmp624:
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movq	%rdx, -40(%rbp)
	callq	hasSuffix
	testq	%rax, %rax
	je	.LBB25_1
.Ltmp625:
# BB#2:                                 # %if.end
	.loc	1 1258 0                # bzip2.c:1258:0
	movq	-24(%rbp), %rdi
	callq	strlen
	movq	-32(%rbp), %rdi
	movq	%rax, %rbx
	callq	strlen
	movq	-24(%rbp), %rcx
	subq	%rax, %rbx
	xorq	%rax, %rax
	movb	%al, (%rcx,%rbx)
	.loc	1 1259 0                # bzip2.c:1259:0
	movq	-24(%rbp), %rdi
	movq	-40(%rbp), %rsi
	callq	strcat
	movq	$1, %rax
	jmp	.LBB25_3
.LBB25_1:                               # %if.then
	xorq	%rax, %rax
.LBB25_3:                               # %return
	.loc	1 1260 0                # bzip2.c:1260:0
	movb	%al, -9(%rbp)
	.loc	1 1261 0                # bzip2.c:1261:0
	movzbl	-9(%rbp), %eax
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp626:
.Ltmp627:
	.size	mapSuffix, .Ltmp627-mapSuffix
.Lfunc_end25:
	.cfi_endproc

	.align	16, 0x90
	.type	notAStandardFile,@function
notAStandardFile:                       # @notAStandardFile
.Lfunc_begin26:
	.loc	1 1124 0                # bzip2.c:1124:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp630:
	.cfi_def_cfa_offset 16
.Ltmp631:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp632:
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, -16(%rbp)
	leaq	-168(%rbp), %rsi
	.loc	1 1128 0 prologue_end   # bzip2.c:1128:0
.Ltmp633:
	callq	lstat
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -20(%rbp)
	jne	.LBB26_3
# BB#1:                                 # %if.end
	.loc	1 1130 0                # bzip2.c:1130:0
.Ltmp634:
	movl	-144(%rbp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$32768, %rax            # imm = 0x8000
	jne	.LBB26_3
.Ltmp635:
# BB#2:                                 # %if.then2
	xorq	%rax, %rax
	jmp	.LBB26_4
.LBB26_3:                               # %if.end3
	movq	$1, %rax
.LBB26_4:                               # %return
	.loc	1 1131 0                # bzip2.c:1131:0
	movb	%al, -1(%rbp)
	.loc	1 1132 0                # bzip2.c:1132:0
	movzbl	-1(%rbp), %eax
	addq	$176, %rsp
	popq	%rbp
	retq
.Ltmp636:
.Ltmp637:
	.size	notAStandardFile, .Ltmp637-notAStandardFile
.Lfunc_end26:
	.cfi_endproc

	.align	16, 0x90
	.type	countHardLinks,@function
countHardLinks:                         # @countHardLinks
.Lfunc_begin27:
	.loc	1 1141 0                # bzip2.c:1141:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp640:
	.cfi_def_cfa_offset 16
.Ltmp641:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp642:
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, -16(%rbp)
	leaq	-168(%rbp), %rsi
	.loc	1 1145 0 prologue_end   # bzip2.c:1145:0
.Ltmp643:
	callq	lstat
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -20(%rbp)
	.loc	1 1146 0                # bzip2.c:1146:0
.Ltmp644:
	je	.LBB27_2
.Ltmp645:
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	jmp	.LBB27_3
.LBB27_2:                               # %if.end
	.loc	1 1147 0                # bzip2.c:1147:0
	movq	-152(%rbp), %rax
	decq	%rax
.LBB27_3:                               # %return
	movl	%eax, -4(%rbp)
	.loc	1 1148 0                # bzip2.c:1148:0
	movl	-4(%rbp), %eax
	addq	$176, %rsp
	popq	%rbp
	retq
.Ltmp646:
.Ltmp647:
	.size	countHardLinks, .Ltmp647-countHardLinks
.Lfunc_end27:
	.cfi_endproc

	.align	16, 0x90
	.type	saveInputFileMetaInfo,@function
saveInputFileMetaInfo:                  # @saveInputFileMetaInfo
.Lfunc_begin28:
	.loc	1 1181 0                # bzip2.c:1181:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp650:
	.cfi_def_cfa_offset 16
.Ltmp651:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp652:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 1185 0 prologue_end   # bzip2.c:1185:0
.Ltmp653:
	movabsq	$fileMetaInfo, %rsi
	callq	stat
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, -12(%rbp)
	.loc	1 1186 0                # bzip2.c:1186:0
.Ltmp654:
	jne	.LBB28_2
.Ltmp655:
# BB#1:                                 # %if.end
	.loc	1 1188 0                # bzip2.c:1188:0
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB28_2:                               # %if.then
	.loc	1 1186 0                # bzip2.c:1186:0
.Ltmp656:
	callq	ioError
.Ltmp657:
.Ltmp658:
	.size	saveInputFileMetaInfo, .Ltmp658-saveInputFileMetaInfo
.Lfunc_end28:
	.cfi_endproc

	.align	16, 0x90
	.type	uncompressStream,@function
uncompressStream:                       # @uncompressStream
.Lfunc_begin29:
	.loc	1 549 0                 # bzip2.c:549:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp662:
	.cfi_def_cfa_offset 16
.Ltmp663:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp664:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$10104, %rsp            # imm = 0x2778
.Ltmp665:
	.cfi_offset %rbx, -56
.Ltmp666:
	.cfi_offset %r12, -48
.Ltmp667:
	.cfi_offset %r13, -40
.Ltmp668:
	.cfi_offset %r14, -32
.Ltmp669:
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	.loc	1 550 0 prologue_end    # bzip2.c:550:0
.Ltmp670:
	movq	$0, -72(%rbp)
	.loc	1 564 0                 # bzip2.c:564:0
.Ltmp671:
	movq	-64(%rbp), %rdi
	xorq	%rax, %rax
.Ltmp672:
	.loc	1 558 0                 # bzip2.c:558:0
	movl	%eax, -10116(%rbp)
	.loc	1 559 0                 # bzip2.c:559:0
	movl	%eax, -92(%rbp)
	.loc	1 564 0                 # bzip2.c:564:0
.Ltmp673:
	callq	ferror
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB29_13
.Ltmp674:
# BB#1:                                 # %if.end
	.loc	1 565 0                 # bzip2.c:565:0
	movq	-56(%rbp), %rdi
	callq	ferror
	testq	%r12, %rax
	jne	.LBB29_13
.Ltmp675:
# BB#2:
	leaq	-76(%rbp), %r15
	leaq	-10112(%rbp), %r14
	leaq	-5104(%rbp), %rbx
	movabsq	$4294967291, %r13       # imm = 0xFFFFFFFB
.LBB29_3:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_6 Depth 2
                                        #     Child Loop BB29_17 Depth 2
	.loc	1 569 0                 # bzip2.c:569:0
.Ltmp676:
	movq	-56(%rbp), %rsi
	movzbl	smallMode(%rip), %ecx
	movl	-10116(%rbp), %r9d
	movl	verbosity(%rip), %edx
	movq	%r15, %rdi
	movq	%r14, %r8
	callq	BZ2_bzReadOpen
	testq	%rax, %rax
	movq	%rax, -72(%rbp)
	je	.LBB29_39
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 573 0                 # bzip2.c:573:0
.Ltmp677:
	movl	-76(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB29_39
.Ltmp678:
# BB#5:                                 # %if.end10
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 574 0                 # bzip2.c:574:0
	movl	-92(%rbp), %eax
	incq	%rax
	movl	%eax, -92(%rbp)
	.align	16, 0x90
.LBB29_6:                               # %while.cond11
                                        #   Parent Loop BB29_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	.loc	1 576 0                 # bzip2.c:576:0
	movl	-76(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB29_14
# BB#7:                                 # %while.body14
                                        #   in Loop: Header=BB29_6 Depth=2
	.loc	1 577 0                 # bzip2.c:577:0
.Ltmp679:
	movq	-72(%rbp), %rsi
	movq	$5000, %rcx             # imm = 0x1388
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	BZ2_bzRead
	.loc	1 578 0                 # bzip2.c:578:0
.Ltmp680:
	movl	-76(%rbp), %ecx
.Ltmp681:
	.loc	1 577 0                 # bzip2.c:577:0
	movl	%eax, -88(%rbp)
	.loc	1 578 0                 # bzip2.c:578:0
.Ltmp682:
	cmpq	%r13, %rcx
	je	.LBB29_32
.Ltmp683:
# BB#8:                                 # %if.end20
                                        #   in Loop: Header=BB29_6 Depth=2
	.loc	1 579 0                 # bzip2.c:579:0
	movl	-76(%rbp), %eax
	testq	%rax, %rax
	je	.LBB29_10
# BB#9:                                 # %lor.lhs.false23
                                        #   in Loop: Header=BB29_6 Depth=2
	movl	-76(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB29_12
.LBB29_10:                              # %land.lhs.true
                                        #   in Loop: Header=BB29_6 Depth=2
	movslq	-88(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB29_12
# BB#11:                                # %if.then28
                                        #   in Loop: Header=BB29_6 Depth=2
	.loc	1 580 0                 # bzip2.c:580:0
	movslq	-88(%rbp), %rdx
	movq	-64(%rbp), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	fwrite
.Ltmp684:
.LBB29_12:                              # %if.end32
                                        #   in Loop: Header=BB29_6 Depth=2
	.loc	1 581 0                 # bzip2.c:581:0
	movq	-64(%rbp), %rdi
	callq	ferror
	testq	%r12, %rax
	je	.LBB29_6
	jmp	.LBB29_13
.Ltmp685:
	.align	16, 0x90
.LBB29_14:                              # %while.end
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 583 0                 # bzip2.c:583:0
	movl	-76(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB29_39
.Ltmp686:
# BB#15:                                # %if.end40
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 585 0                 # bzip2.c:585:0
	movq	-72(%rbp), %rsi
	movq	%r15, %rdi
	leaq	-10128(%rbp), %rdx
	leaq	-10116(%rbp), %rcx
	callq	BZ2_bzReadGetUnused
	.loc	1 586 0                 # bzip2.c:586:0
.Ltmp687:
	movl	-76(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB29_54
.Ltmp688:
# BB#16:                                # %if.end44
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 588 0                 # bzip2.c:588:0
	movq	-10128(%rbp), %rax
	movq	%rax, -10136(%rbp)
	xorq	%rax, %rax
	.loc	1 589 0                 # bzip2.c:589:0
.Ltmp689:
	movl	%eax, -96(%rbp)
	jmp	.LBB29_17
	.align	16, 0x90
.LBB29_18:                              # %for.inc
                                        #   in Loop: Header=BB29_17 Depth=2
	movslq	-96(%rbp), %rax
	movq	-10136(%rbp), %rcx
	movl	-96(%rbp), %edx
	movzbl	(%rcx,%rax), %ecx
	movb	%cl, -10112(%rbp,%rax)
	incq	%rdx
	movl	%edx, -96(%rbp)
.LBB29_17:                              # %for.cond
                                        #   Parent Loop BB29_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-96(%rbp), %rax
	movslq	-10116(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB29_18
.Ltmp690:
# BB#19:                                # %for.end
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 591 0                 # bzip2.c:591:0
	movq	-72(%rbp), %rsi
	movq	%r15, %rdi
	callq	BZ2_bzReadClose
	.loc	1 592 0                 # bzip2.c:592:0
.Ltmp691:
	movl	-76(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB29_54
.Ltmp692:
# BB#20:                                # %if.end53
                                        #   in Loop: Header=BB29_3 Depth=1
	.loc	1 594 0                 # bzip2.c:594:0
	movl	-10116(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB29_3
# BB#21:                                # %land.lhs.true56
                                        #   in Loop: Header=BB29_3 Depth=1
	movq	-56(%rbp), %rdi
	callq	myfeof
	testq	%rax, %rax
	je	.LBB29_3
	jmp	.LBB29_22
.Ltmp693:
.LBB29_32:                              # %trycat
	.loc	1 619 0                 # bzip2.c:619:0
	movzbl	forceOverwrite(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_39
# BB#33:                                # %if.then96
	.loc	1 620 0                 # bzip2.c:620:0
.Ltmp694:
	movq	-56(%rbp), %rdi
	callq	rewind
	leaq	-5104(%rbp), %rbx
	.align	16, 0x90
.LBB29_34:                              # %while.body97
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 622 0                 # bzip2.c:622:0
.Ltmp695:
	movq	-56(%rbp), %rdi
	callq	myfeof
	testq	%rax, %rax
	jne	.LBB29_22
.Ltmp696:
# BB#35:                                # %if.end101
                                        #   in Loop: Header=BB29_34 Depth=1
	.loc	1 623 0                 # bzip2.c:623:0
	movq	-56(%rbp), %rcx
	movq	$1, %rsi
	movq	$5000, %rdx             # imm = 0x1388
	movq	%rbx, %rdi
	callq	fread
	.loc	1 624 0                 # bzip2.c:624:0
.Ltmp697:
	movq	-56(%rbp), %rdi
.Ltmp698:
	.loc	1 623 0                 # bzip2.c:623:0
	movl	%eax, -88(%rbp)
	.loc	1 624 0                 # bzip2.c:624:0
.Ltmp699:
	callq	ferror
	testq	%r12, %rax
	jne	.LBB29_13
.Ltmp700:
# BB#36:                                # %if.end108
                                        #   in Loop: Header=BB29_34 Depth=1
	.loc	1 625 0                 # bzip2.c:625:0
	movslq	-88(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB29_38
# BB#37:                                # %if.then111
                                        #   in Loop: Header=BB29_34 Depth=1
	movslq	-88(%rbp), %rdx
	movq	-64(%rbp), %rcx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	fwrite
.Ltmp701:
.LBB29_38:                              # %if.end115
                                        #   in Loop: Header=BB29_34 Depth=1
	.loc	1 626 0                 # bzip2.c:626:0
	movq	-64(%rbp), %rdi
	callq	ferror
	testq	%r12, %rax
	je	.LBB29_34
	jmp	.LBB29_13
.Ltmp702:
.LBB29_22:                              # %closeok
	.loc	1 598 0                 # bzip2.c:598:0
	movq	-56(%rbp), %rdi
	callq	ferror
	testq	%r12, %rax
	jne	.LBB29_13
.Ltmp703:
# BB#23:                                # %if.end66
	.loc	1 599 0                 # bzip2.c:599:0
	movq	-56(%rbp), %rdi
	callq	fclose
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	%r12, %rcx
	movl	%eax, -84(%rbp)
	je	.LBB29_13
# BB#24:                                # %if.end71
	.loc	1 602 0                 # bzip2.c:602:0
.Ltmp704:
	movq	-64(%rbp), %rdi
	callq	ferror
	testq	%r12, %rax
	jne	.LBB29_13
.Ltmp705:
# BB#25:                                # %if.end75
	.loc	1 603 0                 # bzip2.c:603:0
	movq	-64(%rbp), %rdi
	callq	fflush
	testq	%r12, %rax
	movl	%eax, -84(%rbp)
	jne	.LBB29_13
# BB#26:                                # %if.end80
	.loc	1 608 0                 # bzip2.c:608:0
.Ltmp706:
	movq	-64(%rbp), %rax
	movq	stdout(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB29_28
# BB#27:                                # %if.then83
	.loc	1 610 0                 # bzip2.c:610:0
.Ltmp707:
	movq	-64(%rbp), %rdi
	callq	fclose
	movl	%eax, -84(%rbp)
	.loc	1 612 0                 # bzip2.c:612:0
.Ltmp708:
	movl	-84(%rbp), %eax
.Ltmp709:
	.loc	1 611 0                 # bzip2.c:611:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 612 0                 # bzip2.c:612:0
.Ltmp710:
	cmpq	%r12, %rax
	je	.LBB29_13
.Ltmp711:
.LBB29_28:                              # %if.end89
	.loc	1 615 0                 # bzip2.c:615:0
	movslq	verbosity(%rip), %rax
.Ltmp712:
	.loc	1 614 0                 # bzip2.c:614:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 615 0                 # bzip2.c:615:0
.Ltmp713:
	cmpq	$2, %rax
	jl	.LBB29_30
# BB#29:                                # %if.then92
	movq	stderr(%rip), %rdi
	movabsq	$.L.str49, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB29_30
.Ltmp714:
.LBB29_39:                              # %errhandler
	.loc	1 632 0                 # bzip2.c:632:0
	movq	-72(%rbp), %rsi
	leaq	-80(%rbp), %rdi
	callq	BZ2_bzReadClose
	.loc	1 633 0                 # bzip2.c:633:0
	movl	-76(%rbp), %eax
	addq	$9, %rax
	andq	%r12, %rax
	cmpq	$6, %rax
	ja	.LBB29_53
# BB#40:                                # %errhandler
	jmpq	*.LJTI29_0(,%rax,8)
.LBB29_45:                              # %sw.bb126
	.loc	1 650 0                 # bzip2.c:650:0
.Ltmp715:
	movq	-56(%rbp), %rax
	movq	stdin(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB29_47
# BB#46:                                # %if.then129
	movq	-56(%rbp), %rdi
	callq	fclose
.Ltmp716:
.LBB29_47:                              # %if.end131
	.loc	1 651 0                 # bzip2.c:651:0
	movq	-64(%rbp), %rax
	movq	stdout(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB29_49
# BB#48:                                # %if.then134
	movq	-64(%rbp), %rdi
	callq	fclose
.Ltmp717:
.LBB29_49:                              # %if.end136
	.loc	1 653 0                 # bzip2.c:653:0
	movl	-92(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB29_51
# BB#50:                                # %if.then139
	xorq	%rax, %rax
	jmp	.LBB29_31
.LBB29_51:                              # %if.else
	.loc	1 656 0                 # bzip2.c:656:0
.Ltmp718:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_30
# BB#52:                                # %if.then141
	.loc	1 657 0                 # bzip2.c:657:0
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str85, %rsi
	movabsq	$inName, %rcx
	xorq	%rax, %rax
	callq	fprintf
.Ltmp719:
.LBB29_30:                              # %if.end94
	movq	$1, %rax
.LBB29_31:                              # %return
	.loc	1 616 0                 # bzip2.c:616:0
	movb	%al, -41(%rbp)
	.loc	1 668 0                 # bzip2.c:668:0
	movzbl	-41(%rbp), %eax
	addq	$10104, %rsp            # imm = 0x2778
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB29_53:                              # %sw.default
	.loc	1 663 0                 # bzip2.c:663:0
.Ltmp720:
	movabsq	$.L.str86, %rdi
	callq	panic
.Ltmp721:
.LBB29_54:                              # %if.then43
	.loc	1 586 0                 # bzip2.c:586:0
	movabsq	$.L.str84, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp722:
.LBB29_41:                              # %sw.bb
	.loc	1 635 0                 # bzip2.c:635:0
	callq	configError
.LBB29_44:                              # %sw.bb125
	.loc	1 644 0                 # bzip2.c:644:0
	callq	compressedStreamEOF
.LBB29_13:                              # %errhandler_io
	.loc	1 638 0                 # bzip2.c:638:0
	callq	ioError
.LBB29_42:                              # %sw.bb123
	.loc	1 640 0                 # bzip2.c:640:0
	callq	crcError
.LBB29_43:                              # %sw.bb124
	.loc	1 642 0                 # bzip2.c:642:0
	callq	outOfMemory
.Ltmp723:
.Ltmp724:
	.size	uncompressStream, .Ltmp724-uncompressStream
.Lfunc_end29:
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_41
	.quad	.LBB29_53
	.quad	.LBB29_44
	.quad	.LBB29_13
	.quad	.LBB29_45
	.quad	.LBB29_42
	.quad	.LBB29_43

	.text
	.align	16, 0x90
	.type	applySavedMetaInfoToOutputFile,@function
applySavedMetaInfoToOutputFile:         # @applySavedMetaInfoToOutputFile
.Lfunc_begin30:
	.loc	1 1193 0                # bzip2.c:1193:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp728:
	.cfi_def_cfa_offset 16
.Ltmp729:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp730:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
.Ltmp731:
	.cfi_offset %rbx, -24
	movq	%rdi, -16(%rbp)
	.loc	1 1198 0 prologue_end   # bzip2.c:1198:0
.Ltmp732:
	movq	fileMetaInfo+72(%rip), %rax
	.loc	1 1199 0                # bzip2.c:1199:0
	movq	fileMetaInfo+88(%rip), %rcx
	.loc	1 1201 0                # bzip2.c:1201:0
	movq	-16(%rbp), %rdi
	movl	fileMetaInfo+24(%rip), %esi
	.loc	1 1198 0                # bzip2.c:1198:0
	movq	%rax, -40(%rbp)
	.loc	1 1199 0                # bzip2.c:1199:0
	movq	%rcx, -32(%rbp)
	.loc	1 1201 0                # bzip2.c:1201:0
	callq	chmod
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movl	%eax, -20(%rbp)
	jne	.LBB30_3
# BB#1:                                 # %if.end
	.loc	1 1204 0                # bzip2.c:1204:0
	movq	-16(%rbp), %rdi
	leaq	-40(%rbp), %rsi
	callq	utime
	testq	%rbx, %rax
	movl	%eax, -20(%rbp)
	jne	.LBB30_3
# BB#2:                                 # %if.end4
	.loc	1 1207 0                # bzip2.c:1207:0
	movq	-16(%rbp), %rdi
	movl	fileMetaInfo+32(%rip), %edx
	movl	fileMetaInfo+28(%rip), %esi
	callq	chown
	movl	%eax, -20(%rbp)
	.loc	1 1212 0                # bzip2.c:1212:0
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB30_3:                               # %if.then3
	.loc	1 1205 0                # bzip2.c:1205:0
.Ltmp733:
	callq	ioError
.Ltmp734:
.Ltmp735:
	.size	applySavedMetaInfoToOutputFile, .Ltmp735-applySavedMetaInfoToOutputFile
.Lfunc_end30:
	.file	4 "../musl-1.0.4/include/utime.h"
	.cfi_endproc

	.align	16, 0x90
	.type	crcError,@function
crcError:                               # @crcError
.Lfunc_begin31:
	.loc	1 900 0                 # bzip2.c:900:0
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
	.loc	1 901 0 prologue_end    # bzip2.c:901:0
.Ltmp741:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str89, %rsi
	xorq	%rax, %rax
	callq	fprintf
	.loc	1 904 0                 # bzip2.c:904:0
	callq	showFileNames
	.loc	1 905 0                 # bzip2.c:905:0
	callq	cadvise
	movq	$2, %rdi
	.loc	1 906 0                 # bzip2.c:906:0
	callq	cleanUpAndFail
.Ltmp742:
.Ltmp743:
	.size	crcError, .Ltmp743-crcError
.Lfunc_end31:
	.cfi_endproc

	.align	16, 0x90
	.type	compressedStreamEOF,@function
compressedStreamEOF:                    # @compressedStreamEOF
.Lfunc_begin32:
	.loc	1 913 0                 # bzip2.c:913:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp746:
	.cfi_def_cfa_offset 16
.Ltmp747:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp748:
	.cfi_def_cfa_register %rbp
	.loc	1 914 0 prologue_end    # bzip2.c:914:0
.Ltmp749:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB32_2
# BB#1:                                 # %if.then
	.loc	1 915 0                 # bzip2.c:915:0
.Ltmp750:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movabsq	$.L.str87, %rsi
	xorq	%rax, %rax
	callq	fprintf
	.loc	1 919 0                 # bzip2.c:919:0
	movq	progName(%rip), %rdi
	callq	perror
	.loc	1 920 0                 # bzip2.c:920:0
	callq	showFileNames
	.loc	1 921 0                 # bzip2.c:921:0
	callq	cadvise
.Ltmp751:
.LBB32_2:                               # %if.end
	movq	$2, %rdi
	.loc	1 923 0                 # bzip2.c:923:0
	callq	cleanUpAndFail
.Ltmp752:
.Ltmp753:
	.size	compressedStreamEOF, .Ltmp753-compressedStreamEOF
.Lfunc_end32:
	.cfi_endproc

	.align	16, 0x90
	.type	cadvise,@function
cadvise:                                # @cadvise
.Lfunc_begin33:
	.loc	1 791 0                 # bzip2.c:791:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp756:
	.cfi_def_cfa_offset 16
.Ltmp757:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp758:
	.cfi_def_cfa_register %rbp
	.loc	1 792 0 prologue_end    # bzip2.c:792:0
.Ltmp759:
	movzbl	noisy(%rip), %eax
	testq	%rax, %rax
	je	.LBB33_2
# BB#1:                                 # %if.then
	.loc	1 793 0                 # bzip2.c:793:0
	movq	stderr(%rip), %rdi
	movabsq	$.L.str88, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp760:
.LBB33_2:                               # %if.end
	.loc	1 800 0                 # bzip2.c:800:0
	popq	%rbp
	retq
.Ltmp761:
.Ltmp762:
	.size	cadvise, .Ltmp762-cadvise
.Lfunc_end33:
	.cfi_endproc

	.align	16, 0x90
	.type	hasSuffix,@function
hasSuffix:                              # @hasSuffix
.Lfunc_begin34:
	.loc	1 1245 0                # bzip2.c:1245:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp765:
	.cfi_def_cfa_offset 16
.Ltmp766:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp767:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 1246 0 prologue_end   # bzip2.c:1246:0
.Ltmp768:
	movq	-16(%rbp), %rdi
	movq	%rsi, -24(%rbp)
	callq	strlen
	.loc	1 1247 0                # bzip2.c:1247:0
	movq	-24(%rbp), %rdi
	.loc	1 1246 0                # bzip2.c:1246:0
	movl	%eax, -28(%rbp)
	.loc	1 1247 0                # bzip2.c:1247:0
	callq	strlen
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	.loc	1 1248 0                # bzip2.c:1248:0
.Ltmp769:
	movslq	-28(%rbp), %rsi
	movq	$32, %rcx
.Ltmp770:
	.loc	1 1247 0                # bzip2.c:1247:0
	sarq	%cl, %rdx
	movl	%eax, -32(%rbp)
	.loc	1 1248 0                # bzip2.c:1248:0
.Ltmp771:
	cmpq	%rdx, %rsi
	jl	.LBB34_3
.Ltmp772:
# BB#1:                                 # %if.end
	.loc	1 1249 0                # bzip2.c:1249:0
	movq	-16(%rbp), %rax
	movslq	-28(%rbp), %rdi
	movslq	-32(%rbp), %rcx
	movq	-24(%rbp), %rsi
	addq	%rax, %rdi
	subq	%rcx, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB34_2
.Ltmp773:
.LBB34_3:                               # %if.end10
	xorq	%rax, %rax
	jmp	.LBB34_4
.LBB34_2:                               # %if.then9
	movq	$1, %rax
.LBB34_4:                               # %return
	.loc	1 1250 0                # bzip2.c:1250:0
	movb	%al, -1(%rbp)
	.loc	1 1251 0                # bzip2.c:1251:0
	movzbl	-1(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp774:
.Ltmp775:
	.size	hasSuffix, .Ltmp775-hasSuffix
.Lfunc_end34:
	.cfi_endproc

	.align	16, 0x90
	.type	compressStream,@function
compressStream:                         # @compressStream
.Lfunc_begin35:
	.loc	1 440 0                 # bzip2.c:440:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp779:
	.cfi_def_cfa_offset 16
.Ltmp780:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp781:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$5176, %rsp             # imm = 0x1438
.Ltmp782:
	.cfi_offset %rbx, -56
.Ltmp783:
	.cfi_offset %r12, -48
.Ltmp784:
	.cfi_offset %r13, -40
.Ltmp785:
	.cfi_offset %r14, -32
.Ltmp786:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	.loc	1 451 0 prologue_end    # bzip2.c:451:0
.Ltmp787:
	movq	-48(%rbp), %rdi
	movq	%rsi, -56(%rbp)
.Ltmp788:
	.loc	1 441 0                 # bzip2.c:441:0
	movq	$0, -64(%rbp)
	.loc	1 451 0                 # bzip2.c:451:0
.Ltmp789:
	callq	ferror
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB35_28
.Ltmp790:
# BB#1:                                 # %if.end
	.loc	1 452 0                 # bzip2.c:452:0
	movq	-56(%rbp), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB35_28
.Ltmp791:
# BB#2:                                 # %if.end4
	.loc	1 454 0                 # bzip2.c:454:0
	movq	-56(%rbp), %rsi
	movl	workFactor(%rip), %r8d
	movl	verbosity(%rip), %ecx
	movl	blockSize100k(%rip), %edx
	leaq	-5096(%rbp), %rdi
	callq	BZ2_bzWriteOpen
	.loc	1 456 0                 # bzip2.c:456:0
.Ltmp792:
	movl	-5096(%rbp), %ecx
.Ltmp793:
	.loc	1 454 0                 # bzip2.c:454:0
	movq	%rax, -64(%rbp)
	.loc	1 456 0                 # bzip2.c:456:0
.Ltmp794:
	testq	%rcx, %rcx
	jne	.LBB35_10
.Ltmp795:
# BB#3:                                 # %if.end7
	.loc	1 458 0                 # bzip2.c:458:0
	movslq	verbosity(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB35_5
# BB#4:                                 # %if.then9
	movq	stderr(%rip), %rdi
	movabsq	$.L.str94, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp796:
	.align	16, 0x90
.LBB35_5:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 462 0                 # bzip2.c:462:0
	movq	-48(%rbp), %rdi
	callq	myfeof
	testq	%rax, %rax
	jne	.LBB35_14
.Ltmp797:
# BB#6:                                 # %if.end15
                                        #   in Loop: Header=BB35_5 Depth=1
	.loc	1 463 0                 # bzip2.c:463:0
	movq	-48(%rbp), %rcx
	leaq	-5072(%rbp), %rdi
	movq	$1, %rsi
	movq	$5000, %rdx             # imm = 0x1388
	callq	fread
	.loc	1 464 0                 # bzip2.c:464:0
.Ltmp798:
	movq	-48(%rbp), %rdi
.Ltmp799:
	.loc	1 463 0                 # bzip2.c:463:0
	movl	%eax, -5076(%rbp)
	.loc	1 464 0                 # bzip2.c:464:0
.Ltmp800:
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB35_28
.Ltmp801:
# BB#7:                                 # %if.end20
                                        #   in Loop: Header=BB35_5 Depth=1
	.loc	1 465 0                 # bzip2.c:465:0
	movslq	-5076(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB35_9
# BB#8:                                 # %if.then23
                                        #   in Loop: Header=BB35_5 Depth=1
	movq	-64(%rbp), %rsi
	movl	-5076(%rbp), %ecx
	leaq	-5096(%rbp), %rdi
	leaq	-5072(%rbp), %rdx
	callq	BZ2_bzWrite
.Ltmp802:
.LBB35_9:                               # %if.end25
                                        #   in Loop: Header=BB35_5 Depth=1
	.loc	1 466 0                 # bzip2.c:466:0
	movl	-5096(%rbp), %eax
	testq	%rax, %rax
	je	.LBB35_5
	jmp	.LBB35_10
.Ltmp803:
.LBB35_14:                              # %while.end
	.loc	1 470 0                 # bzip2.c:470:0
	movq	-64(%rbp), %rsi
	leaq	-5092(%rbp), %rax
	leaq	-5096(%rbp), %rdi
	leaq	-5080(%rbp), %rcx
	leaq	-5084(%rbp), %r8
	leaq	-5088(%rbp), %r9
	xorq	%rdx, %rdx
	movq	%rax, (%rsp)
	callq	BZ2_bzWriteClose64
	.loc	1 473 0                 # bzip2.c:473:0
.Ltmp804:
	movl	-5096(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB35_10
.Ltmp805:
# BB#15:                                # %if.end33
	.loc	1 475 0                 # bzip2.c:475:0
	movq	-56(%rbp), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB35_28
.Ltmp806:
# BB#16:                                # %if.end37
	.loc	1 476 0                 # bzip2.c:476:0
	movq	-56(%rbp), %rdi
	callq	fflush
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	%rbx, %rcx
	movl	%eax, -5104(%rbp)
	je	.LBB35_28
# BB#17:                                # %if.end42
	.loc	1 481 0                 # bzip2.c:481:0
.Ltmp807:
	movq	-56(%rbp), %rax
	movq	stdout(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB35_19
# BB#18:                                # %if.then45
	.loc	1 483 0                 # bzip2.c:483:0
.Ltmp808:
	movq	-56(%rbp), %rdi
	callq	fclose
	movl	%eax, -5104(%rbp)
	.loc	1 485 0                 # bzip2.c:485:0
.Ltmp809:
	movl	-5104(%rbp), %eax
.Ltmp810:
	.loc	1 484 0                 # bzip2.c:484:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 485 0                 # bzip2.c:485:0
.Ltmp811:
	cmpq	%rbx, %rax
	je	.LBB35_28
.Ltmp812:
.LBB35_19:                              # %if.end51
	.loc	1 488 0                 # bzip2.c:488:0
	movq	-48(%rbp), %rdi
.Ltmp813:
	.loc	1 487 0                 # bzip2.c:487:0
	movq	$0, outputHandleJustInCase(%rip)
	.loc	1 488 0                 # bzip2.c:488:0
.Ltmp814:
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB35_28
.Ltmp815:
# BB#20:                                # %if.end55
	.loc	1 489 0                 # bzip2.c:489:0
	movq	-48(%rbp), %rdi
	callq	fclose
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	%rbx, %rcx
	movl	%eax, -5104(%rbp)
	je	.LBB35_28
# BB#21:                                # %if.end60
	.loc	1 492 0                 # bzip2.c:492:0
.Ltmp816:
	movslq	verbosity(%rip), %rax
	testq	%rax, %rax
	jle	.LBB35_26
# BB#22:                                # %if.then63
	.loc	1 493 0                 # bzip2.c:493:0
.Ltmp817:
	movl	-5080(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB35_25
# BB#23:                                # %land.lhs.true
	movl	-5084(%rbp), %eax
	testq	%rax, %rax
	je	.LBB35_24
.LBB35_25:                              # %if.else
.Ltmp818:
	#DEBUG_VALUE: buf_nin <- [RBP+-5136]
	#DEBUG_VALUE: buf_nout <- [RBP+-5168]
	#DEBUG_VALUE: nbytes_in <- [RBP+-5176]
	#DEBUG_VALUE: nbytes_out <- [RBP+-5184]
	#DEBUG_VALUE: nbytes_in_d <- [RBP+-5192]
	#DEBUG_VALUE: nbytes_out_d <- [RBP+-5200]
	.loc	1 499 0                 # bzip2.c:499:0
	movl	-5084(%rbp), %edx
	movl	-5080(%rbp), %esi
	leaq	-5176(%rbp), %r15
	movq	%r15, %rdi
	callq	uInt64_from_UInt32s
	.loc	1 501 0                 # bzip2.c:501:0
	movl	-5092(%rbp), %edx
	movl	-5088(%rbp), %esi
	leaq	-5184(%rbp), %rbx
	movq	%rbx, %rdi
	callq	uInt64_from_UInt32s
	.loc	1 503 0                 # bzip2.c:503:0
	movq	%r15, %rdi
	callq	uInt64_to_double
	movq	%rax, -5192(%rbp)
	.loc	1 504 0                 # bzip2.c:504:0
	movq	%rbx, %rdi
	callq	uInt64_to_double
	movq	%rax, -5200(%rbp)
	leaq	-5136(%rbp), %rdi
	.loc	1 505 0                 # bzip2.c:505:0
	movq	%r15, %rsi
	callq	uInt64_toAscii
	leaq	-5168(%rbp), %rdi
	movq	%rdi, %r12
	.loc	1 506 0                 # bzip2.c:506:0
	movq	%rbx, %rsi
	callq	uInt64_toAscii
	.loc	1 507 0                 # bzip2.c:507:0
	movq	-5192(%rbp), %rbx
	movq	-5200(%rbp), %r15
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_div
	movq	%rax, %r13
	movabsq	$4620693217682128896, %rsi # imm = 0x4020000000000000
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%rax, %rsi
	callq	float64_sub
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rbx
	movabsq	$.L.str96, %rsi
	xorq	%rax, %rax
	movq	%r12, (%rsp)
	movq	stderr(%rip), %rdi
	movq	%r13, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	leaq	-5136(%rbp), %r9
	callq	fprintf
.Ltmp819:
.LBB35_26:                              # %if.end81
	.loc	1 518 0                 # bzip2.c:518:0
	addq	$5176, %rsp             # imm = 0x1438
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB35_24:                              # %if.then68
	.loc	1 494 0                 # bzip2.c:494:0
.Ltmp820:
	movq	stderr(%rip), %rdi
	movabsq	$.L.str95, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB35_26
.Ltmp821:
.LBB35_10:                              # %errhandler
	.loc	1 521 0                 # bzip2.c:521:0
	movq	-64(%rbp), %rax
	leaq	-5092(%rbp), %rcx
	leaq	-5100(%rbp), %rsi
	leaq	-5080(%rbp), %rbx
	leaq	-5084(%rbp), %r8
	leaq	-5088(%rbp), %r9
	movq	$1, %rdx
	movq	%rcx, (%rsp)
	movq	%rsi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rcx
	callq	BZ2_bzWriteClose64
	.loc	1 524 0                 # bzip2.c:524:0
	movl	-5096(%rbp), %eax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	cmpq	%rcx, %rax
	je	.LBB35_27
# BB#11:                                # %errhandler
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	cmpq	%rcx, %rax
	jne	.LBB35_12
.LBB35_28:                              # %errhandler_io
	.loc	1 531 0                 # bzip2.c:531:0
.Ltmp822:
	callq	ioError
.LBB35_27:                              # %sw.bb82
	.loc	1 528 0                 # bzip2.c:528:0
	callq	outOfMemory
.LBB35_12:                              # %errhandler
	movabsq	$4294967287, %rcx       # imm = 0xFFFFFFF7
	cmpq	%rcx, %rax
	jne	.LBB35_29
# BB#13:                                # %sw.bb
	.loc	1 526 0                 # bzip2.c:526:0
	callq	configError
.LBB35_29:                              # %sw.default
	.loc	1 533 0                 # bzip2.c:533:0
	movabsq	$.L.str97, %rax
	movq	%rax, %rdi
	callq	panic
.Ltmp823:
.Ltmp824:
	.size	compressStream, .Ltmp824-compressStream
.Lfunc_end35:
	.cfi_endproc

	.align	16, 0x90
	.type	uInt64_from_UInt32s,@function
uInt64_from_UInt32s:                    # @uInt64_from_UInt32s
.Lfunc_begin36:
	.loc	1 341 0                 # bzip2.c:341:0
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
	movq	%rdi, -8(%rbp)
	movl	%edx, -16(%rbp)
	.loc	1 342 0 prologue_end    # bzip2.c:342:0
.Ltmp830:
	movzbl	-13(%rbp), %eax
	movq	-8(%rbp), %rcx
	movl	%esi, -12(%rbp)
	movb	%al, 7(%rcx)
	.loc	1 343 0                 # bzip2.c:343:0
	movzbl	-14(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 6(%rcx)
	.loc	1 344 0                 # bzip2.c:344:0
	movzbl	-15(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 5(%rcx)
	.loc	1 345 0                 # bzip2.c:345:0
	movzbl	-16(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 4(%rcx)
	.loc	1 346 0                 # bzip2.c:346:0
	movzbl	-9(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 3(%rcx)
	.loc	1 347 0                 # bzip2.c:347:0
	movzbl	-10(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 2(%rcx)
	.loc	1 348 0                 # bzip2.c:348:0
	movzbl	-11(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, 1(%rcx)
	.loc	1 349 0                 # bzip2.c:349:0
	movzbl	-12(%rbp), %eax
	movq	-8(%rbp), %rcx
	movb	%al, (%rcx)
	.loc	1 350 0                 # bzip2.c:350:0
	popq	%rbp
	retq
.Ltmp831:
.Ltmp832:
	.size	uInt64_from_UInt32s, .Ltmp832-uInt64_from_UInt32s
.Lfunc_end36:
	.cfi_endproc

	.align	16, 0x90
	.type	uInt64_to_double,@function
uInt64_to_double:                       # @uInt64_to_double
.Lfunc_begin37:
	.loc	1 355 0                 # bzip2.c:355:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp836:
	.cfi_def_cfa_offset 16
.Ltmp837:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp838:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$32, %rsp
.Ltmp839:
	.cfi_offset %rbx, -32
.Ltmp840:
	.cfi_offset %r14, -24
	movq	%rdi, -24(%rbp)
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	.loc	1 357 0 prologue_end    # bzip2.c:357:0
.Ltmp841:
	movq	%rax, -40(%rbp)
	.loc	1 358 0                 # bzip2.c:358:0
	movq	$0, -48(%rbp)
	xorq	%rax, %rax
	.loc	1 359 0                 # bzip2.c:359:0
.Ltmp842:
	movl	%eax, -28(%rbp)
	movabsq	$4643211215818981376, %r14 # imm = 0x4070000000000000
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_2:                               # %for.inc
                                        #   in Loop: Header=BB37_1 Depth=1
	.loc	1 360 0                 # bzip2.c:360:0
.Ltmp843:
	movslq	-28(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	-40(%rbp), %rbx
	movzbl	(%rcx,%rax), %edi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	-48(%rbp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	.loc	1 361 0                 # bzip2.c:361:0
	movq	-40(%rbp), %rdi
	.loc	1 360 0                 # bzip2.c:360:0
	movq	%rax, -48(%rbp)
	.loc	1 361 0                 # bzip2.c:361:0
	movq	%r14, %rsi
	callq	float64_mul
.Ltmp844:
	.loc	1 359 0                 # bzip2.c:359:0
	movl	-28(%rbp), %ecx
	.loc	1 361 0                 # bzip2.c:361:0
.Ltmp845:
	movq	%rax, -40(%rbp)
.Ltmp846:
	.loc	1 359 0                 # bzip2.c:359:0
	incq	%rcx
	movl	%ecx, -28(%rbp)
.LBB37_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-28(%rbp), %rax
	cmpq	$7, %rax
	jle	.LBB37_2
.Ltmp847:
# BB#3:                                 # %for.end
	.loc	1 363 0                 # bzip2.c:363:0
	movq	-48(%rbp), %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Ltmp848:
.Ltmp849:
	.size	uInt64_to_double, .Ltmp849-uInt64_to_double
.Lfunc_end37:
	.cfi_endproc

	.align	16, 0x90
	.type	uInt64_toAscii,@function
uInt64_toAscii:                         # @uInt64_toAscii
.Lfunc_begin38:
	.loc	1 398 0                 # bzip2.c:398:0
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
	pushq	%rbx
	subq	$72, %rsp
.Ltmp856:
	.cfi_offset %rbx, -24
	movq	%rsi, -24(%rbp)
	.loc	1 402 0 prologue_end    # bzip2.c:402:0
.Ltmp857:
	movq	-24(%rbp), %rax
	movq	%rdi, -16(%rbp)
	xorq	%rcx, %rcx
	.loc	1 401 0                 # bzip2.c:401:0
	movl	%ecx, -68(%rbp)
	.loc	1 402 0                 # bzip2.c:402:0
	movq	(%rax), %rax
	movq	%rax, -80(%rbp)
	leaq	-80(%rbp), %rbx
	.align	16, 0x90
.LBB38_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 404 0                 # bzip2.c:404:0
.Ltmp858:
	movq	%rbx, %rdi
	callq	uInt64_qrm10
	movl	%eax, -32(%rbp)
	.loc	1 406 0                 # bzip2.c:406:0
	movl	-68(%rbp), %ecx
	.loc	1 405 0                 # bzip2.c:405:0
	movslq	-68(%rbp), %rdx
	addq	$48, %rax
	movb	%al, -64(%rbp,%rdx)
	.loc	1 406 0                 # bzip2.c:406:0
	incq	%rcx
	movl	%ecx, -68(%rbp)
.Ltmp859:
	.loc	1 407 0                 # bzip2.c:407:0
	movq	%rbx, %rdi
	callq	uInt64_isZero
	testq	%rax, %rax
	je	.LBB38_1
# BB#2:                                 # %do.end
	.loc	1 408 0                 # bzip2.c:408:0
	movslq	-68(%rbp), %rax
	movq	-16(%rbp), %rcx
	xorq	%rdx, %rdx
	movb	%dl, (%rcx,%rax)
	.loc	1 409 0                 # bzip2.c:409:0
.Ltmp860:
	movl	%edx, -28(%rbp)
	leaq	-64(%rbp), %rax
	jmp	.LBB38_3
	.align	16, 0x90
.LBB38_4:                               # %for.body
                                        #   in Loop: Header=BB38_3 Depth=1
	.loc	1 410 0                 # bzip2.c:410:0
	movl	-68(%rbp), %edx
	movslq	-28(%rbp), %rsi
	subq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	-1(%rdx,%rax), %ecx
	movq	-16(%rbp), %rdx
	movb	%cl, (%rdx,%rsi)
	.loc	1 409 0                 # bzip2.c:409:0
	movl	-28(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -28(%rbp)
.LBB38_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-28(%rbp), %rcx
	movslq	-68(%rbp), %rdx
	cmpq	%rdx, %rcx
	jl	.LBB38_4
.Ltmp861:
# BB#5:                                 # %for.end
	.loc	1 411 0                 # bzip2.c:411:0
	addq	$72, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp862:
.Ltmp863:
	.size	uInt64_toAscii, .Ltmp863-uInt64_toAscii
.Lfunc_end38:
	.cfi_endproc

	.align	16, 0x90
	.type	uInt64_qrm10,@function
uInt64_qrm10:                           # @uInt64_qrm10
.Lfunc_begin39:
	.loc	1 380 0                 # bzip2.c:380:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp866:
	.cfi_def_cfa_offset 16
.Ltmp867:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp868:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 383 0 prologue_end    # bzip2.c:383:0
.Ltmp869:
	movl	%eax, -12(%rbp)
	movq	$7, %rax
	.loc	1 384 0                 # bzip2.c:384:0
.Ltmp870:
	movl	%eax, -20(%rbp)
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movabsq	$-3689348814741910323, %r10 # imm = 0xCCCCCCCCCCCCCCCD
	movabsq	$4611686018427387902, %r9 # imm = 0x3FFFFFFFFFFFFFFE
	jmp	.LBB39_1
	.align	16, 0x90
.LBB39_2:                               # %for.inc
                                        #   in Loop: Header=BB39_1 Depth=1
	.loc	1 385 0                 # bzip2.c:385:0
.Ltmp871:
	movslq	-20(%rbp), %rax
	movq	-8(%rbp), %rcx
	movl	-12(%rbp), %edx
	movzbl	(%rcx,%rax), %eax
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rax
	movl	%eax, -16(%rbp)
	andq	%r8, %rax
	.loc	1 386 0                 # bzip2.c:386:0
	movslq	-20(%rbp), %rsi
	movq	-8(%rbp), %rdi
	mulq	%r10
	movq	$3, %rcx
	shrq	%cl, %rdx
	movb	%dl, (%rdi,%rsi)
	.loc	1 387 0                 # bzip2.c:387:0
	movl	-16(%rbp), %esi
	movq	%rsi, %rax
	mulq	%r10
	movq	$2, %rcx
	shrq	%cl, %rdx
	andq	%r9, %rdx
.Ltmp872:
	.loc	1 384 0                 # bzip2.c:384:0
	movl	-20(%rbp), %eax
	.loc	1 387 0                 # bzip2.c:387:0
.Ltmp873:
	leaq	(%rdx,%rdx,4), %rcx
	subq	%rcx, %rsi
	movl	%esi, -12(%rbp)
.Ltmp874:
	.loc	1 384 0                 # bzip2.c:384:0
	decq	%rax
	movl	%eax, -20(%rbp)
.LBB39_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-20(%rbp), %rax
	testq	%rax, %rax
	jns	.LBB39_2
.Ltmp875:
# BB#3:                                 # %for.end
	.loc	1 389 0                 # bzip2.c:389:0
	movl	-12(%rbp), %eax
	popq	%rbp
	retq
.Ltmp876:
.Ltmp877:
	.size	uInt64_qrm10, .Ltmp877-uInt64_qrm10
.Lfunc_end39:
	.cfi_endproc

	.align	16, 0x90
	.type	uInt64_isZero,@function
uInt64_isZero:                          # @uInt64_isZero
.Lfunc_begin40:
	.loc	1 369 0                 # bzip2.c:369:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp880:
	.cfi_def_cfa_offset 16
.Ltmp881:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp882:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -16(%rbp)
	xorq	%rax, %rax
	jmp	.LBB40_1
	.align	16, 0x90
.LBB40_4:                               # %for.inc
                                        #   in Loop: Header=BB40_1 Depth=1
	.loc	1 371 0 prologue_end    # bzip2.c:371:0
.Ltmp883:
	movl	-20(%rbp), %eax
	incq	%rax
.LBB40_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -20(%rbp)
	movslq	-20(%rbp), %rax
	cmpq	$7, %rax
	jg	.LBB40_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB40_1 Depth=1
	.loc	1 372 0                 # bzip2.c:372:0
.Ltmp884:
	movslq	-20(%rbp), %rax
	movq	-16(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	testq	%rax, %rax
	je	.LBB40_4
.Ltmp885:
# BB#3:                                 # %if.then
	xorq	%rax, %rax
	jmp	.LBB40_6
.LBB40_5:                               # %for.end
	movq	$1, %rax
.LBB40_6:                               # %return
	.loc	1 373 0                 # bzip2.c:373:0
	movb	%al, -1(%rbp)
	.loc	1 374 0                 # bzip2.c:374:0
	movzbl	-1(%rbp), %eax
	popq	%rbp
	retq
.Ltmp886:
.Ltmp887:
	.size	uInt64_isZero, .Ltmp887-uInt64_isZero
.Lfunc_end40:
	.cfi_endproc

	.align	16, 0x90
	.type	mkCell,@function
mkCell:                                 # @mkCell
.Lfunc_begin41:
	.loc	1 1850 0                # bzip2.c:1850:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp890:
	.cfi_def_cfa_offset 16
.Ltmp891:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp892:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	$16, %rdi
	.loc	1 1853 0 prologue_end   # bzip2.c:1853:0
.Ltmp893:
	callq	myMalloc
	movq	%rax, -8(%rbp)
	.loc	1 1854 0                # bzip2.c:1854:0
	movq	$0, (%rax)
	.loc	1 1855 0                # bzip2.c:1855:0
	movq	-8(%rbp), %rax
	movq	$0, 8(%rax)
	.loc	1 1856 0                # bzip2.c:1856:0
	movq	-8(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp894:
.Ltmp895:
	.size	mkCell, .Ltmp895-mkCell
.Lfunc_end41:
	.cfi_endproc

	.align	16, 0x90
	.type	myMalloc,@function
myMalloc:                               # @myMalloc
.Lfunc_begin42:
	.loc	1 1838 0                # bzip2.c:1838:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp898:
	.cfi_def_cfa_offset 16
.Ltmp899:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp900:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	.loc	1 1841 0 prologue_end   # bzip2.c:1841:0
.Ltmp901:
	movslq	-4(%rbp), %rdi
	callq	malloc
	testq	%rax, %rax
	movq	%rax, -16(%rbp)
	.loc	1 1842 0                # bzip2.c:1842:0
.Ltmp902:
	je	.LBB42_2
.Ltmp903:
# BB#1:                                 # %if.end
	.loc	1 1843 0                # bzip2.c:1843:0
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB42_2:                               # %if.then
	.loc	1 1842 0                # bzip2.c:1842:0
.Ltmp904:
	callq	outOfMemory
.Ltmp905:
.Ltmp906:
	.size	myMalloc, .Ltmp906-myMalloc
.Lfunc_end42:
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	".bz2"
	.size	.L.str, 5

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	".bz"
	.size	.L.str1, 4

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	".tbz2"
	.size	.L.str2, 6

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	".tbz"
	.size	.L.str3, 5

	.type	zSuffix,@object         # @zSuffix
	.data
	.globl	zSuffix
	.align	16
zSuffix:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.size	zSuffix, 32

	.type	.L.str4,@object         # @.str4
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str4:
	.zero	1
	.size	.L.str4, 1

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	".tar"
	.size	.L.str5, 5

	.type	unzSuffix,@object       # @unzSuffix
	.data
	.globl	unzSuffix
	.align	16
unzSuffix:
	.quad	.L.str4
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str5
	.size	unzSuffix, 32

	.type	outputHandleJustInCase,@object # @outputHandleJustInCase
	.comm	outputHandleJustInCase,8,8
	.type	smallMode,@object       # @smallMode
	.comm	smallMode,1,1
	.type	keepInputFiles,@object  # @keepInputFiles
	.comm	keepInputFiles,1,1
	.type	forceOverwrite,@object  # @forceOverwrite
	.comm	forceOverwrite,1,1
	.type	noisy,@object           # @noisy
	.comm	noisy,1,1
	.type	verbosity,@object       # @verbosity
	.comm	verbosity,4,4
	.type	blockSize100k,@object   # @blockSize100k
	.comm	blockSize100k,4,4
	.type	testFailsExist,@object  # @testFailsExist
	.comm	testFailsExist,1,1
	.type	unzFailsExist,@object   # @unzFailsExist
	.comm	unzFailsExist,1,1
	.type	numFileNames,@object    # @numFileNames
	.comm	numFileNames,4,4
	.type	numFilesProcessed,@object # @numFilesProcessed
	.comm	numFilesProcessed,4,4
	.type	workFactor,@object      # @workFactor
	.comm	workFactor,4,4
	.type	deleteOutputOnInterrupt,@object # @deleteOutputOnInterrupt
	.comm	deleteOutputOnInterrupt,1,1
	.type	exitValue,@object       # @exitValue
	.comm	exitValue,4,4
	.type	inName,@object          # @inName
	.comm	inName,1034,16
	.type	.L.str6,@object         # @.str6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str6:
	.asciz	"(none)"
	.size	.L.str6, 7

	.type	outName,@object         # @outName
	.comm	outName,1034,16
	.type	progNameReally,@object  # @progNameReally
	.comm	progNameReally,1034,16
	.type	progName,@object        # @progName
	.comm	progName,8,8
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"BZIP2"
	.size	.L.str7, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"BZIP"
	.size	.L.str8, 5

	.type	longestFileName,@object # @longestFileName
	.comm	longestFileName,4,4
	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"--"
	.size	.L.str9, 3

	.type	srcMode,@object         # @srcMode
	.comm	srcMode,4,4
	.type	opMode,@object          # @opMode
	.comm	opMode,4,4
	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"unzip"
	.size	.L.str10, 6

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"UNZIP"
	.size	.L.str11, 6

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"z2cat"
	.size	.L.str12, 6

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Z2CAT"
	.size	.L.str13, 6

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"zcat"
	.size	.L.str14, 5

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"ZCAT"
	.size	.L.str15, 5

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"%s: Bad flag `%s'\n"
	.size	.L.str16, 19

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"--stdout"
	.size	.L.str17, 9

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"--decompress"
	.size	.L.str18, 13

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"--compress"
	.size	.L.str19, 11

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"--force"
	.size	.L.str20, 8

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"--test"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"--keep"
	.size	.L.str22, 7

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"--small"
	.size	.L.str23, 8

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"--quiet"
	.size	.L.str24, 8

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"--version"
	.size	.L.str25, 10

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"--license"
	.size	.L.str26, 10

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"--exponential"
	.size	.L.str27, 14

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"--repetitive-best"
	.size	.L.str28, 18

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"--repetitive-fast"
	.size	.L.str29, 18

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"--fast"
	.size	.L.str30, 7

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"--best"
	.size	.L.str31, 7

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"--verbose"
	.size	.L.str32, 10

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"--help"
	.size	.L.str33, 7

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"%s: -c and -t cannot be used together.\n"
	.size	.L.str34, 40

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"\nYou can use the `bzip2recover' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n"
	.size	.L.str35, 113

	.type	tmpName,@object         # @tmpName
	.comm	tmpName,1034,16
	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"testf: bad modes\n"
	.size	.L.str36, 18

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"(stdin)"
	.size	.L.str37, 8

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"%s: There are no files matching `%s'.\n"
	.size	.L.str38, 39

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"%s: Can't open input %s: %s.\n"
	.size	.L.str39, 30

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"%s: Input file %s is a directory.\n"
	.size	.L.str40, 35

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"%s: I won't read compressed data from a terminal.\n"
	.size	.L.str41, 51

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"%s: For help, type: `%s --help'.\n"
	.size	.L.str42, 34

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"rb"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"%s: Can't open input file %s:%s.\n"
	.size	.L.str44, 34

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"testf: bad srcMode"
	.size	.L.str45, 19

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"  %s: "
	.size	.L.str46, 7

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"ok\n"
	.size	.L.str47, 4

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"test:bzReadGetUnused"
	.size	.L.str48, 21

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"\n    "
	.size	.L.str49, 6

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"%s: %s: "
	.size	.L.str50, 9

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"data integrity (CRC) error in data\n"
	.size	.L.str51, 36

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"file ends unexpectedly\n"
	.size	.L.str52, 24

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"bad magic number (file not created by bzip2)\n"
	.size	.L.str53, 46

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"trailing garbage after EOF ignored\n"
	.size	.L.str54, 36

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"test:unexpected error"
	.size	.L.str55, 22

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"\n%s: couldn't allocate enough memory\n"
	.size	.L.str56, 38

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"%s: Deleting output file %s, if it exists.\n"
	.size	.L.str57, 44

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"%s: WARNING: deletion of output file (apparently) failed.\n"
	.size	.L.str58, 59

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"%s: WARNING: deletion of output file suppressed\n"
	.size	.L.str59, 49

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"%s:    since input file no longer exists.  Output file\n"
	.size	.L.str60, 56

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"%s:    `%s' may be incomplete.\n"
	.size	.L.str61, 32

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"%s:    I suggest doing an integrity test (bzip2 -tv) of it.\n"
	.size	.L.str62, 61

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"%s: WARNING: some files have not been processed:\n%s:    %d specified on command line, %d not processed yet.\n\n"
	.size	.L.str63, 110

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"\tInput file = %s, output file = %s\n"
	.size	.L.str64, 36

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"\n%s: I/O or other error, bailing out.  Possible reason follows.\n"
	.size	.L.str65, 65

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"bzip2: I'm not configured correctly for this platform!\n\tI require Int32, Int16 and Char to have sizes\n\tof 4, 2 and 1 bytes to run properly, and they don't.\n\tProbably you can fix this by defining them correctly,\n\tand recompiling.  Bye!\n"
	.size	.L.str66, 236

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	" "
	.size	.L.str67, 2

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"\n%s: PANIC -- internal consistency error:\n\t%s\n\tThis is a BUG.  Please report it to me at:\n\tjseward@bzip.org\n"
	.size	.L.str68, 109

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"uncompress: bad modes\n"
	.size	.L.str69, 23

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"(stdout)"
	.size	.L.str70, 9

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	".out"
	.size	.L.str71, 5

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"%s: Can't open input file %s: %s.\n"
	.size	.L.str72, 35

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"%s: Input file %s is not a normal file.\n"
	.size	.L.str73, 41

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"%s: Can't guess original name for %s -- using %s\n"
	.size	.L.str74, 50

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"%s: Output file %s already exists.\n"
	.size	.L.str75, 36

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"%s: Input file %s has %d other link%s.\n"
	.size	.L.str76, 40

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"s"
	.size	.L.str77, 2

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"wb"
	.size	.L.str78, 3

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"%s: Can't create output file %s: %s.\n"
	.size	.L.str79, 38

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"uncompress: bad srcMode"
	.size	.L.str80, 24

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"done\n"
	.size	.L.str81, 6

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"not a bzip2 file.\n"
	.size	.L.str82, 19

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"%s: %s is not a bzip2 file.\n"
	.size	.L.str83, 29

	.type	fileMetaInfo,@object    # @fileMetaInfo
	.local	fileMetaInfo
	.comm	fileMetaInfo,144,8
	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"decompress:bzReadGetUnused"
	.size	.L.str84, 27

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"\n%s: %s: trailing garbage after EOF ignored\n"
	.size	.L.str85, 45

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"decompress:unexpected error"
	.size	.L.str86, 28

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"\n%s: Compressed file ends unexpectedly;\n\tperhaps it is corrupted?  *Possible* reason follows.\n"
	.size	.L.str87, 95

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"\nIt is possible that the compressed file(s) have become corrupted.\nYou can use the -tvv option to test integrity of such files.\n\nYou can use the `bzip2recover' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n"
	.size	.L.str88, 241

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"\n%s: Data integrity error when decompressing.\n"
	.size	.L.str89, 47

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"compress: bad modes\n"
	.size	.L.str90, 21

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"%s: Input file %s already has %s suffix.\n"
	.size	.L.str91, 42

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"%s: I won't write compressed data to a terminal.\n"
	.size	.L.str92, 50

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"compress: bad srcMode"
	.size	.L.str93, 22

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"\n"
	.size	.L.str94, 2

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	" no data compressed.\n"
	.size	.L.str95, 22

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"%6.3f:1, %6.3f bits/byte, %5.2f%% saved, %s in, %s out.\n"
	.size	.L.str96, 57

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"compress:unexpected error"
	.size	.L.str97, 26

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"\n%s: Control-C or similar caught, quitting.\n"
	.size	.L.str98, 45

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"%s: %s is redundant in versions 0.9.5 and above\n"
	.size	.L.str99, 49

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"bzip2, a block-sorting file compressor.  Version %s.\n\n   usage: %s [flags and input files in any order]\n\n   -h --help           print this message\n   -d --decompress     force decompression\n   -z --compress       force compression\n   -k --keep           keep (don't delete) input files\n   -f --force          overwrite existing output files\n   -t --test           test compressed file integrity\n   -c --stdout         output to standard out\n   -q --quiet          suppress noncritical error messages\n   -v --verbose        be verbose (a 2nd -v gives more)\n   -L --license        display software version & license\n   -V --version        display software version & license\n   -s --small          use less memory (at most 2500k)\n   -1 .. -9            set block size to 100k .. 900k\n   --fast              alias for -1\n   --best              alias for -9\n\n   If invoked as `bzip2', default action is to compress.\n              as `bunzip2',  default action is to decompress.\n              as `bzcat', default action is to decompress to stdout.\n\n   If no file names are given, bzip2 compresses or decompresses\n   from standard input to standard output.  You can combine\n   short flags, so `-v -4' means the same as -v4 or -4v, &c.\n\n"
	.size	.L.str100, 1230

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"bzip2, a block-sorting file compressor.  Version %s.\n   \n   Copyright (C) 1996-2005 by Julian Seward.\n   \n   This program is free software; you can redistribute it and/or modify\n   it under the terms set out in the LICENSE file, which is included\n   in the bzip2-1.0 source distribution.\n   \n   This program is distributed in the hope that it will be useful,\n   but WITHOUT ANY WARRANTY; without even the implied warranty of\n   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n   LICENSE file for more details.\n   \n"
	.size	.L.str101, 529

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"bzip2: file name\n`%s'\nis suspiciously (more than %d chars) long.\nTry using a reasonable file name instead.  Sorry! :-)\n"
	.size	.L.str102, 120

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"\n%s: Caught a SIGSEGV or SIGBUS whilst compressing.\n\n   Possible causes are (most likely first):\n   (1) This computer has unreliable memory or cache hardware\n       (a surprisingly common problem; try a different machine.)\n   (2) A bug in the compiler used to create this executable\n       (unlikely, if you didn't compile bzip2 yourself.)\n   (3) A real bug in bzip2 -- I hope this should never be the case.\n   The user's manual, Section 4.3, has more info on (1) and (2).\n   \n   If you suspect this is a bug in bzip2, or are unsure about (1)\n   or (2), feel free to report it to me at: jseward@bzip.org.\n   Section 4.3 of the user's manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don't\n   have the manual or can't be bothered to read it, mail me anyway.\n\n"
	.size	.L.str103, 869

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"\n%s: Caught a SIGSEGV or SIGBUS whilst decompressing.\n\n   Possible causes are (most likely first):\n   (1) The compressed data is corrupted, and bzip2's usual checks\n       failed to detect this.  Try bzip2 -tvv my_file.bz2.\n   (2) This computer has unreliable memory or cache hardware\n       (a surprisingly common problem; try a different machine.)\n   (3) A bug in the compiler used to create this executable\n       (unlikely, if you didn't compile bzip2 yourself.)\n   (4) A real bug in bzip2 -- I hope this should never be the case.\n   The user's manual, Section 4.3, has more info on (2) and (3).\n   \n   If you suspect this is a bug in bzip2, or are unsure about (2)\n   or (3), feel free to report it to me at: jseward@bzip.org.\n   Section 4.3 of the user's manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don't\n   have the manual or can't be bothered to read it, mail me anyway.\n\n"
	.size	.L.str104, 996

	.data
.Ldebug_end1:
	.text
.Ldebug_end2:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"bzip2.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"zSuffix"
.Linfo_string4:
	.asciz	"char"
.Linfo_string5:
	.asciz	"Char"
.Linfo_string6:
	.asciz	"int"
.Linfo_string7:
	.asciz	"unzSuffix"
.Linfo_string8:
	.asciz	"verbosity"
.Linfo_string9:
	.asciz	"Int32"
.Linfo_string10:
	.asciz	"keepInputFiles"
.Linfo_string11:
	.asciz	"unsigned char"
.Linfo_string12:
	.asciz	"Bool"
.Linfo_string13:
	.asciz	"smallMode"
.Linfo_string14:
	.asciz	"deleteOutputOnInterrupt"
.Linfo_string15:
	.asciz	"forceOverwrite"
.Linfo_string16:
	.asciz	"testFailsExist"
.Linfo_string17:
	.asciz	"unzFailsExist"
.Linfo_string18:
	.asciz	"noisy"
.Linfo_string19:
	.asciz	"numFileNames"
.Linfo_string20:
	.asciz	"numFilesProcessed"
.Linfo_string21:
	.asciz	"blockSize100k"
.Linfo_string22:
	.asciz	"exitValue"
.Linfo_string23:
	.asciz	"opMode"
.Linfo_string24:
	.asciz	"srcMode"
.Linfo_string25:
	.asciz	"longestFileName"
.Linfo_string26:
	.asciz	"inName"
.Linfo_string27:
	.asciz	"outName"
.Linfo_string28:
	.asciz	"tmpName"
.Linfo_string29:
	.asciz	"progName"
.Linfo_string30:
	.asciz	"progNameReally"
.Linfo_string31:
	.asciz	"outputHandleJustInCase"
.Linfo_string32:
	.asciz	"_IO_FILE"
.Linfo_string33:
	.asciz	"FILE"
.Linfo_string34:
	.asciz	"workFactor"
.Linfo_string35:
	.asciz	"fileMetaInfo"
.Linfo_string36:
	.asciz	"st_dev"
.Linfo_string37:
	.asciz	"long unsigned int"
.Linfo_string38:
	.asciz	"dev_t"
.Linfo_string39:
	.asciz	"st_ino"
.Linfo_string40:
	.asciz	"ino_t"
.Linfo_string41:
	.asciz	"st_nlink"
.Linfo_string42:
	.asciz	"nlink_t"
.Linfo_string43:
	.asciz	"st_mode"
.Linfo_string44:
	.asciz	"unsigned int"
.Linfo_string45:
	.asciz	"mode_t"
.Linfo_string46:
	.asciz	"st_uid"
.Linfo_string47:
	.asciz	"uid_t"
.Linfo_string48:
	.asciz	"st_gid"
.Linfo_string49:
	.asciz	"gid_t"
.Linfo_string50:
	.asciz	"__pad0"
.Linfo_string51:
	.asciz	"st_rdev"
.Linfo_string52:
	.asciz	"st_size"
.Linfo_string53:
	.asciz	"long int"
.Linfo_string54:
	.asciz	"off_t"
.Linfo_string55:
	.asciz	"st_blksize"
.Linfo_string56:
	.asciz	"blksize_t"
.Linfo_string57:
	.asciz	"st_blocks"
.Linfo_string58:
	.asciz	"blkcnt_t"
.Linfo_string59:
	.asciz	"st_atim"
.Linfo_string60:
	.asciz	"tv_sec"
.Linfo_string61:
	.asciz	"time_t"
.Linfo_string62:
	.asciz	"tv_nsec"
.Linfo_string63:
	.asciz	"timespec"
.Linfo_string64:
	.asciz	"st_mtim"
.Linfo_string65:
	.asciz	"st_ctim"
.Linfo_string66:
	.asciz	"__unused"
.Linfo_string67:
	.asciz	"stat"
.Linfo_string68:
	.asciz	"fopen_output_safely"
.Linfo_string69:
	.asciz	"main"
.Linfo_string70:
	.asciz	"IntNative"
.Linfo_string71:
	.asciz	"testf"
.Linfo_string72:
	.asciz	"testStream"
.Linfo_string73:
	.asciz	"outOfMemory"
.Linfo_string74:
	.asciz	"cleanUpAndFail"
.Linfo_string75:
	.asciz	"showFileNames"
.Linfo_string76:
	.asciz	"ioError"
.Linfo_string77:
	.asciz	"configError"
.Linfo_string78:
	.asciz	"myfeof"
.Linfo_string79:
	.asciz	"pad"
.Linfo_string80:
	.asciz	"fileExists"
.Linfo_string81:
	.asciz	"containsDubiousChars"
.Linfo_string82:
	.asciz	"panic"
.Linfo_string83:
	.asciz	"setExit"
.Linfo_string84:
	.asciz	"uncompress"
.Linfo_string85:
	.asciz	"applySavedMetaInfoToOutputFile"
.Linfo_string86:
	.asciz	"uncompressStream"
.Linfo_string87:
	.asciz	"compressedStreamEOF"
.Linfo_string88:
	.asciz	"cadvise"
.Linfo_string89:
	.asciz	"crcError"
.Linfo_string90:
	.asciz	"saveInputFileMetaInfo"
.Linfo_string91:
	.asciz	"countHardLinks"
.Linfo_string92:
	.asciz	"notAStandardFile"
.Linfo_string93:
	.asciz	"mapSuffix"
.Linfo_string94:
	.asciz	"hasSuffix"
.Linfo_string95:
	.asciz	"compress"
.Linfo_string96:
	.asciz	"compressStream"
.Linfo_string97:
	.asciz	"uInt64_toAscii"
.Linfo_string98:
	.asciz	"uInt64_isZero"
.Linfo_string99:
	.asciz	"uInt64_qrm10"
.Linfo_string100:
	.asciz	"uInt64_to_double"
.Linfo_string101:
	.asciz	"double"
.Linfo_string102:
	.asciz	"uInt64_from_UInt32s"
.Linfo_string103:
	.asciz	"mySignalCatcher"
.Linfo_string104:
	.asciz	"redundant"
.Linfo_string105:
	.asciz	"usage"
.Linfo_string106:
	.asciz	"license"
.Linfo_string107:
	.asciz	"snocString"
.Linfo_string108:
	.asciz	"name"
.Linfo_string109:
	.asciz	"link"
.Linfo_string110:
	.asciz	"zzzz"
.Linfo_string111:
	.asciz	"Cell"
.Linfo_string112:
	.asciz	"myMalloc"
.Linfo_string113:
	.asciz	"mkCell"
.Linfo_string114:
	.asciz	"addFlagsFromEnvVar"
.Linfo_string115:
	.asciz	"copyFileName"
.Linfo_string116:
	.asciz	"mySIGSEGVorSIGBUScatcher"
.Linfo_string117:
	.asciz	"mode"
.Linfo_string118:
	.asciz	"fp"
.Linfo_string119:
	.asciz	"fh"
.Linfo_string120:
	.asciz	"argc"
.Linfo_string121:
	.asciz	"argv"
.Linfo_string122:
	.asciz	"i"
.Linfo_string123:
	.asciz	"j"
.Linfo_string124:
	.asciz	"tmp"
.Linfo_string125:
	.asciz	"argList"
.Linfo_string126:
	.asciz	"aa"
.Linfo_string127:
	.asciz	"decode"
.Linfo_string128:
	.asciz	"aa2"
.Linfo_string129:
	.asciz	"n"
.Linfo_string130:
	.asciz	"to"
.Linfo_string131:
	.asciz	"from"
.Linfo_string132:
	.asciz	"varName"
.Linfo_string133:
	.asciz	"k"
.Linfo_string134:
	.asciz	"envbase"
.Linfo_string135:
	.asciz	"p"
.Linfo_string136:
	.asciz	"root"
.Linfo_string137:
	.asciz	"fullProgName"
.Linfo_string138:
	.asciz	"flag"
.Linfo_string139:
	.asciz	"inStr"
.Linfo_string140:
	.asciz	"outStr"
.Linfo_string141:
	.asciz	"statBuf"
.Linfo_string142:
	.asciz	"retVal"
.Linfo_string143:
	.asciz	"magicNumberOK"
.Linfo_string144:
	.asciz	"cantGuess"
.Linfo_string145:
	.asciz	"v"
.Linfo_string146:
	.asciz	"allOK"
.Linfo_string147:
	.asciz	"s"
.Linfo_string148:
	.asciz	"exists"
.Linfo_string149:
	.asciz	"zStream"
.Linfo_string150:
	.asciz	"bzf"
.Linfo_string151:
	.asciz	"bzerr"
.Linfo_string152:
	.asciz	"bzerr_dummy"
.Linfo_string153:
	.asciz	"ret"
.Linfo_string154:
	.asciz	"nread"
.Linfo_string155:
	.asciz	"streamNo"
.Linfo_string156:
	.asciz	"obuf"
.Linfo_string157:
	.asciz	"UChar"
.Linfo_string158:
	.asciz	"unused"
.Linfo_string159:
	.asciz	"nUnused"
.Linfo_string160:
	.asciz	"unusedTmpV"
.Linfo_string161:
	.asciz	"unusedTmp"
.Linfo_string162:
	.asciz	"f"
.Linfo_string163:
	.asciz	"c"
.Linfo_string164:
	.asciz	"ec"
.Linfo_string165:
	.asciz	"oldSuffix"
.Linfo_string166:
	.asciz	"newSuffix"
.Linfo_string167:
	.asciz	"srcName"
.Linfo_string168:
	.asciz	"stream"
.Linfo_string169:
	.asciz	"dstName"
.Linfo_string170:
	.asciz	"uTimBuf"
.Linfo_string171:
	.asciz	"actime"
.Linfo_string172:
	.asciz	"modtime"
.Linfo_string173:
	.asciz	"utimbuf"
.Linfo_string174:
	.asciz	"suffix"
.Linfo_string175:
	.asciz	"ns"
.Linfo_string176:
	.asciz	"nx"
.Linfo_string177:
	.asciz	"ibuf"
.Linfo_string178:
	.asciz	"nIbuf"
.Linfo_string179:
	.asciz	"nbytes_in_lo32"
.Linfo_string180:
	.asciz	"UInt32"
.Linfo_string181:
	.asciz	"nbytes_in_hi32"
.Linfo_string182:
	.asciz	"nbytes_out_lo32"
.Linfo_string183:
	.asciz	"nbytes_out_hi32"
.Linfo_string184:
	.asciz	"buf_nin"
.Linfo_string185:
	.asciz	"buf_nout"
.Linfo_string186:
	.asciz	"nbytes_in"
.Linfo_string187:
	.asciz	"b"
.Linfo_string188:
	.asciz	"UInt64"
.Linfo_string189:
	.asciz	"nbytes_out"
.Linfo_string190:
	.asciz	"nbytes_in_d"
.Linfo_string191:
	.asciz	"nbytes_out_d"
.Linfo_string192:
	.asciz	"lo32"
.Linfo_string193:
	.asciz	"hi32"
.Linfo_string194:
	.asciz	"base"
.Linfo_string195:
	.asciz	"sum"
.Linfo_string196:
	.asciz	"outbuf"
.Linfo_string197:
	.asciz	"q"
.Linfo_string198:
	.asciz	"buf"
.Linfo_string199:
	.asciz	"nBuf"
.Linfo_string200:
	.asciz	"n_copy"
.Linfo_string201:
	.asciz	"rem"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	5129                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x1402 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x16 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	60                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	1238                    # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	zSuffix
	.byte	3                       # Abbrev [3] 0x3c:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x41:0x6 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.byte	3                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x48:0x5 DW_TAG_pointer_type
	.long	77                      # DW_AT_type
	.byte	6                       # Abbrev [6] 0x4d:0xc DW_TAG_typedef
	.long	89                      # DW_AT_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x59:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x60:0x7 DW_TAG_base_type
	.long	.Linfo_string6          # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	2                       # Abbrev [2] 0x67:0x16 DW_TAG_variable
	.long	.Linfo_string7          # DW_AT_name
	.long	60                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	1240                    # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	unzSuffix
	.byte	2                       # Abbrev [2] 0x7d:0x16 DW_TAG_variable
	.long	.Linfo_string8          # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	282                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	verbosity
	.byte	6                       # Abbrev [6] 0x93:0xc DW_TAG_typedef
	.long	159                     # DW_AT_type
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	263                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x9f:0x7 DW_TAG_base_type
	.long	.Linfo_string6          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # Abbrev [2] 0xa6:0x16 DW_TAG_variable
	.long	.Linfo_string10         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	keepInputFiles
	.byte	6                       # Abbrev [6] 0xbc:0xc DW_TAG_typedef
	.long	200                     # DW_AT_type
	.long	.Linfo_string12         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0xc8:0x7 DW_TAG_base_type
	.long	.Linfo_string11         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	2                       # Abbrev [2] 0xcf:0x16 DW_TAG_variable
	.long	.Linfo_string13         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	smallMode
	.byte	2                       # Abbrev [2] 0xe5:0x16 DW_TAG_variable
	.long	.Linfo_string14         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	deleteOutputOnInterrupt
	.byte	2                       # Abbrev [2] 0xfb:0x16 DW_TAG_variable
	.long	.Linfo_string15         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	forceOverwrite
	.byte	2                       # Abbrev [2] 0x111:0x16 DW_TAG_variable
	.long	.Linfo_string16         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	testFailsExist
	.byte	2                       # Abbrev [2] 0x127:0x16 DW_TAG_variable
	.long	.Linfo_string17         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	unzFailsExist
	.byte	2                       # Abbrev [2] 0x13d:0x16 DW_TAG_variable
	.long	.Linfo_string18         # DW_AT_name
	.long	188                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	noisy
	.byte	2                       # Abbrev [2] 0x153:0x16 DW_TAG_variable
	.long	.Linfo_string19         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	numFileNames
	.byte	2                       # Abbrev [2] 0x169:0x16 DW_TAG_variable
	.long	.Linfo_string20         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	numFilesProcessed
	.byte	2                       # Abbrev [2] 0x17f:0x16 DW_TAG_variable
	.long	.Linfo_string21         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	blockSize100k
	.byte	2                       # Abbrev [2] 0x195:0x16 DW_TAG_variable
	.long	.Linfo_string22         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	286                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	exitValue
	.byte	2                       # Abbrev [2] 0x1ab:0x16 DW_TAG_variable
	.long	.Linfo_string23         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	298                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	opMode
	.byte	2                       # Abbrev [2] 0x1c1:0x16 DW_TAG_variable
	.long	.Linfo_string24         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	299                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	srcMode
	.byte	2                       # Abbrev [2] 0x1d7:0x16 DW_TAG_variable
	.long	.Linfo_string25         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	303                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	longestFileName
	.byte	2                       # Abbrev [2] 0x1ed:0x16 DW_TAG_variable
	.long	.Linfo_string26         # DW_AT_name
	.long	515                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	304                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	inName
	.byte	3                       # Abbrev [3] 0x203:0xd DW_TAG_array_type
	.long	77                      # DW_AT_type
	.byte	9                       # Abbrev [9] 0x208:0x7 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.short	1033                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	2                       # Abbrev [2] 0x210:0x16 DW_TAG_variable
	.long	.Linfo_string27         # DW_AT_name
	.long	515                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	305                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	outName
	.byte	2                       # Abbrev [2] 0x226:0x16 DW_TAG_variable
	.long	.Linfo_string28         # DW_AT_name
	.long	515                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	306                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	tmpName
	.byte	2                       # Abbrev [2] 0x23c:0x16 DW_TAG_variable
	.long	.Linfo_string29         # DW_AT_name
	.long	72                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	307                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	progName
	.byte	2                       # Abbrev [2] 0x252:0x16 DW_TAG_variable
	.long	.Linfo_string30         # DW_AT_name
	.long	515                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	308                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	progNameReally
	.byte	2                       # Abbrev [2] 0x268:0x16 DW_TAG_variable
	.long	.Linfo_string31         # DW_AT_name
	.long	638                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	312                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	outputHandleJustInCase
	.byte	5                       # Abbrev [5] 0x27e:0x5 DW_TAG_pointer_type
	.long	643                     # DW_AT_type
	.byte	6                       # Abbrev [6] 0x283:0xc DW_TAG_typedef
	.long	655                     # DW_AT_type
	.long	.Linfo_string33         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x28f:0x5 DW_TAG_structure_type
	.long	.Linfo_string32         # DW_AT_name
                                        # DW_AT_declaration
	.byte	2                       # Abbrev [2] 0x294:0x16 DW_TAG_variable
	.long	.Linfo_string34         # DW_AT_name
	.long	147                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.short	314                     # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	workFactor
	.byte	11                      # Abbrev [11] 0x2aa:0x16 DW_TAG_variable
	.long	.Linfo_string35         # DW_AT_name
	.long	704                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	1176                    # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	fileMetaInfo
	.byte	12                      # Abbrev [12] 0x2c0:0xcc DW_TAG_structure_type
	.long	.Linfo_string67         # DW_AT_name
	.byte	144                     # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	4                       # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x2c8:0xd DW_TAG_member
	.long	.Linfo_string36         # DW_AT_name
	.long	908                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	5                       # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x2d5:0xd DW_TAG_member
	.long	.Linfo_string39         # DW_AT_name
	.long	926                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	6                       # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x2e2:0xd DW_TAG_member
	.long	.Linfo_string41         # DW_AT_name
	.long	937                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	7                       # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x2ef:0xd DW_TAG_member
	.long	.Linfo_string43         # DW_AT_name
	.long	948                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	9                       # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x2fc:0xd DW_TAG_member
	.long	.Linfo_string46         # DW_AT_name
	.long	966                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	10                      # DW_AT_decl_line
	.byte	28                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x309:0xd DW_TAG_member
	.long	.Linfo_string48         # DW_AT_name
	.long	978                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	11                      # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x316:0xd DW_TAG_member
	.long	.Linfo_string50         # DW_AT_name
	.long	959                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	12                      # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x323:0xd DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	908                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	13                      # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x330:0xd DW_TAG_member
	.long	.Linfo_string52         # DW_AT_name
	.long	990                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	14                      # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x33d:0xd DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	1008                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	15                      # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x34a:0xd DW_TAG_member
	.long	.Linfo_string57         # DW_AT_name
	.long	1019                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	16                      # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x357:0xd DW_TAG_member
	.long	.Linfo_string59         # DW_AT_name
	.long	1030                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	18                      # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x364:0xd DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	1030                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	19                      # DW_AT_decl_line
	.byte	88                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x371:0xd DW_TAG_member
	.long	.Linfo_string65         # DW_AT_name
	.long	1030                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	20                      # DW_AT_decl_line
	.byte	104                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x37e:0xd DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	1076                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	21                      # DW_AT_decl_line
	.byte	120                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x38c:0xb DW_TAG_typedef
	.long	919                     # DW_AT_type
	.long	.Linfo_string38         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	202                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x397:0x7 DW_TAG_base_type
	.long	.Linfo_string37         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	14                      # Abbrev [14] 0x39e:0xb DW_TAG_typedef
	.long	919                     # DW_AT_type
	.long	.Linfo_string40         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	197                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0x3a9:0xb DW_TAG_typedef
	.long	919                     # DW_AT_type
	.long	.Linfo_string42         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	187                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0x3b4:0xb DW_TAG_typedef
	.long	959                     # DW_AT_type
	.long	.Linfo_string45         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	182                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x3bf:0x7 DW_TAG_base_type
	.long	.Linfo_string44         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	6                       # Abbrev [6] 0x3c6:0xc DW_TAG_typedef
	.long	959                     # DW_AT_type
	.long	.Linfo_string47         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0x3d2:0xc DW_TAG_typedef
	.long	959                     # DW_AT_type
	.long	.Linfo_string49         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0x3de:0xb DW_TAG_typedef
	.long	1001                    # DW_AT_type
	.long	.Linfo_string54         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	192                     # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x3e9:0x7 DW_TAG_base_type
	.long	.Linfo_string53         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	14                      # Abbrev [14] 0x3f0:0xb DW_TAG_typedef
	.long	1001                    # DW_AT_type
	.long	.Linfo_string56         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0x3fb:0xb DW_TAG_typedef
	.long	1001                    # DW_AT_type
	.long	.Linfo_string58         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.byte	12                      # Abbrev [12] 0x406:0x23 DW_TAG_structure_type
	.long	.Linfo_string63         # DW_AT_name
	.byte	16                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	254                     # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x40e:0xd DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	1065                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	254                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x41b:0xd DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	1001                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	254                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	14                      # Abbrev [14] 0x429:0xb DW_TAG_typedef
	.long	1001                    # DW_AT_type
	.long	.Linfo_string61         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	54                      # DW_AT_decl_line
	.byte	3                       # Abbrev [3] 0x434:0xc DW_TAG_array_type
	.long	1001                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x439:0x6 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.byte	2                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x440:0x5c DW_TAG_subprogram
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1102                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	638                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x45f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1102                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0x46e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1102                    # DW_AT_decl_line
	.long	4957                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	17                      # Abbrev [17] 0x47d:0xf DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1105                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0x48c:0xf DW_TAG_variable
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1106                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x49c:0xb2 DW_TAG_subprogram
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1909                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	1358                    # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x4bb:0xf DW_TAG_formal_parameter
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1909                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	16                      # Abbrev [16] 0x4ca:0xf DW_TAG_formal_parameter
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1909                    # DW_AT_decl_line
	.long	4967                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0x4d9:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1911                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	17                      # Abbrev [17] 0x4e8:0xf DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1911                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0x4f7:0x10 DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1912                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	17                      # Abbrev [17] 0x507:0x10 DW_TAG_variable
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1913                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	17                      # Abbrev [17] 0x517:0x10 DW_TAG_variable
	.long	.Linfo_string126        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1914                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	17                      # Abbrev [17] 0x527:0x10 DW_TAG_variable
	.long	.Linfo_string127        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1915                    # DW_AT_decl_line
	.long	188                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\247\177"
	.byte	18                      # Abbrev [18] 0x537:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	17                      # Abbrev [17] 0x53c:0x10 DW_TAG_variable
	.long	.Linfo_string128        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	2155                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x54e:0xc DW_TAG_typedef
	.long	159                     # DW_AT_type
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.byte	19                      # Abbrev [19] 0x55a:0x59 DW_TAG_subprogram
	.long	.Linfo_string71         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1645                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin13          # DW_AT_low_pc
	.quad	.Lfunc_end13            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x575:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1645                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0x584:0xf DW_TAG_variable
	.long	.Linfo_string139        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1647                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	17                      # Abbrev [17] 0x593:0xf DW_TAG_variable
	.long	.Linfo_string146        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1648                    # DW_AT_decl_line
	.long	188                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	87
	.byte	17                      # Abbrev [17] 0x5a2:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1649                    # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300~"
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x5b3:0xf2 DW_TAG_subprogram
	.long	.Linfo_string72         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	676                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin18          # DW_AT_low_pc
	.quad	.Lfunc_end18            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x5d2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string149        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	676                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0x5e1:0xf DW_TAG_variable
	.long	.Linfo_string150        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	679                     # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0x5f0:0x10 DW_TAG_variable
	.long	.Linfo_string151        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	17                      # Abbrev [17] 0x600:0x10 DW_TAG_variable
	.long	.Linfo_string152        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	17                      # Abbrev [17] 0x610:0x10 DW_TAG_variable
	.long	.Linfo_string153        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	17                      # Abbrev [17] 0x620:0x10 DW_TAG_variable
	.long	.Linfo_string154        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	17                      # Abbrev [17] 0x630:0x10 DW_TAG_variable
	.long	.Linfo_string155        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	17                      # Abbrev [17] 0x640:0x10 DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	680                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	17                      # Abbrev [17] 0x650:0x10 DW_TAG_variable
	.long	.Linfo_string156        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	681                     # DW_AT_decl_line
	.long	4977                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240X"
	.byte	17                      # Abbrev [17] 0x660:0x11 DW_TAG_variable
	.long	.Linfo_string158        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	682                     # DW_AT_decl_line
	.long	4977                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\220\261\177"
	.byte	17                      # Abbrev [17] 0x671:0x11 DW_TAG_variable
	.long	.Linfo_string159        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	683                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\214\261\177"
	.byte	17                      # Abbrev [17] 0x682:0x11 DW_TAG_variable
	.long	.Linfo_string160        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	684                     # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\200\261\177"
	.byte	17                      # Abbrev [17] 0x693:0x11 DW_TAG_variable
	.long	.Linfo_string161        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	685                     # DW_AT_decl_line
	.long	5002                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\370\260\177"
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x6a5:0x1b DW_TAG_subprogram
	.long	.Linfo_string73         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1014                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin22          # DW_AT_low_pc
	.quad	.Lfunc_end22            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	19                      # Abbrev [19] 0x6c0:0x4a DW_TAG_subprogram
	.long	.Linfo_string74         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	818                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin24          # DW_AT_low_pc
	.quad	.Lfunc_end24            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x6db:0xf DW_TAG_formal_parameter
	.long	.Linfo_string164        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	818                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	17                      # Abbrev [17] 0x6ea:0xf DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	820                     # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0x6f9:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	822                     # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340~"
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x70a:0x1b DW_TAG_subprogram
	.long	.Linfo_string75         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	805                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin23          # DW_AT_low_pc
	.quad	.Lfunc_end23            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	21                      # Abbrev [21] 0x725:0x1b DW_TAG_subprogram
	.long	.Linfo_string76         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	929                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin21          # DW_AT_low_pc
	.quad	.Lfunc_end21            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	21                      # Abbrev [21] 0x740:0x1b DW_TAG_subprogram
	.long	.Linfo_string77         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1026                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin20          # DW_AT_low_pc
	.quad	.Lfunc_end20            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	20                      # Abbrev [20] 0x75b:0x3e DW_TAG_subprogram
	.long	.Linfo_string78         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	423                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin19          # DW_AT_low_pc
	.quad	.Lfunc_end19            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x77a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string162        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	423                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0x789:0xf DW_TAG_variable
	.long	.Linfo_string163        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	426                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x799:0x3a DW_TAG_subprogram
	.long	.Linfo_string79         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1051                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin17          # DW_AT_low_pc
	.quad	.Lfunc_end17            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x7b4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string147        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1051                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0x7c3:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1053                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x7d3:0x4d DW_TAG_subprogram
	.long	.Linfo_string80         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1083                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin16          # DW_AT_low_pc
	.quad	.Lfunc_end16            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x7f2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1083                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	17                      # Abbrev [17] 0x801:0xf DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1085                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0x810:0xf DW_TAG_variable
	.long	.Linfo_string148        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1086                    # DW_AT_decl_line
	.long	188                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	111
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x820:0x2f DW_TAG_subprogram
	.long	.Linfo_string81         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1217                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin15          # DW_AT_low_pc
	.quad	.Lfunc_end15            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x83f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1217                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x84f:0x2b DW_TAG_subprogram
	.long	.Linfo_string82         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	879                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin14          # DW_AT_low_pc
	.quad	.Lfunc_end14            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x86a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string147        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	879                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x87a:0x2b DW_TAG_subprogram
	.long	.Linfo_string83         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	778                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.quad	.Lfunc_end12            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x895:0xf DW_TAG_formal_parameter
	.long	.Linfo_string145        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	778                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x8a5:0x11b DW_TAG_subprogram
	.long	.Linfo_string84         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1447                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.quad	.Lfunc_end11            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x8c0:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1447                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0x8cf:0xf DW_TAG_variable
	.long	.Linfo_string139        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1449                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	17                      # Abbrev [17] 0x8de:0xf DW_TAG_variable
	.long	.Linfo_string140        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1450                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	17                      # Abbrev [17] 0x8ed:0xf DW_TAG_variable
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1451                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	17                      # Abbrev [17] 0x8fc:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1451                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0x90b:0xf DW_TAG_variable
	.long	.Linfo_string143        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1452                    # DW_AT_decl_line
	.long	188                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	71
	.byte	17                      # Abbrev [17] 0x91a:0xf DW_TAG_variable
	.long	.Linfo_string144        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1453                    # DW_AT_decl_line
	.long	188                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	70
	.byte	17                      # Abbrev [17] 0x929:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1454                    # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260~"
	.byte	18                      # Abbrev [18] 0x939:0x86 DW_TAG_lexical_block
	.long	.Ldebug_ranges10        # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0x93e:0x5e DW_TAG_lexical_block
	.quad	.Ltmp374                # DW_AT_low_pc
	.quad	.Ltmp381                # DW_AT_high_pc
	.byte	22                      # Abbrev [22] 0x94f:0x4c DW_TAG_lexical_block
	.quad	.Ltmp374                # DW_AT_low_pc
	.quad	.Ltmp381                # DW_AT_high_pc
	.byte	22                      # Abbrev [22] 0x960:0x3a DW_TAG_lexical_block
	.quad	.Ltmp375                # DW_AT_low_pc
	.quad	.Ltmp381                # DW_AT_high_pc
	.byte	18                      # Abbrev [18] 0x971:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges6         # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0x976:0x22 DW_TAG_lexical_block
	.quad	.Ltmp379                # DW_AT_low_pc
	.quad	.Ltmp381                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0x987:0x10 DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1614                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x99c:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges9         # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0x9a1:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges8         # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0x9a6:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges7         # DW_AT_ranges
	.byte	17                      # Abbrev [17] 0x9ab:0x10 DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1622                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x9c0:0x49 DW_TAG_subprogram
	.long	.Linfo_string85         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1192                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin30          # DW_AT_low_pc
	.quad	.Lfunc_end30            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x9db:0xf DW_TAG_formal_parameter
	.long	.Linfo_string169        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1192                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0x9ea:0xf DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1195                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	17                      # Abbrev [17] 0x9f9:0xf DW_TAG_variable
	.long	.Linfo_string170        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1196                    # DW_AT_decl_line
	.long	5007                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xa09:0x102 DW_TAG_subprogram
	.long	.Linfo_string86         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	547                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin29          # DW_AT_low_pc
	.quad	.Lfunc_end29            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xa28:0xf DW_TAG_formal_parameter
	.long	.Linfo_string149        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	547                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	16                      # Abbrev [16] 0xa37:0xf DW_TAG_formal_parameter
	.long	.Linfo_string168        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	547                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0xa46:0x10 DW_TAG_variable
	.long	.Linfo_string150        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	550                     # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	17                      # Abbrev [17] 0xa56:0x10 DW_TAG_variable
	.long	.Linfo_string151        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	17                      # Abbrev [17] 0xa66:0x10 DW_TAG_variable
	.long	.Linfo_string152        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	17                      # Abbrev [17] 0xa76:0x10 DW_TAG_variable
	.long	.Linfo_string153        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	17                      # Abbrev [17] 0xa86:0x10 DW_TAG_variable
	.long	.Linfo_string154        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	17                      # Abbrev [17] 0xa96:0x10 DW_TAG_variable
	.long	.Linfo_string155        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	17                      # Abbrev [17] 0xaa6:0x10 DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	17                      # Abbrev [17] 0xab6:0x10 DW_TAG_variable
	.long	.Linfo_string156        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	552                     # DW_AT_decl_line
	.long	4977                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220X"
	.byte	17                      # Abbrev [17] 0xac6:0x11 DW_TAG_variable
	.long	.Linfo_string158        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	553                     # DW_AT_decl_line
	.long	4977                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\200\261\177"
	.byte	17                      # Abbrev [17] 0xad7:0x11 DW_TAG_variable
	.long	.Linfo_string159        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	554                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\374\260\177"
	.byte	17                      # Abbrev [17] 0xae8:0x11 DW_TAG_variable
	.long	.Linfo_string160        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	555                     # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\360\260\177"
	.byte	17                      # Abbrev [17] 0xaf9:0x11 DW_TAG_variable
	.long	.Linfo_string161        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	556                     # DW_AT_decl_line
	.long	5002                    # DW_AT_type
	.byte	4                       # DW_AT_location
	.byte	145
	.ascii	"\350\260\177"
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xb0b:0x1b DW_TAG_subprogram
	.long	.Linfo_string87         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	912                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin32          # DW_AT_low_pc
	.quad	.Lfunc_end32            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	21                      # Abbrev [21] 0xb26:0x1b DW_TAG_subprogram
	.long	.Linfo_string88         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	790                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin33          # DW_AT_low_pc
	.quad	.Lfunc_end33            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	21                      # Abbrev [21] 0xb41:0x1b DW_TAG_subprogram
	.long	.Linfo_string89         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	899                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin31          # DW_AT_low_pc
	.quad	.Lfunc_end31            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	19                      # Abbrev [19] 0xb5c:0x3a DW_TAG_subprogram
	.long	.Linfo_string90         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1180                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin28          # DW_AT_low_pc
	.quad	.Lfunc_end28            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xb77:0xf DW_TAG_formal_parameter
	.long	.Linfo_string167        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1180                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	17                      # Abbrev [17] 0xb86:0xf DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1183                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xb96:0x4e DW_TAG_subprogram
	.long	.Linfo_string91         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1140                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	147                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin27          # DW_AT_low_pc
	.quad	.Lfunc_end27            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xbb5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1140                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0xbc4:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1142                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	17                      # Abbrev [17] 0xbd3:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1143                    # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330~"
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xbe4:0x4e DW_TAG_subprogram
	.long	.Linfo_string92         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1123                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin26          # DW_AT_low_pc
	.quad	.Lfunc_end26            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xc03:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1123                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0xc12:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1125                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	17                      # Abbrev [17] 0xc21:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1126                    # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330~"
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xc32:0x4d DW_TAG_subprogram
	.long	.Linfo_string93         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1254                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin25          # DW_AT_low_pc
	.quad	.Lfunc_end25            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xc51:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1254                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	16                      # Abbrev [16] 0xc60:0xf DW_TAG_formal_parameter
	.long	.Linfo_string165        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1255                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	16                      # Abbrev [16] 0xc6f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string166        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1255                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xc7f:0x5c DW_TAG_subprogram
	.long	.Linfo_string94         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1244                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin34          # DW_AT_low_pc
	.quad	.Lfunc_end34            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xc9e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string147        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1244                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0xcad:0xf DW_TAG_formal_parameter
	.long	.Linfo_string174        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1244                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	17                      # Abbrev [17] 0xcbc:0xf DW_TAG_variable
	.long	.Linfo_string175        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1246                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	17                      # Abbrev [17] 0xccb:0xf DW_TAG_variable
	.long	.Linfo_string176        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1247                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0xcdb:0x9f DW_TAG_subprogram
	.long	.Linfo_string95         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1266                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.quad	.Lfunc_end10            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xcf6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1266                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0xd05:0xf DW_TAG_variable
	.long	.Linfo_string139        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1268                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	17                      # Abbrev [17] 0xd14:0xf DW_TAG_variable
	.long	.Linfo_string140        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1269                    # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	17                      # Abbrev [17] 0xd23:0xf DW_TAG_variable
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1270                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	17                      # Abbrev [17] 0xd32:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1270                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0xd41:0x10 DW_TAG_variable
	.long	.Linfo_string141        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1271                    # DW_AT_decl_line
	.long	704                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270~"
	.byte	18                      # Abbrev [18] 0xd51:0x28 DW_TAG_lexical_block
	.long	.Ldebug_ranges5         # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0xd56:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0xd5b:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0xd60:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	17                      # Abbrev [17] 0xd65:0x10 DW_TAG_variable
	.long	.Linfo_string142        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1436                    # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0xd7a:0x15d DW_TAG_subprogram
	.long	.Linfo_string96         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	438                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin35          # DW_AT_low_pc
	.quad	.Lfunc_end35            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xd95:0xf DW_TAG_formal_parameter
	.long	.Linfo_string168        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	438                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	16                      # Abbrev [16] 0xda4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string149        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	438                     # DW_AT_decl_line
	.long	638                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0xdb3:0xf DW_TAG_variable
	.long	.Linfo_string150        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	441                     # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0xdc2:0x10 DW_TAG_variable
	.long	.Linfo_string177        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	442                     # DW_AT_decl_line
	.long	4977                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260X"
	.byte	17                      # Abbrev [17] 0xdd2:0x10 DW_TAG_variable
	.long	.Linfo_string178        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	443                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254X"
	.byte	17                      # Abbrev [17] 0xde2:0x10 DW_TAG_variable
	.long	.Linfo_string179        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	444                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250X"
	.byte	17                      # Abbrev [17] 0xdf2:0x10 DW_TAG_variable
	.long	.Linfo_string181        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	444                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244X"
	.byte	17                      # Abbrev [17] 0xe02:0x10 DW_TAG_variable
	.long	.Linfo_string182        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	445                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240X"
	.byte	17                      # Abbrev [17] 0xe12:0x10 DW_TAG_variable
	.long	.Linfo_string183        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	445                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234X"
	.byte	17                      # Abbrev [17] 0xe22:0x10 DW_TAG_variable
	.long	.Linfo_string151        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	446                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230X"
	.byte	17                      # Abbrev [17] 0xe32:0x10 DW_TAG_variable
	.long	.Linfo_string152        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	446                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224X"
	.byte	17                      # Abbrev [17] 0xe42:0x10 DW_TAG_variable
	.long	.Linfo_string153        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	446                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220X"
	.byte	18                      # Abbrev [18] 0xe52:0x84 DW_TAG_lexical_block
	.long	.Ldebug_ranges13        # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0xe57:0x7e DW_TAG_lexical_block
	.long	.Ldebug_ranges12        # DW_AT_ranges
	.byte	18                      # Abbrev [18] 0xe5c:0x78 DW_TAG_lexical_block
	.long	.Ldebug_ranges11        # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0xe61:0x72 DW_TAG_lexical_block
	.quad	.Ltmp818                # DW_AT_low_pc
	.quad	.Ltmp819                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0xe72:0x10 DW_TAG_variable
	.long	.Linfo_string184        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	496                     # DW_AT_decl_line
	.long	5054                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360W"
	.byte	17                      # Abbrev [17] 0xe82:0x10 DW_TAG_variable
	.long	.Linfo_string185        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	496                     # DW_AT_decl_line
	.long	5054                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320W"
	.byte	17                      # Abbrev [17] 0xe92:0x10 DW_TAG_variable
	.long	.Linfo_string186        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	497                     # DW_AT_decl_line
	.long	5066                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310W"
	.byte	17                      # Abbrev [17] 0xea2:0x10 DW_TAG_variable
	.long	.Linfo_string189        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	497                     # DW_AT_decl_line
	.long	5066                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300W"
	.byte	17                      # Abbrev [17] 0xeb2:0x10 DW_TAG_variable
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	498                     # DW_AT_decl_line
	.long	4180                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270W"
	.byte	17                      # Abbrev [17] 0xec2:0x10 DW_TAG_variable
	.long	.Linfo_string191        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	498                     # DW_AT_decl_line
	.long	4180                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260W"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0xed7:0x87 DW_TAG_subprogram
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin38          # DW_AT_low_pc
	.quad	.Lfunc_end38            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xef2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string196        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
	.long	5115                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0xf01:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
	.long	5110                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	17                      # Abbrev [17] 0xf10:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	399                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	17                      # Abbrev [17] 0xf1f:0xf DW_TAG_variable
	.long	.Linfo_string197        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	399                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	17                      # Abbrev [17] 0xf2e:0xf DW_TAG_variable
	.long	.Linfo_string198        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.long	5120                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0xf3d:0x10 DW_TAG_variable
	.long	.Linfo_string199        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	401                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	17                      # Abbrev [17] 0xf4d:0x10 DW_TAG_variable
	.long	.Linfo_string200        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	402                     # DW_AT_decl_line
	.long	5066                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xf5e:0x3e DW_TAG_subprogram
	.long	.Linfo_string98         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	368                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	188                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin40          # DW_AT_low_pc
	.quad	.Lfunc_end40            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xf7d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	368                     # DW_AT_decl_line
	.long	5110                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0xf8c:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	370                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xf9c:0x5c DW_TAG_subprogram
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	379                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	147                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin39          # DW_AT_low_pc
	.quad	.Lfunc_end39            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0xfbb:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	379                     # DW_AT_decl_line
	.long	5110                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	17                      # Abbrev [17] 0xfca:0xf DW_TAG_variable
	.long	.Linfo_string201        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	381                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	17                      # Abbrev [17] 0xfd9:0xf DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	381                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	17                      # Abbrev [17] 0xfe8:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	382                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0xff8:0x5c DW_TAG_subprogram
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	354                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	4180                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin37          # DW_AT_low_pc
	.quad	.Lfunc_end37            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x1017:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	354                     # DW_AT_decl_line
	.long	5110                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	17                      # Abbrev [17] 0x1026:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	356                     # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	17                      # Abbrev [17] 0x1035:0xf DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	357                     # DW_AT_decl_line
	.long	4180                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	17                      # Abbrev [17] 0x1044:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	358                     # DW_AT_decl_line
	.long	4180                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x1054:0x7 DW_TAG_base_type
	.long	.Linfo_string101        # DW_AT_name
	.byte	4                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	19                      # Abbrev [19] 0x105b:0x49 DW_TAG_subprogram
	.long	.Linfo_string102        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin36          # DW_AT_low_pc
	.quad	.Lfunc_end36            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x1076:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
	.long	5110                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	16                      # Abbrev [16] 0x1085:0xf DW_TAG_formal_parameter
	.long	.Linfo_string192        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	16                      # Abbrev [16] 0x1094:0xf DW_TAG_formal_parameter
	.long	.Linfo_string193        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
	.long	5042                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x10a4:0x2b DW_TAG_subprogram
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	943                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.quad	.Lfunc_end9             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x10bf:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	943                     # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x10cf:0x2b DW_TAG_subprogram
	.long	.Linfo_string104        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1803                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.quad	.Lfunc_end8             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x10ea:0xf DW_TAG_formal_parameter
	.long	.Linfo_string138        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1803                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x10fa:0x2b DW_TAG_subprogram
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1759                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.quad	.Lfunc_end7             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x1115:0xf DW_TAG_formal_parameter
	.long	.Linfo_string137        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1759                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0x1125:0x1b DW_TAG_subprogram
	.long	.Linfo_string106        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1734                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.quad	.Lfunc_end6             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	20                      # Abbrev [20] 0x1140:0x86 DW_TAG_subprogram
	.long	.Linfo_string107        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1862                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	4550                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.quad	.Lfunc_end5             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x115f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string136        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1862                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0x116e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1862                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	22                      # Abbrev [22] 0x117d:0x48 DW_TAG_lexical_block
	.quad	.Ltmp196                # DW_AT_low_pc
	.quad	.Ltmp203                # DW_AT_high_pc
	.byte	18                      # Abbrev [18] 0x118e:0x15 DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	17                      # Abbrev [17] 0x1193:0xf DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1870                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0x11a3:0x21 DW_TAG_lexical_block
	.quad	.Ltmp201                # DW_AT_low_pc
	.quad	.Ltmp202                # DW_AT_high_pc
	.byte	17                      # Abbrev [17] 0x11b4:0xf DW_TAG_variable
	.long	.Linfo_string124        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1865                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x11c6:0x5 DW_TAG_pointer_type
	.long	4555                    # DW_AT_type
	.byte	6                       # Abbrev [6] 0x11cb:0xc DW_TAG_typedef
	.long	4567                    # DW_AT_type
	.long	.Linfo_string111        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1832                    # DW_AT_decl_line
	.byte	23                      # Abbrev [23] 0x11d7:0x26 DW_TAG_structure_type
	.long	.Linfo_string110        # DW_AT_name
	.byte	16                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.short	1828                    # DW_AT_decl_line
	.byte	24                      # Abbrev [24] 0x11e0:0xe DW_TAG_member
	.long	.Linfo_string108        # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	1829                    # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	24                      # Abbrev [24] 0x11ee:0xe DW_TAG_member
	.long	.Linfo_string109        # DW_AT_name
	.long	4605                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	1830                    # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x11fd:0x5 DW_TAG_pointer_type
	.long	4567                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x1202:0x3e DW_TAG_subprogram
	.long	.Linfo_string112        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1837                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	4672                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin42          # DW_AT_low_pc
	.quad	.Lfunc_end42            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x1221:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1837                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	17                      # Abbrev [17] 0x1230:0xf DW_TAG_variable
	.long	.Linfo_string135        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1839                    # DW_AT_decl_line
	.long	4672                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0x1240:0x1 DW_TAG_pointer_type
	.byte	20                      # Abbrev [20] 0x1241:0x2f DW_TAG_subprogram
	.long	.Linfo_string113        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1849                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	4550                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin41          # DW_AT_low_pc
	.quad	.Lfunc_end41            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	17                      # Abbrev [17] 0x1260:0xf DW_TAG_variable
	.long	.Linfo_string163        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1851                    # DW_AT_decl_line
	.long	4550                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x1270:0x88 DW_TAG_subprogram
	.long	.Linfo_string114        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1880                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.quad	.Lfunc_end4             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x128b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string125        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1880                    # DW_AT_decl_line
	.long	4972                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	16                      # Abbrev [16] 0x129a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string132        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1880                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	17                      # Abbrev [17] 0x12a9:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1882                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	17                      # Abbrev [17] 0x12b8:0xf DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1882                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	17                      # Abbrev [17] 0x12c7:0x10 DW_TAG_variable
	.long	.Linfo_string133        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1882                    # DW_AT_decl_line
	.long	147                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	17                      # Abbrev [17] 0x12d7:0x10 DW_TAG_variable
	.long	.Linfo_string134        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1883                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	17                      # Abbrev [17] 0x12e7:0x10 DW_TAG_variable
	.long	.Linfo_string135        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1883                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x12f8:0x3a DW_TAG_subprogram
	.long	.Linfo_string115        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1062                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.quad	.Lfunc_end3             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x1313:0xf DW_TAG_formal_parameter
	.long	.Linfo_string130        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1062                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	16                      # Abbrev [16] 0x1322:0xf DW_TAG_formal_parameter
	.long	.Linfo_string131        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1062                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x1332:0x2b DW_TAG_subprogram
	.long	.Linfo_string116        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	954                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x134d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	954                     # DW_AT_decl_line
	.long	1358                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x135d:0x5 DW_TAG_pointer_type
	.long	4962                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0x1362:0x5 DW_TAG_const_type
	.long	89                      # DW_AT_type
	.byte	5                       # Abbrev [5] 0x1367:0x5 DW_TAG_pointer_type
	.long	72                      # DW_AT_type
	.byte	5                       # Abbrev [5] 0x136c:0x5 DW_TAG_pointer_type
	.long	4550                    # DW_AT_type
	.byte	3                       # Abbrev [3] 0x1371:0xd DW_TAG_array_type
	.long	4990                    # DW_AT_type
	.byte	9                       # Abbrev [9] 0x1376:0x7 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.short	4999                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x137e:0xc DW_TAG_typedef
	.long	200                     # DW_AT_type
	.long	.Linfo_string157        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	262                     # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x138a:0x5 DW_TAG_pointer_type
	.long	4990                    # DW_AT_type
	.byte	12                      # Abbrev [12] 0x138f:0x23 DW_TAG_structure_type
	.long	.Linfo_string173        # DW_AT_name
	.byte	16                      # DW_AT_byte_size
	.byte	4                       # DW_AT_decl_file
	.byte	12                      # DW_AT_decl_line
	.byte	13                      # Abbrev [13] 0x1397:0xd DW_TAG_member
	.long	.Linfo_string171        # DW_AT_name
	.long	1065                    # DW_AT_type
	.byte	4                       # DW_AT_decl_file
	.byte	14                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	13                      # Abbrev [13] 0x13a4:0xd DW_TAG_member
	.long	.Linfo_string172        # DW_AT_name
	.long	1065                    # DW_AT_type
	.byte	4                       # DW_AT_decl_file
	.byte	15                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x13b2:0xc DW_TAG_typedef
	.long	959                     # DW_AT_type
	.long	.Linfo_string180        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
	.byte	3                       # Abbrev [3] 0x13be:0xc DW_TAG_array_type
	.long	77                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x13c3:0x6 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.byte	31                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x13ca:0xc DW_TAG_typedef
	.long	5078                    # DW_AT_type
	.long	.Linfo_string188        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	336                     # DW_AT_decl_line
	.byte	27                      # Abbrev [27] 0x13d6:0x14 DW_TAG_structure_type
	.byte	8                       # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.short	335                     # DW_AT_decl_line
	.byte	24                      # Abbrev [24] 0x13db:0xe DW_TAG_member
	.long	.Linfo_string187        # DW_AT_name
	.long	5098                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	335                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x13ea:0xc DW_TAG_array_type
	.long	4990                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x13ef:0x6 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.byte	7                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x13f6:0x5 DW_TAG_pointer_type
	.long	5066                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x13fb:0x5 DW_TAG_pointer_type
	.long	89                      # DW_AT_type
	.byte	3                       # Abbrev [3] 0x1400:0xc DW_TAG_array_type
	.long	4990                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1405:0x6 DW_TAG_subrange_type
	.long	96                      # DW_AT_type
	.byte	31                      # DW_AT_upper_bound
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
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
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
	.byte	7                       # Abbreviation Code
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
	.byte	8                       # Abbreviation Code
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
	.byte	9                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	60                      # DW_AT_declaration
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
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
	.byte	12                      # Abbreviation Code
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
	.byte	13                      # Abbreviation Code
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
	.byte	14                      # Abbreviation Code
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
	.byte	18                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	19                      # Abbreviation Code
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
	.byte	20                      # Abbreviation Code
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
	.byte	21                      # Abbreviation Code
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
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	24                      # Abbreviation Code
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
	.byte	25                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
	.byte	38                      # DW_TAG_const_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
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
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp131
	.quad	.Ltmp132
	.quad	.Ltmp133
	.quad	.Ltmp138
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp197
	.quad	.Ltmp201
	.quad	.Ltmp202
	.quad	.Ltmp203
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp303
	.quad	.Ltmp304
	.quad	.Ltmp315
	.quad	.Ltmp316
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp300
	.quad	.Ltmp301
	.quad	.Ltmp302
	.quad	.Ltmp304
	.quad	.Ltmp315
	.quad	.Ltmp316
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp299
	.quad	.Ltmp304
	.quad	.Ltmp315
	.quad	.Ltmp316
	.quad	0
	.quad	0
.Ldebug_ranges5:
	.quad	.Ltmp296
	.quad	.Ltmp297
	.quad	.Ltmp298
	.quad	.Ltmp304
	.quad	.Ltmp315
	.quad	.Ltmp316
	.quad	0
	.quad	0
.Ldebug_ranges6:
	.quad	.Ltmp376
	.quad	.Ltmp377
	.quad	.Ltmp378
	.quad	.Ltmp381
	.quad	0
	.quad	0
.Ldebug_ranges7:
	.quad	.Ltmp384
	.quad	.Ltmp385
	.quad	.Ltmp407
	.quad	.Ltmp408
	.quad	0
	.quad	0
.Ldebug_ranges8:
	.quad	.Ltmp381
	.quad	.Ltmp382
	.quad	.Ltmp383
	.quad	.Ltmp385
	.quad	.Ltmp407
	.quad	.Ltmp408
	.quad	0
	.quad	0
.Ldebug_ranges9:
	.quad	.Ltmp381
	.quad	.Ltmp385
	.quad	.Ltmp407
	.quad	.Ltmp408
	.quad	0
	.quad	0
.Ldebug_ranges10:
	.quad	.Ltmp371
	.quad	.Ltmp372
	.quad	.Ltmp373
	.quad	.Ltmp385
	.quad	.Ltmp407
	.quad	.Ltmp408
	.quad	0
	.quad	0
.Ldebug_ranges11:
	.quad	.Ltmp817
	.quad	.Ltmp819
	.quad	.Ltmp820
	.quad	.Ltmp821
	.quad	0
	.quad	0
.Ldebug_ranges12:
	.quad	.Ltmp817
	.quad	.Ltmp819
	.quad	.Ltmp820
	.quad	.Ltmp821
	.quad	0
	.quad	0
.Ldebug_ranges13:
	.quad	.Ltmp816
	.quad	.Ltmp819
	.quad	.Ltmp820
	.quad	.Ltmp821
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
	.long	594                     # DIE offset
	.asciz	"progNameReally"        # External Name
	.long	3044                    # DIE offset
	.asciz	"notAStandardFile"      # External Name
	.long	1701                    # DIE offset
	.asciz	"outOfMemory"           # External Name
	.long	1883                    # DIE offset
	.asciz	"myfeof"                # External Name
	.long	2966                    # DIE offset
	.asciz	"countHardLinks"        # External Name
	.long	4416                    # DIE offset
	.asciz	"snocString"            # External Name
	.long	2127                    # DIE offset
	.asciz	"panic"                 # External Name
	.long	3199                    # DIE offset
	.asciz	"hasSuffix"             # External Name
	.long	550                     # DIE offset
	.asciz	"tmpName"               # External Name
	.long	1802                    # DIE offset
	.asciz	"showFileNames"         # External Name
	.long	493                     # DIE offset
	.asciz	"inName"                # External Name
	.long	4720                    # DIE offset
	.asciz	"addFlagsFromEnvVar"    # External Name
	.long	2854                    # DIE offset
	.asciz	"cadvise"               # External Name
	.long	1856                    # DIE offset
	.asciz	"configError"           # External Name
	.long	1829                    # DIE offset
	.asciz	"ioError"               # External Name
	.long	339                     # DIE offset
	.asciz	"numFileNames"          # External Name
	.long	1088                    # DIE offset
	.asciz	"fopen_output_safely"   # External Name
	.long	38                      # DIE offset
	.asciz	"zSuffix"               # External Name
	.long	1728                    # DIE offset
	.asciz	"cleanUpAndFail"        # External Name
	.long	3450                    # DIE offset
	.asciz	"compressStream"        # External Name
	.long	295                     # DIE offset
	.asciz	"unzFailsExist"         # External Name
	.long	572                     # DIE offset
	.asciz	"progName"              # External Name
	.long	2827                    # DIE offset
	.asciz	"compressedStreamEOF"   # External Name
	.long	4856                    # DIE offset
	.asciz	"copyFileName"          # External Name
	.long	471                     # DIE offset
	.asciz	"longestFileName"       # External Name
	.long	2496                    # DIE offset
	.asciz	"applySavedMetaInfoToOutputFile" # External Name
	.long	383                     # DIE offset
	.asciz	"blockSize100k"         # External Name
	.long	660                     # DIE offset
	.asciz	"workFactor"            # External Name
	.long	3996                    # DIE offset
	.asciz	"uInt64_qrm10"          # External Name
	.long	4303                    # DIE offset
	.asciz	"redundant"             # External Name
	.long	1370                    # DIE offset
	.asciz	"testf"                 # External Name
	.long	2170                    # DIE offset
	.asciz	"setExit"               # External Name
	.long	166                     # DIE offset
	.asciz	"keepInputFiles"        # External Name
	.long	4389                    # DIE offset
	.asciz	"license"               # External Name
	.long	361                     # DIE offset
	.asciz	"numFilesProcessed"     # External Name
	.long	1459                    # DIE offset
	.asciz	"testStream"            # External Name
	.long	3291                    # DIE offset
	.asciz	"compress"              # External Name
	.long	2213                    # DIE offset
	.asciz	"uncompress"            # External Name
	.long	103                     # DIE offset
	.asciz	"unzSuffix"             # External Name
	.long	3122                    # DIE offset
	.asciz	"mapSuffix"             # External Name
	.long	4187                    # DIE offset
	.asciz	"uInt64_from_UInt32s"   # External Name
	.long	1945                    # DIE offset
	.asciz	"pad"                   # External Name
	.long	616                     # DIE offset
	.asciz	"outputHandleJustInCase" # External Name
	.long	3799                    # DIE offset
	.asciz	"uInt64_toAscii"        # External Name
	.long	528                     # DIE offset
	.asciz	"outName"               # External Name
	.long	4346                    # DIE offset
	.asciz	"usage"                 # External Name
	.long	207                     # DIE offset
	.asciz	"smallMode"             # External Name
	.long	4260                    # DIE offset
	.asciz	"mySignalCatcher"       # External Name
	.long	2003                    # DIE offset
	.asciz	"fileExists"            # External Name
	.long	4610                    # DIE offset
	.asciz	"myMalloc"              # External Name
	.long	2881                    # DIE offset
	.asciz	"crcError"              # External Name
	.long	427                     # DIE offset
	.asciz	"opMode"                # External Name
	.long	1180                    # DIE offset
	.asciz	"main"                  # External Name
	.long	3934                    # DIE offset
	.asciz	"uInt64_isZero"         # External Name
	.long	125                     # DIE offset
	.asciz	"verbosity"             # External Name
	.long	4088                    # DIE offset
	.asciz	"uInt64_to_double"      # External Name
	.long	2080                    # DIE offset
	.asciz	"containsDubiousChars"  # External Name
	.long	4914                    # DIE offset
	.asciz	"mySIGSEGVorSIGBUScatcher" # External Name
	.long	449                     # DIE offset
	.asciz	"srcMode"               # External Name
	.long	229                     # DIE offset
	.asciz	"deleteOutputOnInterrupt" # External Name
	.long	317                     # DIE offset
	.asciz	"noisy"                 # External Name
	.long	2908                    # DIE offset
	.asciz	"saveInputFileMetaInfo" # External Name
	.long	4673                    # DIE offset
	.asciz	"mkCell"                # External Name
	.long	251                     # DIE offset
	.asciz	"forceOverwrite"        # External Name
	.long	405                     # DIE offset
	.asciz	"exitValue"             # External Name
	.long	2569                    # DIE offset
	.asciz	"uncompressStream"      # External Name
	.long	273                     # DIE offset
	.asciz	"testFailsExist"        # External Name
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
	.long	1065                    # DIE offset
	.asciz	"time_t"                # External Name
	.long	978                     # DIE offset
	.asciz	"gid_t"                 # External Name
	.long	959                     # DIE offset
	.asciz	"unsigned int"          # External Name
	.long	1008                    # DIE offset
	.asciz	"blksize_t"             # External Name
	.long	159                     # DIE offset
	.asciz	"int"                   # External Name
	.long	5066                    # DIE offset
	.asciz	"UInt64"                # External Name
	.long	937                     # DIE offset
	.asciz	"nlink_t"               # External Name
	.long	1019                    # DIE offset
	.asciz	"blkcnt_t"              # External Name
	.long	908                     # DIE offset
	.asciz	"dev_t"                 # External Name
	.long	1358                    # DIE offset
	.asciz	"IntNative"             # External Name
	.long	4990                    # DIE offset
	.asciz	"UChar"                 # External Name
	.long	966                     # DIE offset
	.asciz	"uid_t"                 # External Name
	.long	919                     # DIE offset
	.asciz	"long unsigned int"     # External Name
	.long	4180                    # DIE offset
	.asciz	"double"                # External Name
	.long	1001                    # DIE offset
	.asciz	"long int"              # External Name
	.long	89                      # DIE offset
	.asciz	"char"                  # External Name
	.long	643                     # DIE offset
	.asciz	"FILE"                  # External Name
	.long	4555                    # DIE offset
	.asciz	"Cell"                  # External Name
	.long	5042                    # DIE offset
	.asciz	"UInt32"                # External Name
	.long	4567                    # DIE offset
	.asciz	"zzzz"                  # External Name
	.long	188                     # DIE offset
	.asciz	"Bool"                  # External Name
	.long	990                     # DIE offset
	.asciz	"off_t"                 # External Name
	.long	147                     # DIE offset
	.asciz	"Int32"                 # External Name
	.long	948                     # DIE offset
	.asciz	"mode_t"                # External Name
	.long	926                     # DIE offset
	.asciz	"ino_t"                 # External Name
	.long	1030                    # DIE offset
	.asciz	"timespec"              # External Name
	.long	200                     # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	704                     # DIE offset
	.asciz	"stat"                  # External Name
	.long	5007                    # DIE offset
	.asciz	"utimbuf"               # External Name
	.long	77                      # DIE offset
	.asciz	"Char"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
