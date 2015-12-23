	.file	"toplev.c.bc"
	.text
	.globl	set_Wunused
	.align	16, 0x90
	.type	set_Wunused,@function
set_Wunused:                            # @set_Wunused
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	movl	%edi, warn_unused_function(%rip)
	movl	%edi, warn_unused_label(%rip)
	je	.LBB0_1
# BB#2:                                 # %if.else
	movl	warn_unused_parameter(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_4
	jmp	.LBB0_3
.LBB0_1:                                # %if.then
	xorq	%rax, %rax
.LBB0_3:                                # %if.then2
	movl	%eax, warn_unused_parameter(%rip)
.LBB0_4:                                # %if.end3
	movl	%edi, warn_unused_variable(%rip)
	movl	%edi, warn_unused_value(%rip)
	retq
.Ltmp0:
	.size	set_Wunused, .Ltmp0-set_Wunused
	.cfi_endproc

	.globl	set_fast_math_flags
	.align	16, 0x90
	.type	set_fast_math_flags,@function
set_fast_math_flags:                    # @set_fast_math_flags
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, flag_trapping_math(%rip)
	movq	$1, %rcx
	movl	%ecx, flag_unsafe_math_optimizations(%rip)
	movl	%eax, flag_errno_math(%rip)
	retq
.Ltmp1:
	.size	set_fast_math_flags, .Ltmp1-set_fast_math_flags
	.cfi_endproc

	.globl	set_no_fast_math_flags
	.align	16, 0x90
	.type	set_no_fast_math_flags,@function
set_no_fast_math_flags:                 # @set_no_fast_math_flags
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movl	%eax, flag_trapping_math(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, flag_unsafe_math_optimizations(%rip)
	movl	%eax, flag_errno_math(%rip)
	retq
.Ltmp2:
	.size	set_no_fast_math_flags, .Ltmp2-set_no_fast_math_flags
	.cfi_endproc

	.globl	read_integral_parameter
	.align	16, 0x90
	.type	read_integral_parameter,@function
read_integral_parameter:                # @read_integral_parameter
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 16
.Ltmp6:
	.cfi_offset %rbx, -16
	movzbl	(%rdi), %eax
	movq	%rdx, %rbx
	testq	%rax, %rax
	je	.LBB3_7
# BB#1:                                 # %while.body.preheader
	leaq	1(%rdi), %rcx
	.align	16, 0x90
.LBB3_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB3_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB3_3 Depth=1
	movzbl	(%rcx), %eax
	incq	%rcx
	testq	%rax, %rax
	jne	.LBB3_3
.LBB3_7:                                # %if.end11
	popq	%rbx
	jmp	atoi  # TAILCALL
.LBB3_4:                                # %if.then6
	testq	%rsi, %rsi
	je	.LBB3_6
# BB#5:                                 # %if.then9
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	error
.LBB3_6:                                # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp7:
	.size	read_integral_parameter, .Ltmp7-read_integral_parameter
	.cfi_endproc

	.globl	do_abort
	.align	16, 0x90
	.type	do_abort,@function
do_abort:                               # @do_abort
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 16
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.do_abort, %rdx
	movq	$1592, %rsi             # imm = 0x638
	callq	fancy_abort
.Ltmp10:
	.size	do_abort, .Ltmp10-do_abort
	.cfi_endproc

	.globl	botch
	.align	16, 0x90
	.type	botch,@function
botch:                                  # @botch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp12:
	.cfi_def_cfa_offset 16
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.botch, %rdx
	movq	$1602, %rsi             # imm = 0x642
	callq	fancy_abort
.Ltmp13:
	.size	botch, .Ltmp13-botch
	.cfi_endproc

	.globl	exact_log2_wide
	.align	16, 0x90
	.type	exact_log2_wide,@function
exact_log2_wide:                        # @exact_log2_wide
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	je	.LBB6_4
# BB#1:                                 # %lor.lhs.false
	movq	%rdi, %rcx
	negq	%rcx
	andq	%rdi, %rcx
	cmpq	%rdi, %rcx
	jne	.LBB6_4
# BB#2:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB6_3:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	$1, %rcx
	shrq	%cl, %rdi
	leaq	1(%rax), %rcx
	testq	%rdi, %rdi
	jne	.LBB6_3
.LBB6_4:                                # %return
	retq
.Ltmp14:
	.size	exact_log2_wide, .Ltmp14-exact_log2_wide
	.cfi_endproc

	.globl	floor_log2_wide
	.align	16, 0x90
	.type	floor_log2_wide,@function
floor_log2_wide:                        # @floor_log2_wide
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	je	.LBB7_3
# BB#1:
	movq	$1, %rcx
	.align	16, 0x90
.LBB7_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	incq	%rax
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	jne	.LBB7_2
.LBB7_3:                                # %while.end
	retq
.Ltmp15:
	.size	floor_log2_wide, .Ltmp15-floor_log2_wide
	.cfi_endproc

	.globl	do_float_handler
	.align	16, 0x90
	.type	do_float_handler,@function
do_float_handler:                       # @do_float_handler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp23:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 40
	subq	$200, %rsp
.Ltmp25:
	.cfi_def_cfa_offset 240
.Ltmp26:
	.cfi_offset %rbx, -40
.Ltmp27:
	.cfi_offset %r12, -32
.Ltmp28:
	.cfi_offset %r14, -24
.Ltmp29:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	leaq	(%rsp), %rdi
	callq	setjmp
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rbx
	jne	.LBB8_4
# BB#1:                                 # %if.end
	movq	$1, %rbx
	movl	%ebx, float_handled(%rip)
	movabsq	$float_handler, %rdi
	leaq	(%rsp), %rsi
	movq	$200, %rdx
	callq	memcpy
	movzbl	float_handler_set(%rip), %eax
	testq	%rax, %rax
	jne	.LBB8_3
# BB#2:                                 # %if.then3.i
	movabsq	$float_signal, %rsi
	movq	$8, %rdi
	callq	signal
	movb	%bl, float_handler_set(%rip)
.LBB8_3:                                # %set_float_handler.exit
	movq	%r14, %rdi
	callq	*%r15
.LBB8_4:                                # %cleanup
	movl	%r12d, float_handled(%rip)
	movq	%rbx, %rax
	addq	$200, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp30:
	.size	do_float_handler, .Ltmp30-do_float_handler
	.cfi_endproc

	.globl	strip_off_ending
	.align	16, 0x90
	.type	strip_off_ending,@function
strip_off_ending:                       # @strip_off_ending
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jle	.LBB9_6
# BB#1:
	movq	$3, %rax
	.align	16, 0x90
.LBB9_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax), %rcx
	movq	%rsi, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rdi,%rdx), %ecx
	cmpq	$46, %rcx
	je	.LBB9_5
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB9_4 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$5, %rdx
	jg	.LBB9_6
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB9_4 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	leaq	1(%rax), %r8
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	movq	%r8, %rax
	jl	.LBB9_4
	jmp	.LBB9_6
.LBB9_5:                                # %if.then
	xorq	%rax, %rax
	movb	%al, (%rdi,%rdx)
.LBB9_6:                                # %for.end
	retq
.Ltmp31:
	.size	strip_off_ending, .Ltmp31-strip_off_ending
	.cfi_endproc

	.globl	output_quoted_string
	.align	16, 0x90
	.type	output_quoted_string,@function
output_quoted_string:                   # @output_quoted_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp37:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp38:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp41:
	.cfi_def_cfa_offset 48
.Ltmp42:
	.cfi_offset %rbx, -40
.Ltmp43:
	.cfi_offset %r12, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	$34, %rdi
	movq	%r14, %rsi
	callq	putc
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB10_9
# BB#1:                                 # %while.body.preheader
	incq	%r12
	movabsq	$.L.str2, %r15
	.align	16, 0x90
.LBB10_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$56, %rcx
	movq	%rax, %rbx
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %edx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	testq	$16, %rdx
	jne	.LBB10_3
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB10_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB10_8
	.align	16, 0x90
.LBB10_3:                               # %if.then
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$34, %rax
	je	.LBB10_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$92, %rax
	jne	.LBB10_6
.LBB10_5:                               # %if.then11
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$92, %rdi
	movq	%r14, %rsi
	callq	putc
.LBB10_6:                               # %if.end
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	putc
.LBB10_8:                               # %while.cond.backedge
                                        #   in Loop: Header=BB10_2 Depth=1
	movzbl	(%r12), %eax
	incq	%r12
	testq	%rax, %rax
	jne	.LBB10_2
.LBB10_9:                               # %while.end
	movq	$34, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp46:
	.size	output_quoted_string, .Ltmp46-output_quoted_string
	.cfi_endproc

	.globl	output_file_directive
	.align	16, 0x90
	.type	output_file_directive,@function
output_file_directive:                  # @output_file_directive
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp53:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp55:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp56:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 48
.Ltmp58:
	.cfi_offset %rbx, -48
.Ltmp59:
	.cfi_offset %r12, -40
.Ltmp60:
	.cfi_offset %r13, -32
.Ltmp61:
	.cfi_offset %r14, -24
.Ltmp62:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	%rbx, %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rbx, %rax
	.align	16, 0x90
.LBB11_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r12
	cmpq	%rbx, %r12
	jbe	.LBB11_3
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB11_1 Depth=1
	movzbl	-1(%r12), %ecx
	leaq	-1(%r12), %rax
	cmpq	$47, %rcx
	jne	.LBB11_1
.LBB11_3:                               # %while.end
	movabsq	$.L.str3, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	$1, %r13
	movq	%r15, %rcx
	callq	fwrite
	movq	$34, %rdi
	movq	%r15, %rsi
	callq	putc
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB11_12
# BB#4:
	movabsq	$.L.str2, %r14
	.align	16, 0x90
.LBB11_5:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$56, %rcx
	movq	%rax, %rbx
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %edx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	testq	$16, %rdx
	jne	.LBB11_6
# BB#10:                                # %if.else.i
                                        #   in Loop: Header=BB11_5 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB11_11
	.align	16, 0x90
.LBB11_6:                               # %if.then.i
                                        #   in Loop: Header=BB11_5 Depth=1
	cmpq	$34, %rax
	je	.LBB11_8
# BB#7:                                 # %if.then.i
                                        #   in Loop: Header=BB11_5 Depth=1
	cmpq	$92, %rax
	jne	.LBB11_9
.LBB11_8:                               # %if.then11.i
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	$92, %rdi
	movq	%r15, %rsi
	callq	putc
.LBB11_9:                               # %if.end.i
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	putc
.LBB11_11:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB11_5 Depth=1
	movzbl	(%r12,%r13), %eax
	incq	%r13
	testq	%rax, %rax
	jne	.LBB11_5
.LBB11_12:                              # %output_quoted_string.exit
	movq	$34, %rdi
	movq	%r15, %rsi
	callq	putc
	movq	$10, %rdi
	movq	%r15, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp63:
	.size	output_file_directive, .Ltmp63-output_file_directive
	.cfi_endproc

	.globl	wrapup_global_declarations
	.align	16, 0x90
	.type	wrapup_global_declarations,@function
wrapup_global_declarations:             # @wrapup_global_declarations
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp72:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp73:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp75:
	.cfi_def_cfa_offset 64
.Ltmp76:
	.cfi_offset %rbx, -48
.Ltmp77:
	.cfi_offset %r12, -40
.Ltmp78:
	.cfi_offset %r13, -32
.Ltmp79:
	.cfi_offset %r14, -24
.Ltmp80:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jle	.LBB12_9
# BB#1:
	movq	$1, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%rbx,8), %rdi
	andq	$-262145, 48(%rdi)      # imm = 0xFFFFFFFFFFFBFFFF
	movzbl	16(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB12_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$0, 40(%rdi)
	jne	.LBB12_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	incomplete_decl_finalize_hook(%rip), %rax
	testq	%rax, %rax
	je	.LBB12_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	*%rax
.LBB12_6:                               # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%r13, %r12
	andq	%r15, %r12
	incq	%rbx
	cmpq	%r12, %rax
	jne	.LBB12_2
# BB#7:                                 # %do.body.preheader
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rcx, %rcx
	testq	%r13, %r13
	jle	.LBB12_8
	.align	16, 0x90
.LBB12_13:                              # %for.body11.lr.ph.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_14 Depth 2
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	$1, %r13
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB12_14:                              # %for.body11.us
                                        #   Parent Loop BB12_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%r14,%r13,8), %rbx
	movl	16(%rbx), %eax
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB12_36
# BB#15:                                # %lor.lhs.false.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movzbl	49(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB12_36
# BB#16:                                # %if.end23.us
                                        #   in Loop: Header=BB12_14 Depth=2
	andq	$262399, %rax           # imm = 0x400FF
	cmpq	$262178, %rax           # imm = 0x40022
	jne	.LBB12_26
# BB#17:                                # %if.then35.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB12_19
# BB#18:                                # %cond.false.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB12_19:                              # %cond.end.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movzbl	18(%rax), %eax
	testq	$4, %rax
	jne	.LBB12_25
# BB#20:                                # %if.else.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	48(%rbx), %rax
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB12_26
# BB#21:                                # %if.else54.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movl	16(%rbx), %ecx
	andq	$528384, %rcx           # imm = 0x81000
	cmpq	$4096, %rcx             # imm = 0x1000
	jne	.LBB12_25
# BB#22:                                # %land.lhs.true66.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movl	optimize(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB12_26
# BB#23:                                # %land.lhs.true66.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movl	flag_keep_static_consts(%rip), %ecx
	testq	%r15, %rcx
	je	.LBB12_26
# BB#24:                                # %land.lhs.true66.us
                                        #   in Loop: Header=BB12_14 Depth=2
	andq	$4194304, %rax          # imm = 0x400000
	jne	.LBB12_26
.LBB12_25:                              # %if.then82.us
                                        #   in Loop: Header=BB12_14 Depth=2
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	movq	%rbx, %rdi
	callq	rest_of_decl_compilation
.LBB12_26:                              # %if.end84.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB12_36
# BB#27:                                # %land.lhs.true90.us
                                        #   in Loop: Header=BB12_14 Depth=2
	cmpq	$0, 104(%rbx)
	je	.LBB12_36
# BB#28:                                # %land.lhs.true93.us
                                        #   in Loop: Header=BB12_14 Depth=2
	cmpq	$0, 160(%rbx)
	je	.LBB12_36
# BB#29:                                # %land.lhs.true96.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movl	flag_keep_inline_functions(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB12_35
# BB#30:                                # %lor.lhs.false98.us
                                        #   in Loop: Header=BB12_14 Depth=2
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB12_32
# BB#31:                                # %land.lhs.true105.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movzbl	51(%rbx), %eax
	testq	$4, %rax
	je	.LBB12_35
.LBB12_32:                              # %lor.lhs.false113.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB12_34
# BB#33:                                # %cond.false118.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB12_34:                              # %cond.end119.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movzbl	18(%rax), %eax
	testq	$4, %rax
	je	.LBB12_36
.LBB12_35:                              # %if.then128.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	%rbx, %rdi
	callq	output_inline_function
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB12_36:                              # %for.inc130.us
                                        #   in Loop: Header=BB12_14 Depth=2
	movq	%r13, %rax
	andq	%r15, %rax
	incq	%r13
	cmpq	%r12, %rax
	jne	.LBB12_14
# BB#10:                                # %for.end132.us
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%r15, %rcx
	movq	$1, %rax
	jne	.LBB12_12
# BB#11:                                # %for.end132.us
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
.LBB12_12:                              # %for.end132.us
                                        #   in Loop: Header=BB12_13 Depth=1
	testq	%rcx, %rcx
	movq	%rax, %rcx
	jne	.LBB12_13
	jmp	.LBB12_9
.LBB12_8:
	xorq	%rax, %rax
.LBB12_9:                               # %do.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp81:
	.size	wrapup_global_declarations, .Ltmp81-wrapup_global_declarations
	.cfi_endproc

	.globl	rest_of_decl_compilation
	.align	16, 0x90
	.type	rest_of_decl_compilation,@function
rest_of_decl_compilation:               # @rest_of_decl_compilation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp88:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp89:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp90:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp91:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 48
.Ltmp93:
	.cfi_offset %rbx, -48
.Ltmp94:
	.cfi_offset %r12, -40
.Ltmp95:
	.cfi_offset %r13, -32
.Ltmp96:
	.cfi_offset %r14, -24
.Ltmp97:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	136(%rbx), %rax
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rsi, %r12
	movabsq	$.L.str7, %rdi
	movq	%rax, %rsi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB13_2
# BB#1:                                 # %if.then
	movq	32(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %rdi
	callq	get_identifier
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	assemble_alias
.LBB13_2:                               # %if.end
	movl	16(%rbx), %eax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB13_5
# BB#3:                                 # %lor.lhs.false
	movq	48(%rbx), %rcx
	testq	$256, %rcx              # imm = 0x100
	jne	.LBB13_5
# BB#4:                                 # %lor.lhs.false11
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB13_25
.LBB13_5:                               # %if.then15
	movq	$11, %rdi
	callq	timevar_push
	testq	%r12, %r12
	je	.LBB13_7
# BB#6:                                 # %if.then17
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	make_decl_rtl
.LBB13_7:                               # %if.end18
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r15
	jne	.LBB13_9
# BB#8:                                 # %lor.lhs.false20
	movzbl	50(%rbx), %eax
	testq	$4, %rax
	jne	.LBB13_10
.LBB13_9:                               # %if.then27
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	assemble_variable
.LBB13_10:                              # %if.end28
	movq	last_assemble_variable_decl(%rip), %rax
	cmpq	%rbx, %rax
	jne	.LBB13_23
# BB#11:                                # %do.body
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB13_13
# BB#12:                                # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB13_13:                              # %cond.end
	movl	flag_inhibit_size_directive(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB13_23
# BB#14:                                # %land.lhs.true
	movq	8(%rax), %rax
	movq	8(%rax), %r12
	testq	%r13, %r15
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB13_16
# BB#15:                                # %land.lhs.true
	movq	%rdx, %rcx
.LBB13_16:                              # %land.lhs.true
	cmpq	$0, 40(%rbx)
	je	.LBB13_18
# BB#17:                                # %land.lhs.true
	movq	%rdx, %rax
.LBB13_18:                              # %land.lhs.true
	testq	%r13, %r14
	je	.LBB13_23
# BB#19:                                # %land.lhs.true
	andq	%rcx, %rax
	je	.LBB13_23
# BB#20:                                # %land.lhs.true46
	movq	104(%rbx), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_23
# BB#21:                                # %land.lhs.true46
	movl	size_directive_output(%rip), %eax
	testq	%r13, %rax
	jne	.LBB13_23
# BB#22:                                # %if.then51
	movq	asm_out_file(%rip), %rcx
	movq	$1, %rax
	movl	%eax, size_directive_output(%rip)
	movabsq	$.L.str9, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%r12, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rsi
	movq	$44, %rdi
	callq	putc
	movq	asm_out_file(%rip), %r14
	movq	8(%rbx), %rdi
	callq	int_size_in_bytes
	movq	%rax, %rcx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB13_23:                              # %if.end59
	movq	$11, %rdi
.LBB13_24:                              # %if.end59
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	timevar_pop  # TAILCALL
.LBB13_25:                              # %if.else
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB13_30
# BB#26:                                # %if.else
	testq	%r12, %r12
	je	.LBB13_30
# BB#27:                                # %if.then68
	movq	%r12, %rdi
	callq	decode_reg_name
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB13_28
# BB#34:                                # %if.then71
	movq	$0, 144(%rbx)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	make_decl_rtl  # TAILCALL
.LBB13_30:                              # %if.else83
	movl	write_symbols(%rip), %ecx
	movabsq	$4294967291, %rdx       # imm = 0xFFFFFFFB
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	jne	.LBB13_33
# BB#31:                                # %if.else83
	cmpq	$33, %rax
	jne	.LBB13_33
# BB#32:                                # %if.then93
	movq	$43, %rdi
	callq	timevar_push
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	dbxout_symbol
	movq	$43, %rdi
	jmp	.LBB13_24
.LBB13_28:                              # %if.else74
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	error
	andq	$-1025, 48(%rbx)        # imm = 0xFFFFFFFFFFFFFBFF
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB13_29
.LBB13_33:                              # %if.end97
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_29:                              # %if.then80
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_decl  # TAILCALL
.Ltmp98:
	.size	rest_of_decl_compilation, .Ltmp98-rest_of_decl_compilation
	.cfi_endproc

	.globl	check_global_declarations
	.align	16, 0x90
	.type	check_global_declarations,@function
check_global_declarations:              # @check_global_declarations
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp105:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp107:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp108:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 48
.Ltmp110:
	.cfi_offset %rbx, -48
.Ltmp111:
	.cfi_offset %r12, -40
.Ltmp112:
	.cfi_offset %r13, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB14_31
# BB#1:
	movq	$1, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rbx
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%r12,8), %r15
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$278783, %rcx           # imm = 0x440FF
	cmpq	$262178, %rcx           # imm = 0x40022
	jne	.LBB14_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$0, 144(%r15)
.LBB14_4:                               # %if.end
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB14_18
# BB#5:                                 # %land.lhs.true17
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	warn_unused_function(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_9
# BB#6:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	120(%r15), %rax
	testq	%rax, %rax
	jne	.LBB14_8
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
.LBB14_8:                               # %cond.end
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%rax), %eax
	testq	$4, %rax
	je	.LBB14_18
.LBB14_9:                               # %land.lhs.true29
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$0, 104(%r15)
	jne	.LBB14_18
# BB#10:                                # %land.lhs.true32
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	48(%r15), %rax
	andq	$4194560, %rax          # imm = 0x400100
	cmpq	$256, %rax              # imm = 0x100
	jne	.LBB14_18
# BB#11:                                # %land.lhs.true45
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%r15), %eax
	testq	$8, %rax
	jne	.LBB14_18
# BB#12:                                # %if.then51
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	120(%r15), %rax
	testq	%rax, %rax
	jne	.LBB14_14
# BB#13:                                # %cond.false56
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
.LBB14_14:                              # %cond.end57
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_15
# BB#16:                                # %if.else
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str5, %rsi
	callq	warning_with_decl
	jmp	.LBB14_17
.LBB14_15:                              # %if.then66
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str4, %rsi
	callq	pedwarn_with_decl
.LBB14_17:                              # %if.end67
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	16(%r15), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%r15)
	movq	%r15, %rdi
	callq	assemble_external
	.align	16, 0x90
.LBB14_18:                              # %if.end72
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	warn_unused_function(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_21
# BB#19:                                # %land.lhs.true74
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB14_21
# BB#20:                                # %land.lhs.true80
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	48(%r15), %rcx
	testq	$2048, %rcx             # imm = 0x800
	je	.LBB14_24
	.align	16, 0x90
.LBB14_21:                              # %lor.lhs.false87
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	warn_unused_variable(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_30
# BB#22:                                # %land.lhs.true89
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$4351, %rcx             # imm = 0x10FF
	cmpq	$34, %rcx
	jne	.LBB14_30
# BB#23:                                # %land.lhs.true89.land.lhs.true101_crit_edge
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	48(%r15), %rcx
.LBB14_24:                              # %land.lhs.true101
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	$65792, %rcx            # imm = 0x10100
	jne	.LBB14_30
# BB#25:                                # %land.lhs.true101
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rax, %rdx
	andq	$589824, %rdx           # imm = 0x90000
	testq	%r13, %rdx
	jne	.LBB14_30
# BB#26:                                # %land.lhs.true129
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	$255, %rax
	cmpq	$30, %rax
	je	.LBB14_28
# BB#27:                                # %land.lhs.true129
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	$1024, %rcx             # imm = 0x400
	jne	.LBB14_30
.LBB14_28:                              # %land.lhs.true142
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	72(%r15), %rax
	movzbl	18(%rax), %eax
	testq	$1, %rax
	jne	.LBB14_30
# BB#29:                                # %if.then150
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str6, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB14_30:                              # %if.end151
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$43, %rdi
	callq	timevar_push
	movq	debug_hooks(%rip), %rax
	movq	%r15, %rdi
	callq	*128(%rax)
	movq	$43, %rdi
	callq	timevar_pop
	movq	%r12, %rax
	andq	%r13, %rax
	incq	%r12
	cmpq	%rbx, %rax
	jne	.LBB14_2
.LBB14_31:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp115:
	.size	check_global_declarations, .Ltmp115-check_global_declarations
	.cfi_endproc

	.globl	push_srcloc
	.align	16, 0x90
	.type	push_srcloc,@function
push_srcloc:                            # @push_srcloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp121:
	.cfi_def_cfa_offset 32
.Ltmp122:
	.cfi_offset %rbx, -24
.Ltmp123:
	.cfi_offset %r14, -16
	movq	input_file_stack(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB15_2
# BB#1:                                 # %if.then
	movq	input_filename(%rip), %rcx
	movq	%rcx, (%rax)
	movl	lineno(%rip), %eax
	movq	input_file_stack(%rip), %rcx
	movl	%eax, 16(%rcx)
.LBB15_2:                               # %if.end
	movq	$24, %rdi
	callq	xmalloc
	movq	%rbx, input_filename(%rip)
	movq	%rbx, (%rax)
	movl	%r14d, lineno(%rip)
	movl	%r14d, 16(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 20(%rax)
	movl	input_file_stack_tick(%rip), %ecx
	movq	input_file_stack(%rip), %rdx
	movq	%rdx, 8(%rax)
	movq	%rax, input_file_stack(%rip)
	incq	%rcx
	movl	%ecx, input_file_stack_tick(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp124:
	.size	push_srcloc, .Ltmp124-push_srcloc
	.cfi_endproc

	.globl	pop_srcloc
	.align	16, 0x90
	.type	pop_srcloc,@function
pop_srcloc:                             # @pop_srcloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp126:
	.cfi_def_cfa_offset 16
	movq	input_file_stack(%rip), %rdi
	movq	8(%rdi), %rax
	movq	%rax, input_file_stack(%rip)
	callq	free
	movl	input_file_stack_tick(%rip), %ecx
	movq	input_file_stack(%rip), %rax
	incq	%rcx
	movl	%ecx, input_file_stack_tick(%rip)
	testq	%rax, %rax
	je	.LBB16_2
# BB#1:                                 # %if.end
	movq	(%rax), %rcx
	movq	%rcx, input_filename(%rip)
	movl	16(%rax), %eax
	movl	%eax, lineno(%rip)
	popq	%rax
	retq
.LBB16_2:                               # %if.then
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.pop_srcloc, %rcx
	movq	$2104, %rsi             # imm = 0x838
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp127:
	.size	pop_srcloc, .Ltmp127-pop_srcloc
	.cfi_endproc

	.globl	rest_of_type_compilation
	.align	16, 0x90
	.type	rest_of_type_compilation,@function
rest_of_type_compilation:               # @rest_of_type_compilation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp131:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp132:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp133:
	.cfi_def_cfa_offset 32
.Ltmp134:
	.cfi_offset %rbx, -24
.Ltmp135:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$43, %rdi
	callq	timevar_push
	movl	write_symbols(%rip), %ecx
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_5
# BB#1:                                 # %if.then
	movq	(%r14), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB17_2
# BB#3:                                 # %if.then
	movabsq	$1, %rsi
	jmp	.LBB17_4
.LBB17_2:
	movabsq	$0, %rsi
.LBB17_4:                               # %if.then
	callq	dbxout_symbol
	movl	write_symbols(%rip), %ecx
.LBB17_5:                               # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$4, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rdi
	movq	%rdx, %rsi
	jne	.LBB17_7
# BB#6:                                 # %if.end
	movq	%rdi, %rsi
.LBB17_7:                               # %if.end
	cmpq	$7, %rcx
	jne	.LBB17_9
# BB#8:                                 # %if.end
	movq	%rdi, %rdx
.LBB17_9:                               # %if.end
	orq	%rdx, %rsi
	cmpq	$1, %rsi
	jne	.LBB17_12
# BB#10:                                # %if.end
	testq	%rax, %rbx
	je	.LBB17_12
# BB#11:                                # %if.then6
	movq	(%r14), %rdi
	callq	dwarf2out_decl
.LBB17_12:                              # %if.end9
	movq	$43, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	timevar_pop  # TAILCALL
.Ltmp136:
	.size	rest_of_type_compilation, .Ltmp136-rest_of_type_compilation
	.cfi_endproc

	.globl	rest_of_compilation
	.align	16, 0x90
	.type	rest_of_compilation,@function
rest_of_compilation:                    # @rest_of_compilation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp143:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp144:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp145:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp146:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp147:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp148:
	.cfi_def_cfa_offset 112
.Ltmp149:
	.cfi_offset %rbx, -48
.Ltmp150:
	.cfi_offset %r12, -40
.Ltmp151:
	.cfi_offset %r13, -32
.Ltmp152:
	.cfi_offset %r14, -24
.Ltmp153:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	$44, %rdi
	callq	timevar_push
	movl	optimize(%rip), %eax
	xorq	%rcx, %rcx
	movl	%ecx, generating_concat_p(%rip)
	testq	%rax, %rax
	jne	.LBB18_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB18_3
.LBB18_1:
	movabsq	$0, %rax
.LBB18_3:                               # %entry
	movq	cfun(%rip), %rcx
	movl	%eax, cse_not_expected(%rip)
	movl	424(%rcx), %eax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB18_5
# BB#4:                                 # %if.then
	callq	identify_blocks
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
.LBB18_5:                               # %if.end
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB18_7
# BB#6:                                 # %if.then7
	callq	reorder_blocks
.LBB18_7:                               # %if.end8
	callq	init_flow
	cmpq	$0, 160(%r12)
	jne	.LBB18_49
# BB#8:                                 # %if.then10
	movq	current_function_decl(%rip), %rax
	movq	80(%rax), %rax
	jmp	.LBB18_9
	.align	16, 0x90
.LBB18_13:                              # %for.inc
                                        #   in Loop: Header=BB18_9 Depth=1
	movq	%rax, %rdi
	callq	get_containing_scope
.LBB18_9:                               # %if.then10
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB18_14
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB18_9 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB18_13
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB18_9 Depth=1
	movq	48(%rax), %rcx
	andq	$2304, %rcx             # imm = 0x900
	cmpq	$2304, %rcx             # imm = 0x900
	jne	.LBB18_13
# BB#12:                                # %if.then28
	movq	$0, 104(%r12)
	leaq	48(%r12), %r13
	jmp	.LBB18_221
.LBB18_14:                              # %for.end
	movzbl	49(%r12), %eax
	leaq	48(%r12), %r13
	movl	flag_no_inline(%rip), %edx
	andq	$8, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB18_15
# BB#16:                                # %for.end
	movabsq	$1, %rcx
	jmp	.LBB18_17
.LBB18_15:
	movabsq	$0, %rcx
.LBB18_17:                              # %for.end
	xorq	%r14, %r14
	testq	%rax, %rcx
	jne	.LBB18_19
# BB#18:                                # %for.end
	movl	flag_inline_functions(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_27
.LBB18_19:                              # %if.then41
	movq	$12, %rdi
	callq	timevar_push
	movq	%r12, %rdi
	callq	function_cannot_inline_p
	movq	%rax, %rbx
	movq	$12, %rdi
	callq	timevar_pop
	testq	%rbx, %rbx
	jne	.LBB18_21
# BB#20:                                # %if.then41
	movl	optimize(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_21
# BB#26:                                # %if.else
	orq	$2048, (%r13)           # imm = 0x800
	movq	$1, %r14
	jmp	.LBB18_27
.LBB18_21:                              # %if.then46
	movl	warn_inline(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_24
# BB#22:                                # %land.lhs.true48
	movzbl	1(%r13), %eax
	testq	$8, %rax
	je	.LBB18_24
# BB#23:                                # %if.then56
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	warning_with_decl
.LBB18_24:                              # %if.end57
	movq	$0, 112(%r12)
	movzbl	49(%r12), %eax
	testq	$1, %rax
	je	.LBB18_27
# BB#25:                                # %if.then66
	movq	$0, 104(%r12)
	jmp	.LBB18_221
.LBB18_27:                              # %if.end75
	callq	get_insns
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB18_31
# BB#28:                                # %if.then79
	cmpq	$0, 160(%r12)
	je	.LBB18_30
# BB#29:                                # %if.then84
	movq	rtl_dump_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB18_30:                              # %if.end86
	movabsq	$print_rtl, %rsi
	xorq	%rdi, %rdi
	movq	%rbx, %rdx
	callq	close_dump_file
.LBB18_31:                              # %if.end87
	callq	convert_from_eh_region_ranges
	testq	%r14, %r14
	jne	.LBB18_39
# BB#32:                                # %lor.lhs.false89
	movq	(%r13), %rax
	testq	$2048, %rax             # imm = 0x800
	je	.LBB18_40
# BB#33:                                # %land.lhs.true97
	movl	16(%r12), %ecx
	testq	$525312, %rcx           # imm = 0x80400
	jne	.LBB18_38
# BB#34:                                # %land.lhs.true109
	movq	120(%r12), %rax
	testq	%rax, %rax
	jne	.LBB18_36
# BB#35:                                # %cond.false
	movq	%r12, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r12), %rax
.LBB18_36:                              # %cond.end
	movl	16(%rax), %eax
	movl	flag_keep_inline_functions(%rip), %ecx
	andq	$262144, %rax           # imm = 0x40000
	orq	%rax, %rcx
	testq	%r15, %rcx
	je	.LBB18_39
# BB#37:                                # %cond.end.lor.lhs.false121_crit_edge
	movq	(%r13), %rax
.LBB18_38:                              # %lor.lhs.false121
	testq	$256, %rax              # imm = 0x100
	je	.LBB18_40
.LBB18_39:                              # %if.then129
	movq	(%r13), %rax
	orq	$262144, %rax           # imm = 0x40000
	movq	%rax, (%r13)
.LBB18_40:                              # %if.end134
	testq	$2048, %rax             # imm = 0x800
	je	.LBB18_42
# BB#41:                                # %if.then142
	movq	debug_hooks(%rip), %rax
	movq	%r12, %rdi
	callq	*136(%rax)
	movq	48(%r12), %rax
.LBB18_42:                              # %if.end143
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB18_48
# BB#43:                                # %if.then151
	movl	warn_return_type(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_45
# BB#44:                                # %if.then153
	movl	optimize(%rip), %r15d
	xorq	%rax, %rax
	movl	%eax, optimize(%rip)
	movq	%rbx, %rdi
	callq	rebuild_jump_labels
	callq	find_exception_handler_labels
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$24, %rdi
	callq	cleanup_cfg
	movl	%r15d, optimize(%rip)
	callq	free_bb_for_insn
.LBB18_45:                              # %if.end156
	callq	nothrow_function_p
	testq	%rax, %rax
	movl	%eax, current_function_nothrow(%rip)
	je	.LBB18_47
# BB#46:                                # %if.then159
	movq	current_function_decl(%rip), %rax
	movl	16(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 16(%rax)
.LBB18_47:                              # %if.end164
	movq	$12, %rdi
	callq	timevar_push
	movq	%r12, %rdi
	callq	save_for_inline
	movq	$12, %rdi
	callq	timevar_pop
	movq	160(%r12), %rax
	movl	%r14d, 352(%rax)
	jmp	.LBB18_221
.LBB18_48:                              # %if.end169
	testq	$256, %rax              # imm = 0x100
	jne	.LBB18_221
.LBB18_49:                              # %if.end179
	movq	current_function_decl(%rip), %rax
	movq	80(%rax), %rbx
	jmp	.LBB18_50
	.align	16, 0x90
.LBB18_55:                              # %for.inc209
                                        #   in Loop: Header=BB18_50 Depth=1
	movq	%rbx, %rdi
	callq	get_containing_scope
	movq	%rax, %rbx
.LBB18_50:                              # %if.end179
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB18_56
# BB#51:                                # %for.body186
                                        #   in Loop: Header=BB18_50 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB18_55
# BB#52:                                # %if.then193
                                        #   in Loop: Header=BB18_50 Depth=1
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB18_54
# BB#53:                                # %cond.false199
                                        #   in Loop: Header=BB18_50 Depth=1
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB18_54:                              # %cond.end200
                                        #   in Loop: Header=BB18_50 Depth=1
	movl	16(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 16(%rax)
	jmp	.LBB18_55
.LBB18_56:                              # %for.end211
	movzbl	49(%r12), %eax
	leaq	48(%r12), %r13
	testq	$8, %rax
	je	.LBB18_58
# BB#57:                                # %if.then219
	movq	debug_hooks(%rip), %rax
	movq	%r12, %rdi
	callq	*144(%rax)
.LBB18_58:                              # %if.end220
	callq	remove_unnecessary_notes
	callq	reorder_blocks
	callq	ggc_collect
	callq	init_function_for_compilation
	movzbl	2(%r13), %eax
	testq	$4, %rax
	jne	.LBB18_60
# BB#59:                                # %if.then228
	movl	16(%r12), %eax
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%r12)
.LBB18_60:                              # %if.end233
	xorq	%rax, %rax
	movl	%eax, rtx_equal_function_value_matters(%rip)
	callq	get_insns
	movq	%rax, %rdi
	callq	purge_hard_subreg_sets
	movl	rtl_dump_and_exit(%rip), %eax
	movl	flag_syntax_only(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rcx
	je	.LBB18_62
# BB#61:                                # %if.end233
	movl	warn_return_type(%rip), %eax
	testq	%r15, %rax
	je	.LBB18_221
.LBB18_62:                              # %lor.lhs.false240
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB18_221
# BB#63:                                # %lor.lhs.false242
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB18_221
# BB#64:                                # %if.end248
	movl	flag_optimize_sibling_calls(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_66
# BB#65:                                # %if.then250
	movq	$13, %rdi
	callq	timevar_push
	movq	$1, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	optimize_sibling_and_tail_recursive_calls
	callq	get_insns
	movabsq	$print_rtl, %rsi
	movq	$1, %rdi
	movq	%rax, %rdx
	callq	close_dump_file
	movq	$13, %rdi
	callq	timevar_pop
.LBB18_66:                              # %if.end253
	callq	find_exception_handler_labels
	xorq	%rdi, %rdi
	callq	doing_eh
	testq	%r15, %rax
	je	.LBB18_68
# BB#67:                                # %if.then256
	movq	$13, %rdi
	callq	timevar_push
	movq	$2, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	finish_eh_generation
	callq	get_insns
	movabsq	$print_rtl, %rsi
	movq	$2, %rdi
	movq	%rax, %rdx
	callq	close_dump_file
	movq	$13, %rdi
	callq	timevar_pop
.LBB18_68:                              # %if.end259
	callq	emit_initial_value_sets
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_70
# BB#69:                                # %if.then261
	movq	cfun(%rip), %rax
	movl	424(%rax), %edx
	movq	$7, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	andq	$2097152, %rsi          # imm = 0x200000
	orq	%rdx, %rsi
	movl	%esi, 424(%rax)
.LBB18_70:                              # %if.end271
	callq	get_insns
	movq	current_function_decl(%rip), %rdi
	movq	%rax, %r14
	movq	%r14, %rsi
	callq	unshare_all_rtl
	movq	current_function_decl(%rip), %rdi
	movq	%r14, %rsi
	callq	instantiate_virtual_regs
	movq	$3, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	$13, %rdi
	callq	timevar_push
	callq	expected_value_to_br_prob
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_71
# BB#72:                                # %if.end271
	movabsq	$1, %rdi
	jmp	.LBB18_73
.LBB18_71:
	movabsq	$0, %rdi
.LBB18_73:                              # %if.end271
	orq	$16, %rdi
	callq	cleanup_cfg
	callq	free_bb_for_insn
	movq	%r14, %rdi
	callq	copy_loop_headers
	movq	%r14, %rdi
	callq	purge_line_number_notes
	movq	$13, %rdi
	callq	timevar_pop
	movl	rtl_dump_and_exit(%rip), %eax
	movl	flag_syntax_only(%rip), %ecx
	orq	%rax, %rcx
	testq	%r15, %rcx
	jne	.LBB18_75
# BB#74:                                # %lor.lhs.false282
	movzbl	2(%r13), %eax
	testq	$4, %rax
	je	.LBB18_76
.LBB18_75:                              # %if.then290
	movabsq	$print_rtl, %rsi
	movq	$3, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
.LBB18_221:                             # %exit_rest_of_compilation
	xorq	%rax, %rax
	movl	%eax, reload_completed(%rip)
	movl	%eax, flow2_completed(%rip)
	movl	%eax, no_new_pseudos(%rip)
	movq	$42, %rdi
	callq	timevar_push
	callq	init_insn_lengths
	callq	clear_const_double_mem
	callq	init_temp_slots
	xorq	%rdi, %rdi
	callq	free_basic_block_vars
	callq	free_bb_for_insn
	movq	$42, %rdi
	callq	timevar_pop
	callq	init_recog_no_volatile
	movq	cfun(%rip), %rdi
	callq	free_after_parsing
	movzbl	2(%r13), %eax
	testq	$4, %rax
	jne	.LBB18_223
# BB#222:                               # %if.then670
	movq	cfun(%rip), %rdi
	callq	free_after_compilation
	movq	$0, 160(%r12)
.LBB18_223:                             # %if.end674
	movq	$0, cfun(%rip)
	callq	ggc_collect
	movq	$44, %rdi
	callq	timevar_pop
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_76:                              # %if.end291
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_83
# BB#77:                                # %if.end291
	movl	flag_ssa(%rip), %eax
	testq	%r15, %rax
	je	.LBB18_83
# BB#78:                                # %if.then296
	movq	$38, %rdi
	callq	timevar_push
	movq	$4, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$17, %rdi
	callq	cleanup_cfg
	callq	convert_to_ssa
	movabsq	$print_rtl_with_bb, %rsi
	movq	$4, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$38, %rdi
	callq	timevar_pop
	movl	flag_ssa_ccp(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_80
# BB#79:                                # %if.then301
	movq	$39, %rdi
	callq	timevar_push
	movq	$5, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	ssa_const_prop
	callq	get_insns
	movabsq	$print_rtl_with_bb, %rsi
	movq	$5, %rdi
	movq	%rax, %rdx
	callq	close_dump_file
	movq	$39, %rdi
	callq	timevar_pop
.LBB18_80:                              # %if.end304
	movl	flag_ssa_dce(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_82
# BB#81:                                # %if.then306
	movq	$40, %rdi
	callq	timevar_push
	movq	$6, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	get_insns
	movq	%rax, %r14
	callq	ssa_eliminate_dead_code
	movabsq	$print_rtl_with_bb, %rsi
	movq	$6, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$40, %rdi
	callq	timevar_pop
.LBB18_82:                              # %if.end309
	movq	$41, %rdi
	callq	timevar_push
	movq	$7, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	convert_from_ssa
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	movabsq	$print_rtl_with_bb, %rsi
	movq	$7, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$41, %rdi
	callq	timevar_pop
	callq	ggc_collect
	callq	free_bb_for_insn
.LBB18_83:                              # %if.end312
	movq	$13, %rdi
	callq	timevar_push
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_88
# BB#84:                                # %if.then315
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movl	flag_thread_jumps(%rip), %eax
	testq	%rax, %rax
	movq	$81, %rdi
	jne	.LBB18_86
# BB#85:                                # %select.mid
	movq	$17, %rdi
.LBB18_86:                              # %select.end
	callq	cleanup_cfg
	movq	$21, %rdi
	callq	timevar_push
	xorq	%rdi, %rdi
	callq	if_convert
	movq	$21, %rdi
	callq	timevar_pop
	callq	free_bb_for_insn
	movl	flag_delete_null_pointer_checks(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_88
# BB#87:                                # %if.then322
	movq	%r14, %rdi
	callq	delete_null_pointer_checks
.LBB18_88:                              # %if.end324
	movq	rtl_dump_file(%rip), %rdi
	callq	renumber_insns
	movq	$13, %rdi
	callq	timevar_pop
	movabsq	$print_rtl, %rsi
	movq	$3, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_103
# BB#89:                                # %if.then327
	movq	$8, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	$14, %rdi
	callq	timevar_push
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rcx
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cse_main
	movl	flag_rerun_cse_after_loop(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB18_93
# BB#90:                                # %land.rhs
	movl	flag_gcse(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB18_91
# BB#92:                                # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB18_93
.LBB18_91:
	movabsq	$0, %rbx
.LBB18_93:                              # %land.end
	movl	%ebx, cse_not_expected(%rip)
	testq	%r15, %rax
	jne	.LBB18_95
# BB#94:                                # %land.end
	movl	optimize(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB18_96
.LBB18_95:                              # %if.then340
	movq	$13, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$17, %rdi
	callq	cleanup_cfg
	movq	$13, %rdi
	callq	timevar_pop
	callq	free_bb_for_insn
.LBB18_96:                              # %if.end343
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	delete_trivially_dead_insns
	movl	flag_delete_null_pointer_checks(%rip), %eax
	movl	flag_thread_jumps(%rip), %ecx
	orq	%rax, %rcx
	testq	%r15, %rcx
	je	.LBB18_102
# BB#97:                                # %if.then348
	movq	$13, %rdi
	callq	timevar_push
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movl	flag_thread_jumps(%rip), %eax
	testq	%rax, %rax
	movq	$81, %rdi
	jne	.LBB18_99
# BB#98:                                # %select.mid19
	movq	$17, %rdi
.LBB18_99:                              # %select.end18
	callq	cleanup_cfg
	movl	flag_delete_null_pointer_checks(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_101
# BB#100:                               # %if.then355
	movq	%r14, %rdi
	callq	delete_null_pointer_checks
.LBB18_101:                             # %if.end356
	callq	free_bb_for_insn
	movq	$13, %rdi
	callq	timevar_pop
.LBB18_102:                             # %if.end357
	movq	rtl_dump_file(%rip), %rdi
	callq	renumber_insns
	movq	$14, %rdi
	callq	timevar_pop
	movabsq	$print_rtl, %rsi
	movq	$8, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
.LBB18_103:                             # %if.end358
	movq	$9, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	%r14, %rdi
	callq	purge_addressof
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	movabsq	$print_rtl, %rsi
	movq	$9, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_114
# BB#104:                               # %if.end358
	movl	flag_gcse(%rip), %ecx
	testq	%r15, %rcx
	je	.LBB18_114
# BB#105:                               # %if.then365
	movq	$15, %rdi
	callq	timevar_push
	movq	$10, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$17, %rdi
	callq	cleanup_cfg
	movq	rtl_dump_file(%rip), %rsi
	movq	%r14, %rdi
	callq	gcse_main
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	xorq	%rbx, %rbx
	movl	%ebx, flag_cse_follow_jumps(%rip)
	movl	%ebx, flag_cse_skip_blocks(%rip)
	callq	free_bb_for_insn
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_110
# BB#106:                               # %if.then371
	movq	$14, %rdi
	callq	timevar_push
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rcx
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cse_main
	movq	%rax, %rbx
	movq	$14, %rdi
	callq	timevar_pop
	movl	flag_rerun_cse_after_loop(%rip), %eax
	testq	%rax, %rax
	jne	.LBB18_107
# BB#108:                               # %if.then371
	movabsq	$1, %rax
	jmp	.LBB18_109
.LBB18_107:
	movabsq	$0, %rax
.LBB18_109:                             # %if.then371
	movl	%eax, cse_not_expected(%rip)
.LBB18_110:                             # %while.cond.preheader
	movq	(%rsp), %rax            # 8-byte Reload
	orq	%rbx, %rax
	testq	%r15, %rax
	je	.LBB18_113
.LBB18_111:                             # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$13, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	delete_trivially_dead_insns
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$17, %rdi
	callq	cleanup_cfg
	callq	free_bb_for_insn
	movq	$13, %rdi
	callq	timevar_pop
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_113
# BB#112:                               # %while.cond.backedge
                                        #   in Loop: Header=BB18_111 Depth=1
	movq	$14, %rdi
	callq	timevar_push
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rcx
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cse_main
	movq	%rax, %rbx
	movq	$14, %rdi
	callq	timevar_pop
	testq	%r15, %rbx
	jne	.LBB18_111
.LBB18_113:                             # %while.end
	movabsq	$print_rtl, %rsi
	movq	$10, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$15, %rdi
	callq	timevar_pop
	callq	ggc_collect
	movl	optimize(%rip), %eax
	movl	flag_cse_skip_blocks(%rip), %ecx
	movl	%ecx, flag_cse_skip_blocks(%rip)
	movl	flag_cse_follow_jumps(%rip), %ecx
	movl	%ecx, flag_cse_follow_jumps(%rip)
.LBB18_114:                             # %if.end390
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB18_127
# BB#115:                               # %if.then393
	movq	$16, %rdi
	callq	timevar_push
	movq	$11, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	free_bb_for_insn
	movl	flag_rerun_loop_opt(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_117
# BB#116:                               # %if.then396
	callq	cleanup_barriers
	movq	rtl_dump_file(%rip), %rsi
	movq	$8, %rdx
	movq	%r14, %rdi
	callq	loop_optimize
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	delete_trivially_dead_insns
	callq	max_reg_num
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
.LBB18_117:                             # %if.end406
	callq	cleanup_barriers
	movl	flag_unroll_loops(%rip), %eax
	movq	rtl_dump_file(%rip), %rsi
	testq	%rax, %rax
	je	.LBB18_118
# BB#119:                               # %if.end406
	movabsq	$1, %rdx
	jmp	.LBB18_120
.LBB18_118:
	movabsq	$0, %rdx
.LBB18_120:                             # %if.end406
	movl	flag_prefetch_loop_arrays(%rip), %eax
	testq	%rax, %rax
	movq	$4, %rax
	jne	.LBB18_122
# BB#121:                               # %select.mid21
	xorq	%rax, %rax
.LBB18_122:                             # %select.end20
	orq	%rax, %rdx
	orq	$2, %rdx
	movq	%r14, %rdi
	callq	loop_optimize
	movabsq	$print_rtl, %rsi
	movq	$11, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$16, %rdi
	callq	timevar_pop
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_127
# BB#123:                               # %if.then409
	movq	$17, %rdi
	callq	timevar_push
	movq	$12, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movl	flag_rerun_cse_after_loop(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_126
# BB#124:                               # %if.then412
	movq	$13, %rdi
	callq	timevar_push
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	delete_trivially_dead_insns
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	movq	$21, %rdi
	callq	timevar_push
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$1, %rdi
	callq	cleanup_cfg
	xorq	%rdi, %rdi
	callq	if_convert
	movq	$21, %rdi
	callq	timevar_pop
	movq	$13, %rdi
	callq	timevar_pop
	callq	free_bb_for_insn
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rcx
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cse_main
	testq	%r15, %rax
	je	.LBB18_126
# BB#125:                               # %if.then421
	movq	$13, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$1, %rdi
	callq	cleanup_cfg
	callq	free_bb_for_insn
	movq	$13, %rdi
	callq	timevar_pop
.LBB18_126:                             # %if.end425
	movabsq	$print_rtl, %rsi
	movq	$12, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$17, %rdi
	callq	timevar_pop
	callq	ggc_collect
.LBB18_127:                             # %if.end426
	movq	$1, %rax
	movl	%eax, cse_not_expected(%rip)
	callq	regclass_init
	movq	$19, %rdi
	callq	timevar_push
	movq	$13, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_128
# BB#129:                               # %if.end426
	movabsq	$1, %rax
	jmp	.LBB18_130
.LBB18_128:
	movabsq	$0, %rax
.LBB18_130:                             # %if.end426
	movl	flag_thread_jumps(%rip), %ecx
	testq	%rcx, %rcx
	movq	$64, %rdi
	jne	.LBB18_132
# BB#131:                               # %select.mid23
	xorq	%rdi, %rdi
.LBB18_132:                             # %select.end22
	orq	%rax, %rdi
	callq	cleanup_cfg
	callq	check_function_return_warnings
	callq	mark_constant_function
	movabsq	$print_rtl_with_bb, %rsi
	movq	$13, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movl	profile_arc_flag(%rip), %eax
	movl	flag_test_coverage(%rip), %ecx
	movl	flag_branch_probabilities(%rip), %edx
	orq	%rax, %rcx
	orq	%rcx, %rdx
	testq	%r15, %rdx
	je	.LBB18_134
# BB#133:                               # %if.then440
	movq	$18, %rdi
	callq	timevar_push
	movq	$14, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	branch_prob
	movabsq	$print_rtl_with_bb, %rsi
	movq	$14, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$18, %rdi
	callq	timevar_pop
.LBB18_134:                             # %if.end442
	movq	$15, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_140
# BB#135:                               # %if.then445
	leaq	8(%rsp), %rdi
	movq	$1, %rsi
	callq	flow_loops_find
	movl	flag_guess_branch_prob(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_137
# BB#136:                               # %if.then448
	leaq	8(%rsp), %rdi
	callq	estimate_probability
.LBB18_137:                             # %if.end449
	movq	rtl_dump_file(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB18_139
# BB#138:                               # %if.then451
	leaq	8(%rsp), %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	flow_loops_dump
.LBB18_139:                             # %if.end452
	leaq	8(%rsp), %rdi
	callq	flow_loops_free
.LBB18_140:                             # %if.end453
	movq	rtl_dump_file(%rip), %rsi
	movq	$127, %rdx
	movq	%r14, %rdi
	callq	life_analysis
	movq	$19, %rdi
	callq	timevar_pop
	movl	warn_uninitialized(%rip), %eax
	movl	extra_warnings(%rip), %ecx
	movq	$1, %rdx
	movl	%edx, no_new_pseudos(%rip)
	orq	%rax, %rcx
	testq	%r15, %rcx
	je	.LBB18_143
# BB#141:                               # %if.then457
	movq	104(%r12), %rdi
	callq	uninitialized_vars_warning
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_143
# BB#142:                               # %if.then461
	callq	setjmp_args_warning
.LBB18_143:                             # %if.end463
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_146
# BB#144:                               # %if.then465
	callq	initialize_uninitialized_subregs
	testq	%r15, %rax
	je	.LBB18_146
# BB#145:                               # %if.then468
	callq	get_insns
	movq	rtl_dump_file(%rip), %rsi
	movq	%rax, %r14
	movq	$7, %rdx
	movq	%r14, %rdi
	callq	life_analysis
.LBB18_146:                             # %if.end471
	movabsq	$print_rtl_with_bb, %rsi
	movq	$15, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_153
# BB#147:                               # %if.then474
	movq	$20, %rdi
	callq	timevar_push
	movq	$16, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	max_reg_num
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	combine_instructions
	movq	%rax, %rbx
	movq	$1, %rdi
	callq	purge_all_dead_edges
	orq	%rbx, %rax
	testq	%r15, %rax
	je	.LBB18_149
# BB#148:                               # %if.then482
	movq	$13, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	movq	$13, %rdi
	callq	timevar_pop
	movq	$33, %rdi
	callq	cleanup_cfg
.LBB18_149:                             # %if.end485
	movabsq	$print_rtl_with_bb, %rsi
	movq	$16, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$20, %rdi
	callq	timevar_pop
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_153
# BB#150:                               # %if.end490
	movq	$21, %rdi
	callq	timevar_push
	movq	$17, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	xorq	%rax, %rax
	movl	%eax, no_new_pseudos(%rip)
	movq	$1, %rdi
	movq	$1, %rbx
	callq	if_convert
	movl	%ebx, no_new_pseudos(%rip)
	movabsq	$print_rtl_with_bb, %rsi
	movq	$17, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$21, %rdi
	callq	timevar_pop
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_153
# BB#151:                               # %land.lhs.true493
	movl	flag_regmove(%rip), %eax
	movl	flag_expensive_optimizations(%rip), %ecx
	orq	%rax, %rcx
	testq	%r15, %rcx
	je	.LBB18_153
# BB#152:                               # %if.then497
	movq	$22, %rdi
	callq	timevar_push
	movq	$18, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	regmove_optimize
	movabsq	$print_rtl_with_bb, %rsi
	movq	$18, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$22, %rdi
	callq	timevar_pop
	callq	ggc_collect
.LBB18_153:                             # %if.end500
	movq	$1, %rdi
	callq	split_all_insns
	movq	$23, %rdi
	callq	timevar_push
	xorq	%rax, %rax
	movl	%eax, no_new_pseudos(%rip)
	xorq	%rdi, %rdi
	callq	optimize_mode_switching
	testq	%r15, %rax
	je	.LBB18_154
# BB#155:                               # %if.end500
	movabsq	$1, %rbx
	jmp	.LBB18_156
.LBB18_154:
	movabsq	$0, %rbx
.LBB18_156:                             # %if.end500
	movq	$1, %rax
	movl	%eax, no_new_pseudos(%rip)
	movq	$23, %rdi
	callq	timevar_pop
	movq	$24, %rdi
	callq	timevar_push
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_159
# BB#157:                               # %if.end500
	movl	flag_schedule_insns(%rip), %eax
	testq	%r15, %rax
	je	.LBB18_159
# BB#158:                               # %if.then509
	movq	$19, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	rtl_dump_file(%rip), %rdi
	callq	schedule_insns
	movabsq	$print_rtl_with_bb, %rsi
	movq	$19, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$1, %rbx
.LBB18_159:                             # %if.end511
	movq	$24, %rdi
	callq	timevar_pop
	callq	ggc_collect
	callq	leaf_function_p
	movl	%eax, current_function_is_leaf(%rip)
	movq	$25, %rdi
	callq	timevar_push
	movq	$20, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	testq	%rbx, %rbx
	jne	.LBB18_164
# BB#160:                               # %if.then515
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	jne	.LBB18_161
# BB#162:                               # %if.then515
	movabsq	$1, %rsi
	jmp	.LBB18_163
.LBB18_161:
	movabsq	$0, %rsi
.LBB18_163:                             # %if.then515
	movq	%r14, %rdi
	callq	recompute_reg_usage
.LBB18_164:                             # %if.end519
	movslq	max_regno(%rip), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	callq	allocate_reg_info
	movslq	max_regno(%rip), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, reg_equiv_memory_loc(%rip)
	movq	%rax, %rdi
	callq	allocate_initial_values
	callq	max_reg_num
	movq	rtl_dump_file(%rip), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	regclass
	callq	local_alloc
	movq	%rax, %rbx
	movq	$25, %rdi
	callq	timevar_pop
	movzbl	dump_file+330(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_166
# BB#165:                               # %if.then526
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	callq	dump_flow_info
	movq	rtl_dump_file(%rip), %rdi
	callq	dump_local_alloc
	movabsq	$print_rtl_with_bb, %rsi
	movq	$20, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$2, %rdi
	callq	timevar_pop
.LBB18_166:                             # %if.end527
	movq	%rbx, (%rsp)            # 8-byte Spill
	callq	ggc_collect
	movq	$26, %rdi
	callq	timevar_push
	movq	$21, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_168
# BB#167:                               # %if.then530
	movq	rtl_dump_file(%rip), %rdi
	callq	global_alloc
	jmp	.LBB18_169
.LBB18_168:                             # %if.else532
	movq	%r14, %rdi
	callq	build_insn_chain
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	reload
.LBB18_169:                             # %if.end534
	movq	%rax, %rbx
	movq	$26, %rdi
	callq	timevar_pop
	movzbl	dump_file+346(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_171
# BB#170:                               # %if.then536
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	callq	dump_global_regs
	movabsq	$print_rtl_with_bb, %rsi
	movq	$21, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$2, %rdi
	callq	timevar_pop
.LBB18_171:                             # %if.end537
	testq	%r15, %rbx
	jne	.LBB18_221
# BB#172:                               # %if.end540
	callq	ggc_collect
	movq	$22, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	movq	(%rsp), %rbx            # 8-byte Reload
	jle	.LBB18_174
# BB#173:                               # %if.then544
	movq	$27, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	reload_cse_regs
	movq	$27, %rdi
	callq	timevar_pop
.LBB18_174:                             # %if.end545
	testq	%r15, %rbx
	je	.LBB18_176
# BB#175:                               # %if.then547
	movq	$13, %rdi
	callq	timevar_push
	movq	%r14, %rdi
	callq	rebuild_jump_labels
	movq	$13, %rdi
	callq	timevar_pop
.LBB18_176:                             # %if.end548
	movabsq	$print_rtl_with_bb, %rsi
	movq	$22, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$28, %rdi
	callq	timevar_push
	movq	$23, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_178
# BB#177:                               # %if.then552
	xorq	%rdi, %rdi
	callq	split_all_insns
	movl	optimize(%rip), %eax
.LBB18_178:                             # %if.end553
	testq	%r15, %rax
	je	.LBB18_179
# BB#180:                               # %if.end553
	movabsq	$1, %rdi
	jmp	.LBB18_181
.LBB18_179:
	movabsq	$0, %rdi
.LBB18_181:                             # %if.end553
	callq	cleanup_cfg
	movq	%r14, %rdi
	callq	thread_prologue_and_epilogue_insns
	movslq	n_basic_blocks(%rip), %rsi
	cmpq	$1001, %rsi             # imm = 0x3E9
	movq	$3, %rdi
	jl	.LBB18_186
# BB#182:                               # %land.lhs.true559
	movslq	n_edges(%rip), %rax
	cqto
	idivq	%rsi
	movq	%rax, %rdx
	movq	$32, %rcx
                                        # kill: RAX<def> RDX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$20, %rax
	jl	.LBB18_186
# BB#183:                               # %if.then562
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rdi
	je	.LBB18_187
# BB#184:                               # %if.then562
	movl	warn_disabled_optimization(%rip), %ecx
	testq	%r15, %rcx
	je	.LBB18_187
# BB#185:                               # %if.then566
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rdi
.LBB18_186:                             # %if.end569thread-pre-split
	movl	optimize(%rip), %eax
.LBB18_187:                             # %if.end569
	testq	%r15, %rax
	je	.LBB18_189
# BB#188:                               # %if.then571
	callq	cleanup_cfg
	movq	rtl_dump_file(%rip), %rsi
	movq	$127, %rdx
	movq	%r14, %rdi
	callq	life_analysis
	callq	combine_stack_adjustments
	callq	ggc_collect
.LBB18_189:                             # %if.end574
	movq	$1, %rax
	movl	%eax, flow2_completed(%rip)
	movabsq	$print_rtl_with_bb, %rsi
	movq	$23, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$28, %rdi
	callq	timevar_pop
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_192
# BB#190:                               # %if.end574
	movl	flag_peephole2(%rip), %ecx
	testq	%r15, %rcx
	je	.LBB18_192
# BB#191:                               # %if.then579
	movq	$30, %rdi
	callq	timevar_push
	movq	$24, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	rtl_dump_file(%rip), %rdi
	callq	peephole2_optimize
	movabsq	$print_rtl_with_bb, %rsi
	movq	$24, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$30, %rdi
	callq	timevar_pop
	movl	optimize(%rip), %eax
.LBB18_192:                             # %if.end581
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB18_201
# BB#193:                               # %land.lhs.true584
	movl	flag_rename_registers(%rip), %ecx
	movl	flag_cprop_registers(%rip), %edx
	orq	%rcx, %rdx
	testq	%r15, %rdx
	je	.LBB18_199
# BB#194:                               # %if.then588
	movq	$31, %rdi
	callq	timevar_push
	movq	$25, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movl	flag_rename_registers(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_196
# BB#195:                               # %if.then591
	callq	regrename_optimize
.LBB18_196:                             # %if.end592
	movl	flag_cprop_registers(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_198
# BB#197:                               # %if.then594
	callq	copyprop_hardreg_forward
.LBB18_198:                             # %if.end595
	movabsq	$print_rtl_with_bb, %rsi
	movq	$25, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$31, %rdi
	callq	timevar_pop
	movl	optimize(%rip), %eax
.LBB18_199:                             # %if.end596
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB18_201
# BB#200:                               # %if.then599
	movq	$29, %rdi
	callq	timevar_push
	movq	$26, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	$1, %rdi
	callq	if_convert
	movabsq	$print_rtl_with_bb, %rsi
	movq	$26, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$29, %rdi
	callq	timevar_pop
	movl	optimize(%rip), %eax
.LBB18_201:                             # %if.end601
	testq	%r15, %rax
	je	.LBB18_205
# BB#202:                               # %if.end604
	movq	$1, %rdi
	callq	split_all_insns
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_205
# BB#203:                               # %if.end604
	movl	flag_schedule_insns_after_reload(%rip), %eax
	testq	%r15, %rax
	je	.LBB18_205
# BB#204:                               # %if.then609
	movq	$32, %rdi
	callq	timevar_push
	movq	$27, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	$1, %rdi
	callq	split_all_insns
	movq	rtl_dump_file(%rip), %rdi
	callq	schedule_insns
	movabsq	$print_rtl_with_bb, %rsi
	movq	$27, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$32, %rdi
	callq	timevar_pop
	callq	ggc_collect
.LBB18_205:                             # %if.end611
	movq	$37, %rdi
	callq	timevar_push
	movq	$28, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	rtl_dump_file(%rip), %rsi
	movq	%r14, %rdi
	callq	reg_to_stack
	movabsq	$print_rtl_with_bb, %rsi
	movq	$28, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$37, %rdi
	callq	timevar_pop
	callq	ggc_collect
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB18_209
# BB#206:                               # %if.then615
	movq	$35, %rdi
	callq	timevar_push
	movq	$29, %rdi
	movq	%r12, %rsi
	callq	open_dump_file
	movq	$5, %rdi
	callq	cleanup_cfg
	movl	flag_reorder_blocks(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_208
# BB#207:                               # %if.then619
	callq	reorder_basic_blocks
	movq	$5, %rdi
	callq	cleanup_cfg
.LBB18_208:                             # %if.end621
	movabsq	$print_rtl_with_bb, %rsi
	movq	$29, %rdi
	movq	%r14, %rdx
	callq	close_dump_file
	movq	$35, %rdi
	callq	timevar_pop
.LBB18_209:                             # %if.end622
	callq	compute_alignments
	callq	free_bb_for_insn
	movq	%r14, %rdi
	callq	purge_line_number_notes
	callq	cleanup_barriers
	callq	convert_to_eh_region_ranges
	movq	$36, %rdi
	callq	timevar_push
	callq	get_insns
	movq	%rax, %rdi
	callq	shorten_branches
	movq	$36, %rdi
	callq	timevar_pop
	callq	nothrow_function_p
	testq	%rax, %rax
	movl	%eax, current_function_nothrow(%rip)
	je	.LBB18_211
# BB#210:                               # %if.then627
	movq	current_function_decl(%rip), %rax
	movl	16(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 16(%rax)
.LBB18_211:                             # %if.end633
	movq	$42, %rdi
	callq	timevar_push
	movq	144(%r12), %rax
	testq	%rax, %rax
	jne	.LBB18_213
# BB#212:                               # %cond.false639
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB18_213:                             # %cond.end642
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB18_214
# BB#216:                               # %if.end649
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$68, %rcx
	jne	.LBB18_217
# BB#218:                               # %if.end656
	movq	8(%rax), %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	assemble_start_function
	movq	asm_out_file(%rip), %rsi
	movl	optimize(%rip), %edx
	movq	%r14, %rdi
	callq	final_start_function
	movq	asm_out_file(%rip), %rsi
	movl	optimize(%rip), %edx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	final
	callq	final_end_function
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	assemble_end_function
	callq	output_function_exception_table
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	jne	.LBB18_220
# BB#219:                               # %if.then660
	movq	asm_out_file(%rip), %rdi
	callq	fflush
.LBB18_220:                             # %if.end662
	xorq	%rdi, %rdi
	callq	free_basic_block_vars
	callq	regset_release_memory
	movq	$42, %rdi
	callq	timevar_pop
	callq	ggc_collect
	movq	$43, %rdi
	callq	timevar_push
	movq	debug_hooks(%rip), %rax
	movq	%r12, %rdi
	callq	*120(%rax)
	movq	$43, %rdi
	callq	timevar_pop
	jmp	.LBB18_221
.LBB18_214:                             # %if.then648
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rest_of_compilation, %rcx
	movq	$3494, %rsi             # imm = 0xDA6
	jmp	.LBB18_215
.LBB18_217:                             # %if.then655
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rest_of_compilation, %rcx
	movq	$3497, %rsi             # imm = 0xDA9
.LBB18_215:                             # %if.then648
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp154:
	.size	rest_of_compilation, .Ltmp154-rest_of_compilation
	.cfi_endproc

	.align	16, 0x90
	.type	open_dump_file,@function
open_dump_file:                         # @open_dump_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp163:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp164:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp166:
	.cfi_def_cfa_offset 64
.Ltmp167:
	.cfi_offset %rbx, -48
.Ltmp168:
	.cfi_offset %r12, -40
.Ltmp169:
	.cfi_offset %r13, -32
.Ltmp170:
	.cfi_offset %r14, -24
.Ltmp171:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r15, %r13
	movq	$4, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movzbl	dump_file+10(%rbx), %ecx
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB19_13
# BB#1:                                 # %if.end
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB19_3
# BB#2:                                 # %if.then1
	callq	fclose
.LBB19_3:                               # %if.end2
	leaq	dump_file(%rbx), %r12
	movabsq	$.L.str16, %rsi
	leaq	(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	sprintf
	movq	$4, %rcx
	movq	%r13, %r15
	shlq	%cl, %r15
	movzbl	dump_file+11(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_5
# BB#4:
	movabsq	$.L.str693, %r15
	jmp	.LBB19_9
.LBB19_5:                               # %if.then7
	movl	graph_dump_format(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_8
# BB#6:                                 # %land.lhs.true
	movq	$4, %rcx
	shlq	%cl, %r13
	movzbl	dump_file+9(%r13), %eax
	testq	%rax, %rax
	je	.LBB19_8
# BB#7:                                 # %if.then12
	movq	(%r12), %rsi
	leaq	(%rsp), %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	concat
	movq	dump_base_name(%rip), %rdi
	movq	%rax, %rbx
	movq	%rbx, %rsi
	callq	clean_graph_dump_file
	movq	%rbx, %rdi
	callq	free
.LBB19_8:                               # %if.end17
	leaq	dump_file+11(%r15), %rax
	movq	$1, %rcx
	movb	%cl, (%rax)
	movabsq	$.L.str22, %r15
.LBB19_9:                               # %if.end21
	movq	dump_base_name(%rip), %rdi
	movq	(%r12), %rdx
	leaq	(%rsp), %rsi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	callq	concat
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, rtl_dump_file(%rip)
	je	.LBB19_14
# BB#10:                                # %if.end31
	movq	%rbx, %rdi
	callq	free
	testq	%r14, %r14
	je	.LBB19_12
# BB#11:                                # %if.then33
	movq	rtl_dump_file(%rip), %rbx
	movq	$2, %rsi
	movq	%r14, %rdi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rcx
	movabsq	$.L.str694, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	fprintf
.LBB19_12:                              # %if.end36
	movq	$2, %rdi
	callq	timevar_pop
	movq	$1, %rax
.LBB19_13:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_14:                              # %if.then30
	movabsq	$.L.str350, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	fatal_io_error
.Ltmp172:
	.size	open_dump_file, .Ltmp172-open_dump_file
	.cfi_endproc

	.align	16, 0x90
	.type	close_dump_file,@function
close_dump_file:                        # @close_dump_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp179:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp180:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp181:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp182:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp183:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp184:
	.cfi_def_cfa_offset 64
.Ltmp185:
	.cfi_offset %rbx, -48
.Ltmp186:
	.cfi_offset %r12, -40
.Ltmp187:
	.cfi_offset %r13, -32
.Ltmp188:
	.cfi_offset %r14, -24
.Ltmp189:
	.cfi_offset %r15, -16
	cmpq	$0, rtl_dump_file(%rip)
	movq	%rdx, %r13
	movq	%rsi, %r14
	movq	%rdi, %r15
	je	.LBB20_9
# BB#1:                                 # %if.end
	movq	$2, %rdi
	callq	timevar_push
	testq	%r13, %r13
	je	.LBB20_5
# BB#2:                                 # %if.end
	movl	graph_dump_format(%rip), %eax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB20_5
# BB#3:                                 # %land.lhs.true2
	andq	%r15, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movzbl	dump_file+9(%rax), %eax
	testq	%rax, %rax
	je	.LBB20_5
# BB#4:                                 # %if.then4
	movabsq	$.L.str16, %rsi
	leaq	(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	sprintf
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	dump_file(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	concat
	movq	dump_base_name(%rip), %rdi
	movq	%rax, %r15
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	print_rtl_graph_with_bb
	movq	%r15, %rdi
	callq	free
.LBB20_5:                               # %if.end9
	testq	%r14, %r14
	je	.LBB20_8
# BB#6:                                 # %if.end9
	testq	%r13, %r13
	je	.LBB20_8
# BB#7:                                 # %if.then13
	movq	rtl_dump_file(%rip), %rdi
	movq	%r13, %rsi
	callq	*%r14
.LBB20_8:                               # %if.end14
	movq	rtl_dump_file(%rip), %rdi
	callq	fflush
	movq	rtl_dump_file(%rip), %rdi
	callq	fclose
	movq	$0, rtl_dump_file(%rip)
	movq	$2, %rdi
	callq	timevar_pop
.LBB20_9:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp190:
	.size	close_dump_file, .Ltmp190-close_dump_file
	.cfi_endproc

	.globl	toplev_main
	.align	16, 0x90
	.type	toplev_main,@function
toplev_main:                            # @toplev_main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp197:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp198:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp199:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp200:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp201:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp202:
	.cfi_def_cfa_offset 208
.Ltmp203:
	.cfi_offset %rbx, -48
.Ltmp204:
	.cfi_offset %r12, -40
.Ltmp205:
	.cfi_offset %r13, -32
.Ltmp206:
	.cfi_offset %r14, -24
.Ltmp207:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%rdi, %r14
	callq	hex_init
	movq	(%rbx), %rbx
	movq	%rbx, %rdi
	callq	strlen
	.align	16, 0x90
.LBB21_1:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB21_4
# BB#2:                                 # %land.rhs.i
                                        #   in Loop: Header=BB21_1 Depth=1
	movzbl	-1(%rbx,%rax), %ecx
	decq	%rax
	cmpq	$47, %rcx
	jne	.LBB21_1
# BB#3:                                 # %land.rhs.i.general_init.exit_crit_edge
	leaq	1(%rbx,%rax), %rbx
.LBB21_4:                               # %general_init.exit
	movq	%rbx, progname(%rip)
	movq	%rbx, %rdi
	callq	xmalloc_set_program_name
	movabsq	$float_signal, %rsi
	movq	$8, %rdi
	callq	signal
	movabsq	$crash_signal, %rbx
	movq	$11, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	$4, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	$7, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	$6, %rdi
	movq	%rbx, %rsi
	callq	signal
	movq	global_dc(%rip), %rdi
	callq	diagnostic_initialize
	movl	%r14d, save_argc(%rip)
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, save_argv(%rip)
	callq	init_reg_sets
	movabsq	$lang_independent_params, %rdi
	movq	$6, %rsi
	callq	add_params
	callq	*lang_hooks+16(%rip)
	cmpq	$2, %rbx
	jl	.LBB21_21
# BB#5:
	movq	$1, %rbx
	movabsq	$.L.str356, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB21_6:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_15 Depth 2
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB21_8
# BB#7:                                 # %if.then.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movq	$1, %rax
.LBB21_19:                              # %if.then30.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movl	%eax, optimize(%rip)
	xorq	%rax, %rax
	movl	%eax, optimize_size(%rip)
	jmp	.LBB21_20
	.align	16, 0x90
.LBB21_8:                               # %if.else.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movzbl	(%r15), %eax
	cmpq	$45, %rax
	jne	.LBB21_20
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movzbl	1(%r15), %eax
	cmpq	$79, %rax
	jne	.LBB21_20
# BB#10:                                # %if.then12.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movzbl	2(%r15), %ecx
	testq	%rcx, %rcx
	je	.LBB21_18
# BB#11:                                # %if.then12.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movq	$3, %rax
	cmpq	$115, %rcx
	jne	.LBB21_15
# BB#12:                                # %land.lhs.true20.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movzbl	3(%r15), %ecx
	testq	%rcx, %rcx
	movq	$115, %rcx
	jne	.LBB21_15
# BB#13:                                # %if.then25.i
                                        #   in Loop: Header=BB21_6 Depth=1
	movq	$1, %rax
	movl	%eax, optimize_size(%rip)
	movq	$2, %rax
	movl	%eax, optimize(%rip)
	jmp	.LBB21_20
	.align	16, 0x90
.LBB21_15:                              # %while.body.i.i
                                        #   Parent Loop BB21_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	testq	$4, %rcx
	je	.LBB21_16
# BB#14:                                # %while.cond.i.i
                                        #   in Loop: Header=BB21_15 Depth=2
	movzbl	(%r15,%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB21_15
.LBB21_18:                              # %read_integral_parameter.exit.i
                                        #   in Loop: Header=BB21_6 Depth=1
	addq	$2, %r15
	movq	%r15, %rdi
	callq	atoi
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	%r13, %rcx
	je	.LBB21_20
	jmp	.LBB21_19
.LBB21_16:                              # %if.then6.i.i
                                        #   in Loop: Header=BB21_6 Depth=1
	testq	%r15, %r15
	je	.LBB21_20
# BB#17:                                # %if.then9.i.i
                                        #   in Loop: Header=BB21_6 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdi
	movq	%r15, %rsi
	callq	error
	.align	16, 0x90
.LBB21_20:                              # %for.inc.i
                                        #   in Loop: Header=BB21_6 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	movq	%r14, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB21_6
.LBB21_21:                              # %for.end.i
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_22
# BB#23:                                # %if.end36.i
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%r14, %r15
	jle	.LBB21_29
# BB#24:                                # %if.end40.i
	movq	$1, %rdx
	movl	%edx, flag_defer_pop(%rip)
	movl	%edx, flag_thread_jumps(%rip)
	movl	%edx, flag_guess_branch_prob(%rip)
	movl	%edx, flag_cprop_registers(%rip)
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$2, %rsi
	jl	.LBB21_29
# BB#25:                                # %if.end44.i
	movl	%edx, flag_optimize_sibling_calls(%rip)
	movl	%edx, flag_cse_follow_jumps(%rip)
	movl	%edx, flag_cse_skip_blocks(%rip)
	movl	%edx, flag_gcse(%rip)
	movl	%edx, flag_expensive_optimizations(%rip)
	movl	%edx, flag_strength_reduce(%rip)
	movl	%edx, flag_rerun_cse_after_loop(%rip)
	movl	%edx, flag_rerun_loop_opt(%rip)
	movl	%edx, flag_caller_saves(%rip)
	movl	%edx, flag_force_mem(%rip)
	movl	%edx, flag_peephole2(%rip)
	movl	%edx, flag_schedule_insns(%rip)
	movl	%edx, flag_schedule_insns_after_reload(%rip)
	movl	%edx, flag_regmove(%rip)
	movl	%edx, flag_strict_aliasing(%rip)
	movl	%edx, flag_delete_null_pointer_checks(%rip)
	movl	%edx, flag_reorder_blocks(%rip)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB21_27
# BB#26:                                # %if.then47.i
	movq	$1, %rcx
	movl	%ecx, flag_inline_functions(%rip)
	movl	%ecx, flag_rename_registers(%rip)
.LBB21_27:                              # %if.end48.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB21_29
# BB#28:                                # %if.end48.i
	movl	optimize_size(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB21_30
	jmp	.LBB21_29
.LBB21_22:                              # %if.end36.thread.i
	xorq	%rax, %rax
	movl	%eax, flag_merge_constants(%rip)
	movq	%r14, %r15
.LBB21_29:                              # %if.then52.i
	movq	$1, %rax
	movl	%eax, align_loops(%rip)
	movl	%eax, align_jumps(%rip)
	movl	%eax, align_labels(%rip)
	movl	%eax, align_functions(%rip)
.LBB21_30:                              # %if.end53.i
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rax
	movl	%eax, flag_signed_char(%rip)
	xorq	%rax, %rax
	movl	%eax, target_flags(%rip)
	movabsq	$.L.str28, %rdi
	callq	set_target_switch
	movl	optimize_size(%rip), %esi
	movl	optimize(%rip), %edi
	callq	optimization_options
	cmpq	$2, %rbx
	jl	.LBB21_405
# BB#31:                                # %for.body57.lr.ph.i
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$1, %rsi
	jmp	.LBB21_32
.LBB21_37:                              # %if.end12.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	leaq	1(%rcx), %r15
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movabsq	$.L.str519, %rsi
	callq	strcmp
	movq	%r14, %rbx
	testq	%rbx, %rax
	jne	.LBB21_84
# BB#38:                                # %if.then13.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, 88(%rsp)          # 8-byte Spill
	movabsq	$.Lstr, %rdi
	callq	puts
	movabsq	$.Lstr695, %rdi
	callq	puts
	movabsq	$.Lstr696, %rdi
	callq	puts
	movabsq	$.Lstr697, %rdi
	callq	puts
	movabsq	$.Lstr698, %rdi
	callq	puts
	movabsq	$.Lstr699, %rdi
	callq	puts
	movq	$3104, %rbx             # imm = 0xC20
	movabsq	$.L.str624, %r14
	.align	16, 0x90
.LBB21_39:                              # %for.body.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	f_options-8(%rbx), %rdx
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	je	.LBB21_41
# BB#40:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB21_39 Depth=2
	movq	f_options-32(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
.LBB21_41:                              # %for.cond.backedge.i.i.i
                                        #   in Loop: Header=BB21_39 Depth=2
	addq	$-32, %rbx
	jne	.LBB21_39
# BB#42:                                # %for.end.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.Lstr700, %rdi
	callq	puts
	movabsq	$.Lstr701, %rdi
	callq	puts
	movq	compiler_params(%rip), %r14
	movq	120(%r14), %rdi
	movq	136(%r14), %rbx
	movq	%rdi, %r15
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_47
# BB#43:                                # %land.lhs.true24.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_47
# BB#44:                                # %if.then28.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_46
# BB#45:                                # %if.then28.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_46:                              # %if.then28.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	compiler_params(%rip), %r14
.LBB21_47:                              # %for.cond13.backedge.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	96(%r14), %r15
	movq	112(%r14), %rbx
	movq	%r15, %rdi
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_52
# BB#48:                                # %land.lhs.true24.1.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_52
# BB#49:                                # %if.then28.1.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_51
# BB#50:                                # %if.then28.1.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_51:                              # %if.then28.1.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	compiler_params(%rip), %r14
.LBB21_52:                              # %for.cond13.backedge.1.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	72(%r14), %r15
	movq	88(%r14), %rbx
	movq	%r15, %rdi
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_57
# BB#53:                                # %land.lhs.true24.2.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_57
# BB#54:                                # %if.then28.2.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_56
# BB#55:                                # %if.then28.2.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_56:                              # %if.then28.2.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	compiler_params(%rip), %r14
.LBB21_57:                              # %for.cond13.backedge.2.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	48(%r14), %r15
	movq	64(%r14), %rbx
	movq	%r15, %rdi
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_62
# BB#58:                                # %land.lhs.true24.3.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_62
# BB#59:                                # %if.then28.3.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_61
# BB#60:                                # %if.then28.3.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_61:                              # %if.then28.3.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	compiler_params(%rip), %r14
.LBB21_62:                              # %for.cond13.backedge.3.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	24(%r14), %r15
	movq	40(%r14), %rbx
	movq	%r15, %rdi
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_67
# BB#63:                                # %land.lhs.true24.4.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_67
# BB#64:                                # %if.then28.4.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_66
# BB#65:                                # %if.then28.4.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_66:                              # %if.then28.4.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	compiler_params(%rip), %r14
.LBB21_67:                              # %for.cond13.backedge.4.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	(%r14), %r15
	movq	16(%r14), %rbx
	movq	%r15, %rdi
	callq	strlen
	testq	%rbx, %rbx
	je	.LBB21_72
# BB#68:                                # %land.lhs.true24.5.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_72
# BB#69:                                # %if.then28.5.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jle	.LBB21_71
# BB#70:                                # %if.then28.5.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rdx
.LBB21_71:                              # %if.then28.5.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str627, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str628, %rcx
	movq	%rbx, %r8
	callq	printf
.LBB21_72:                              # %for.cond13.backedge.5.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.Lstr702, %rdi
	callq	puts
	movabsq	$.Lstr703, %rdi
	callq	puts
	movabsq	$.Lstr704, %rdi
	callq	puts
	movabsq	$.Lstr705, %rdi
	callq	puts
	movq	$608, %rbx              # imm = 0x260
	movabsq	$.L.str633, %r14
	.align	16, 0x90
.LBB21_73:                              # %for.body43.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	W_options-8(%rbx), %rdx
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	je	.LBB21_75
# BB#74:                                # %if.then53.i.i.i
                                        #   in Loop: Header=BB21_73 Depth=2
	movq	W_options-32(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
.LBB21_75:                              # %for.cond40.backedge.i.i.i
                                        #   in Loop: Header=BB21_73 Depth=2
	addq	$-32, %rbx
	jne	.LBB21_73
# BB#76:                                # %for.end58.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.Lstr706, %rdi
	callq	puts
	movabsq	$.Lstr707, %rdi
	callq	puts
	movabsq	$.Lstr708, %rdi
	callq	puts
	movabsq	$.Lstr709, %rdi
	callq	puts
	movabsq	$.Lstr710, %rdi
	callq	puts
	movq	$5, %rax
	.align	16, 0x90
.LBB21_77:                              # %for.body67.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	$4, %rax
	cmpq	$5, %rbx
	je	.LBB21_77
# BB#78:                                # %for.cond64.backedge.i.i.i
                                        #   in Loop: Header=BB21_77 Depth=2
	leaq	(%rbx,%rbx,2), %rax
	movq	debug_args+16(,%rax,8), %rdx
	movq	debug_args(,%rax,8), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str639, %rdi
	callq	printf
	testq	%rbx, %rbx
	leaq	-1(%rbx), %rax
	jne	.LBB21_77
# BB#79:                                # %for.end78.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.Lstr711, %rdi
	callq	puts
	movabsq	$.Lstr712, %rdi
	callq	puts
	movabsq	$.Lstr713, %rdi
	callq	puts
	movabsq	$.Lstr714, %rdi
	callq	puts
	movabsq	$.Lstr715, %rdi
	callq	puts
	movabsq	$.Lstr716, %rdi
	callq	puts
	movabsq	$.Lstr717, %rdi
	callq	puts
	movabsq	$.Lstr718, %rdi
	callq	puts
	movq	$-1728, %rbx            # imm = 0xFFFFFFFFFFFFF940
	.align	16, 0x90
.LBB21_80:                              # %for.body90.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	documented_lang_options+1736(%rbx), %rdx
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	je	.LBB21_82
# BB#81:                                # %if.else117.i.i.i
                                        #   in Loop: Header=BB21_80 Depth=2
	movq	documented_lang_options+1728(%rbx), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str652, %rdi
	callq	printf
.LBB21_82:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB21_80 Depth=2
	addq	$16, %rbx
	jne	.LBB21_80
# BB#83:                                # %display_help.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	callq	display_target_options
	movq	$1, %rax
	movb	%al, exit_after_options(%rip)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	88(%rsp), %r15          # 8-byte Reload
.LBB21_84:                              # %if.end14.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, %rdi
	movabsq	$.L.str520, %rsi
	callq	strcmp
	testq	%rbx, %rax
	movq	%r15, %r14
	jne	.LBB21_86
# BB#85:                                # %if.then17.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	callq	display_target_options
	movq	$1, %rax
	movb	%al, exit_after_options(%rip)
.LBB21_86:                              # %if.end18.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdi
	movabsq	$.L.str32, %rsi
	callq	strcmp
	testq	%rbx, %rax
	jne	.LBB21_88
# BB#87:                                # %if.then21.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$.L.str344, 16(%rsp)
	movq	$.L.str28, 8(%rsp)
	xorq	%rax, %rax
	movq	$.L.str343, (%rsp)
	movq	stderr(%rip), %rdi
	movabsq	$.L.str342, %rsi
	movabsq	$.L.str28, %rdx
	movq	%rdx, %rcx
	movq	lang_hooks(%rip), %r8
	movq	version_string(%rip), %r9
	callq	fnotice
	movq	$1, %rax
	movb	%al, exit_after_options(%rip)
.LBB21_88:                              # %if.end22.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdi
	movabsq	$.L.str521, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_103
# BB#89:                                # %if.then26.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %r12
	movq	%rsi, %r14
	cmpq	$1, %r12
	jne	.LBB21_91
# BB#90:                                # %if.then29.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str522, %rdi
	callq	error
	movq	$1, %rbx
	jmp	.LBB21_95
.LBB21_103:                             # %if.end43.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	96(%rsp), %rcx          # 8-byte Reload
	movzbl	1(%rcx), %eax
	cmpq	$89, %rax
	jne	.LBB21_105
# BB#104:                               #   in Loop: Header=BB21_32 Depth=1
	addq	$2, %rcx
	movq	%rcx, %r14
.LBB21_105:                             # %select.mid
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	104(%rsp), %rdx         # 8-byte Reload
	movsbq	(%r14), %rax
	cmpq	$78, %rax
	movq	112(%rsp), %r15         # 8-byte Reload
	movabsq	$0, %rbx
	jg	.LBB21_109
# BB#106:                               # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %rax
	cmpq	$71, %rax
	jne	.LBB21_389
# BB#107:                               # %sw.bb160.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	jne	.LBB21_108
# BB#371:                               # %if.then165.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %r12
	cmpq	$1, %r12
	je	.LBB21_389
# BB#372:                               # %if.end169.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r12
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r13,8), %rdi
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB21_377
# BB#373:                               # %while.body.i336.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	leaq	1(%rdi), %rcx
	.align	16, 0x90
.LBB21_375:                             # %while.body.i336.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_376
# BB#374:                               # %while.cond.i328.i.i
                                        #   in Loop: Header=BB21_375 Depth=2
	movzbl	(%rcx), %eax
	incq	%rcx
	testq	%rax, %rax
	jne	.LBB21_375
.LBB21_377:                             # %if.end11.i339.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, %r14
	callq	atoi
	movq	$2, %rcx
	jmp	.LBB21_382
.LBB21_91:                              # %if.end30.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r13,8), %r15
	movq	$61, %rsi
	movq	%r15, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB21_96
# BB#92:                                # %if.then34.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str523, %rdi
	movq	%r15, %rsi
	jmp	.LBB21_93
.LBB21_109:                             # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	cmpq	$86, %rax
	jg	.LBB21_112
# BB#110:                               # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	andq	%rsi, %rax
	cmpq	$79, %rax
	je	.LBB21_386
# BB#111:                               #   in Loop: Header=BB21_32 Depth=1
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_389
.LBB21_96:                              # %if.else.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movzbl	1(%rax), %ecx
	leaq	1(%rax), %r12
	testq	%rcx, %rcx
	je	.LBB21_101
# BB#97:                                # %while.body.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	addq	$2, %rax
	.align	16, 0x90
.LBB21_99:                              # %while.body.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	testq	$4, %rcx
	je	.LBB21_100
# BB#98:                                # %while.cond.i.i.i
                                        #   in Loop: Header=BB21_99 Depth=2
	movzbl	(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB21_99
.LBB21_101:                             # %read_integral_parameter.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %rdi
	callq	atoi
	movq	%rax, %rcx
	movq	%r14, %rbx
	andq	%rbx, %rcx
	cmpq	%rbx, %rcx
                                        # kill: R14<def> RBX<kill>
	jne	.LBB21_102
.LBB21_100:                             # %if.else39.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str524, %rdi
	movq	%r12, %rsi
.LBB21_93:                              # %if.end69.i
                                        #   in Loop: Header=BB21_32 Depth=1
	callq	error
	jmp	.LBB21_94
.LBB21_112:                             # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	cmpq	$96, %rax
	jg	.LBB21_117
# BB#113:                               # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %rax
	cmpq	$87, %rax
	jne	.LBB21_389
# BB#114:                               # %sw.bb108.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rax
	movzbl	1(%rax), %ecx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB21_330
# BB#115:                               # %if.then113.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movl	warn_uninitialized(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, extra_warnings(%rip)
	cmpq	$1, %rax
	movq	112(%rsp), %r15         # 8-byte Reload
	je	.LBB21_386
# BB#116:                               # %if.then116.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$2, %rax
	movl	%eax, warn_uninitialized(%rip)
	jmp	.LBB21_386
.LBB21_108:                             # %while.body.i352.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r12
	leaq	1(%r14), %rdi
	addq	$2, %r14
	.align	16, 0x90
.LBB21_379:                             # %while.body.i352.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_380
# BB#378:                               # %while.cond.i344.i.i
                                        #   in Loop: Header=BB21_379 Depth=2
	movzbl	(%r14), %eax
	incq	%r14
	testq	%rax, %rax
	jne	.LBB21_379
# BB#381:                               # %if.end11.i355.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, %r14
	callq	atoi
	movq	$1, %rcx
.LBB21_382:                             # %if.end175.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdx
	movq	%r12, %rsi
	andq	%rsi, %rdx
	cmpq	%rsi, %rdx
	movabsq	$0, %rbx
	jne	.LBB21_384
# BB#383:                               #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdx
	jmp	.LBB21_389
.LBB21_117:                             # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	addq	$-97, %rax
	andq	%rsi, %rax
	cmpq	$22, %rax
	ja	.LBB21_389
# BB#118:                               # %select.end
                                        #   in Loop: Header=BB21_32 Depth=1
	jmpq	*.LJTI21_0(,%rax,8)
.LBB21_361:                             # %sw.bb122.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	$8, %rdx
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str530, %rsi
	callq	strncmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_362
# BB#363:                               # %if.then125.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	8(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB21_364
# BB#367:                               # %if.then141.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	addq	$9, %r14
	movq	%r14, aux_info_file_name(%rip)
	movq	$1, %rax
	movl	%eax, flag_gen_aux_info(%rip)
	jmp	.LBB21_386
.LBB21_102:                             # %if.then38.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	set_param_value
.LBB21_94:                              # %if.end69.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$2, %rbx
.LBB21_95:                              # %if.end69.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %r15         # 8-byte Reload
	jmp	.LBB21_389
.LBB21_380:                             #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %rsi
	jmp	.LBB21_389
.LBB21_384:                             # %if.else179.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rdx
	movl	%edx, g_switch_set(%rip)
	movl	%eax, g_switch_value(%rip)
	movq	%rcx, %rbx
	movq	%r14, %rdx
	jmp	.LBB21_389
.LBB21_330:                             # %if.else118.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	leaq	1(%rax), %r15
	movq	%r15, 80(%rsp)          # 8-byte Spill
	leaq	4(%rax), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$18, %r12
	movabsq	$W_options+592, %rbx
	movq	%rsi, %r13
	.align	16, 0x90
.LBB21_331:                             # %for.body.i282.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-16(%rbx), %r14
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	strcmp
	testq	%r13, %rax
	movq	%r13, %rsi
	je	.LBB21_332
# BB#333:                               # %if.end.i287.i.i
                                        #   in Loop: Header=BB21_331 Depth=2
	cmpq	$110, 96(%rsp)          # 8-byte Folded Reload
	jne	.LBB21_336
# BB#334:                               # %land.lhs.true.i289.i.i
                                        #   in Loop: Header=BB21_331 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	cmpq	$111, %rax
	jne	.LBB21_336
# BB#335:                               # %land.lhs.true12.i291.i.i
                                        #   in Loop: Header=BB21_331 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	3(%rax), %eax
	cmpq	$45, %rax
	jne	.LBB21_336
# BB#339:                               # %land.lhs.true17.i297.i.i
                                        #   in Loop: Header=BB21_331 Depth=2
	movq	%rsi, %r15
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	strcmp
	movq	%r15, %rsi
	movq	80(%rsp), %r15          # 8-byte Reload
	testq	%rsi, %rax
	je	.LBB21_340
	.align	16, 0x90
.LBB21_336:                             # %for.cond.backedge.i294.i.i
                                        #   in Loop: Header=BB21_331 Depth=2
	addq	$-32, %rbx
	testq	%rsi, %r12
	leaq	-1(%r12), %r12
	movq	%rsi, %r13
	jne	.LBB21_331
# BB#337:                               # %for.end.i302.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$9, %rdx
	movq	%r15, %rdi
	movabsq	$.L.str531, %rsi
	callq	strncmp
	testq	%r13, %rax
	jne	.LBB21_341
# BB#338:                               # %if.then36.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str532, %rdi
	callq	warning
	movq	$1, %rbx
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	%r13, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_389
.LBB21_376:                             #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %rsi
	jmp	.LBB21_389
.LBB21_332:                             # %if.then.i285.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	(%rbx), %eax
	movq	-8(%rbx), %rcx
	movl	%eax, (%rcx)
	jmp	.LBB21_242
.LBB21_341:                             # %if.else.i305.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$12, %rdx
	movq	%r15, %rdi
	movq	%r15, %rbx
	movabsq	$.L.str533, %rsi
	callq	strncmp
	testq	%r13, %rax
	movq	%r13, %r14
	je	.LBB21_342
# BB#343:                               # %if.else.i305.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rdi, %rdi
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB21_344
.LBB21_288:                             # %sw.bb58.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str525, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_291
# BB#289:                               # %if.then61.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %r12
	cmpq	$1, %r12
	movq	104(%rsp), %rdx         # 8-byte Reload
	movabsq	$0, %rbx
	je	.LBB21_389
# BB#290:                               # %if.end65.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r13,8), %rax
	movq	%rax, dump_base_name(%rip)
	movq	$2, %rbx
	jmp	.LBB21_389
.LBB21_119:                             # %sw.bb52.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	leaq	1(%r14), %r12
	leaq	4(%r14), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r14, 88(%rsp)          # 8-byte Spill
	movq	$96, %r15
	movabsq	$f_options+3088, %r13
	movq	%rsi, %rbx
.LBB21_120:                             # %for.body.i232.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-16(%r13), %r14
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	strcmp
	testq	%rbx, %rax
	movq	%rbx, %rsi
	je	.LBB21_121
# BB#122:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB21_120 Depth=2
	movzbl	(%r12), %eax
	cmpq	$110, %rax
	jne	.LBB21_125
# BB#123:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB21_120 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	cmpq	$111, %rax
	jne	.LBB21_125
# BB#124:                               # %land.lhs.true12.i.i.i
                                        #   in Loop: Header=BB21_120 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	3(%rax), %eax
	cmpq	$45, %rax
	jne	.LBB21_125
# BB#128:                               # %land.lhs.true17.i.i.i
                                        #   in Loop: Header=BB21_120 Depth=2
	movq	%rsi, %rbx
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	je	.LBB21_129
.LBB21_125:                             # %for.cond.backedge.i235.i.i
                                        #   in Loop: Header=BB21_120 Depth=2
	addq	$-32, %r13
	testq	%rsi, %r15
	leaq	-1(%r15), %r15
	movq	%rsi, %rbx
	jne	.LBB21_120
# BB#126:                               # %for.end.i236.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %r14
	movq	%r14, %rdi
	movabsq	$.L.str593, %rsi
	callq	strcmp
	testq	%rbx, %rax
	movq	%rbx, %rsi
	jne	.LBB21_133
# BB#127:                               # %if.then34.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rcx, %rcx
	movl	%ecx, flag_trapping_math(%rip)
	movq	$1, %rax
	movl	%eax, flag_unsafe_math_optimizations(%rip)
	movl	%ecx, flag_errno_math(%rip)
	jmp	.LBB21_242
.LBB21_243:                             # %sw.bb55.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	leaq	1(%r14), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%r14, %r15
	movq	$debug_args, da(%rip)
	movabsq	$debug_args+24, %r13
	movabsq	$.L.str28, %rbx
.LBB21_244:                             # %for.body.i242.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	je	.LBB21_246
# BB#245:                               # %lor.lhs.false.i243.i.i
                                        #   in Loop: Header=BB21_244 Depth=2
	movq	%rdi, %r14
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	strncmp
	movq	%r14, %rdi
	testq	%rdi, %rax
	jne	.LBB21_282
.LBB21_246:                             # %if.then.i245.i.i
                                        #   in Loop: Header=BB21_244 Depth=2
	movzbl	1(%r12,%r15), %eax
	movl	-16(%r13), %r14d
	leaq	1(%r12,%r15), %rbx
	testq	%rax, %rax
	je	.LBB21_252
# BB#247:                               # %land.lhs.true.i248.i.i
                                        #   in Loop: Header=BB21_244 Depth=2
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	jne	.LBB21_248
.LBB21_282:                             # %for.inc.i264.i.i
                                        #   in Loop: Header=BB21_244 Depth=2
	movq	(%r13), %rbx
	movq	%r13, da(%rip)
	addq	$24, %r13
	testq	%rbx, %rbx
	jne	.LBB21_244
# BB#283:                               # %decode_g_option.exit.i.i.loopexit
                                        #   in Loop: Header=BB21_32 Depth=1
	addq	$-24, %r13
	movq	112(%rsp), %r14         # 8-byte Reload
.LBB21_284:                             # %decode_g_option.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_285
.LBB21_385:                             # %sw.bb.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	incq	%r14
	movq	%rsi, %rbx
	movq	%r14, %rdi
	callq	set_target_switch
	movq	%rbx, %rsi
	jmp	.LBB21_386
.LBB21_368:                             # %sw.bb148.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	jne	.LBB21_389
# BB#369:                               # %sw.bb148.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %r12
	cmpq	$1, %r12
	je	.LBB21_389
# BB#370:                               # %if.end157.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r13,8), %rax
	movq	%rax, asm_file_name(%rip)
	movq	$2, %rbx
	jmp	.LBB21_389
.LBB21_318:                             # %sw.bb70.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str526, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_320
# BB#319:                               # %if.then73.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movl	%eax, pedantic(%rip)
	jmp	.LBB21_386
.LBB21_324:                             # %sw.bb88.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str528, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_362
# BB#325:                               # %if.then91.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movl	%eax, quiet_flag(%rip)
	jmp	.LBB21_386
.LBB21_326:                             # %sw.bb94.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str529, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_362
# BB#327:                               # %if.then97.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movb	%al, version_flag(%rip)
	jmp	.LBB21_386
.LBB21_328:                             # %sw.bb100.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	jne	.LBB21_389
# BB#329:                               # %if.then105.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	$1, %rax
	movl	%eax, inhibit_warnings(%rip)
	jmp	.LBB21_386
.LBB21_342:                             #   in Loop: Header=BB21_32 Depth=1
	movq	88(%rsp), %rcx          # 8-byte Reload
	leaq	13(%rcx), %rdi
	movq	112(%rsp), %r15         # 8-byte Reload
.LBB21_344:                             # %if.else.i305.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rdi, %rdi
	jne	.LBB21_345
# BB#354:                               # %if.else51.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, %rdi
	movabsq	$.L.str534, %rsi
	callq	strcmp
	movq	%r14, %rsi
	testq	%rsi, %rax
	jne	.LBB21_358
# BB#355:                               # %if.then54.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	warn_unused_parameter(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, warn_unused_function(%rip)
	movl	%ecx, warn_unused_label(%rip)
	testq	%rax, %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	jne	.LBB21_357
# BB#356:                               # %if.then2.i.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	%esi, warn_unused_parameter(%rip)
.LBB21_357:                             # %set_Wunused.exit.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rbx
	movl	%ebx, warn_unused_variable(%rip)
	movl	%ebx, warn_unused_value(%rip)
	jmp	.LBB21_389
.LBB21_345:                             # %if.then45.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB21_350
# BB#346:                               # %while.body.i.i316.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	leaq	-1(%rcx), %rsi
	addq	$14, %rcx
.LBB21_348:                             # %while.body.i.i316.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_349
# BB#347:                               # %while.cond.i.i308.i.i
                                        #   in Loop: Header=BB21_348 Depth=2
	movzbl	(%rcx), %eax
	incq	%rcx
	testq	%rax, %rax
	jne	.LBB21_348
.LBB21_350:                             # %if.end11.i.i319.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	callq	atoi
	movq	%r14, %rsi
	jmp	.LBB21_351
.LBB21_291:                             # %if.else67.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%r14), %ebx
	testq	%rbx, %rbx
	je	.LBB21_386
# BB#292:                               #   in Loop: Header=BB21_32 Depth=1
	incq	%r14
	jmp	.LBB21_293
.LBB21_320:                             # %if.else74.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str527, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_322
# BB#321:                               # %if.then77.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movl	%eax, pedantic(%rip)
	movl	%eax, flag_pedantic_errors(%rip)
	jmp	.LBB21_386
.LBB21_121:                             # %if.then.i233.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	(%r13), %eax
.LBB21_131:                             # %if.then23.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	-8(%r13), %rcx
	movl	%eax, (%rcx)
	jmp	.LBB21_242
.LBB21_340:                             # %if.then23.i299.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	-8(%rbx), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB21_242
.LBB21_252:                             # %if.else.i259.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	decode_g_option.level(%rip), %ecx
	testq	%rcx, %rcx
	movq	$2, %rax
	je	.LBB21_254
# BB#253:                               # %if.else.i259.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rcx, %rax
	jmp	.LBB21_254
.LBB21_358:                             # %if.else55.i324.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r14
	movq	%rbx, %rdi
	movabsq	$.L.str535, %rsi
	callq	strcmp
	movq	%r14, %rsi
	xorq	%rbx, %rbx
	testq	%rsi, %rax
	jne	.LBB21_359
# BB#360:                               # %if.then58.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movl	%eax, warn_unused_function(%rip)
	movl	%eax, warn_unused_label(%rip)
	movl	%eax, warn_unused_parameter(%rip)
	movl	%eax, warn_unused_variable(%rip)
	movl	%eax, warn_unused_value(%rip)
	jmp	.LBB21_386
.LBB21_248:                             # %while.cond.i.i250.lr.ph.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	leaq	2(%r12,%r15), %rdx
.LBB21_249:                             # %while.cond.i.i250.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB21_251
# BB#250:                               # %while.cond.i.while.body.i_crit_edge.i.i.i
                                        #   in Loop: Header=BB21_249 Depth=2
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	incq	%rdx
	testq	$4, %rcx
	movq	$4, %rax
	jne	.LBB21_249
	jmp	.LBB21_254
.LBB21_364:                             # %if.then125.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_362
# BB#365:                               # %if.then130.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %r12
	cmpq	$1, %r12
	movq	104(%rsp), %rdx         # 8-byte Reload
	movabsq	$0, %rbx
	je	.LBB21_389
# BB#366:                               # %if.end134.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r13,8), %rax
	movq	%rax, aux_info_file_name(%rip)
	movq	$1, %rax
	movl	%eax, flag_gen_aux_info(%rip)
	movq	$2, %rbx
	jmp	.LBB21_389
.LBB21_322:                             # %if.else78.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%r14), %eax
	testq	%rax, %rax
	jne	.LBB21_362
# BB#323:                               # %if.then83.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movl	%eax, profile_flag(%rip)
	jmp	.LBB21_386
.LBB21_133:                             # %if.else.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str594, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_135
# BB#134:                               # %if.then37.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rax
	movl	%eax, flag_trapping_math(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, flag_unsafe_math_optimizations(%rip)
	movl	%eax, flag_errno_math(%rip)
	jmp	.LBB21_242
.LBB21_349:                             # %if.then6.i.i317.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdi
	callq	error
	movq	%r14, %rsi
	movq	%rsi, %rax
.LBB21_351:                             # %read_integral_parameter.exit.i321.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	%rsi, %rax
	cmpq	%rsi, %rax
	movq	%rdx, larger_than_size(%rip)
	movabsq	$0, %rax
	je	.LBB21_353
# BB#352:                               # %read_integral_parameter.exit.i321.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$1, %rax
.LBB21_353:                             # %read_integral_parameter.exit.i321.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	%eax, warn_larger_than(%rip)
	jmp	.LBB21_386
.LBB21_359:                             #   in Loop: Header=BB21_32 Depth=1
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_389
.LBB21_251:                             # %if.end11.i.i257.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rdi, %r13
	movq	%rbx, %rdi
	callq	atoi
	movq	%r13, %rdi
.LBB21_254:                             # %if.end20.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$32, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movl	%eax, decode_g_option.level(%rip)
	jl	.LBB21_261
# BB#255:                               # %land.lhs.true23.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB21_257
# BB#256:                               #   in Loop: Header=BB21_32 Depth=1
	movq	%r13, %rdi
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB21_261
.LBB21_257:                             # %land.lhs.true26.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	88(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str581, %rsi
	movq	%r12, %rdx
	callq	strncmp
	movq	%r13, %rdi
	testq	%rdi, %rax
	jne	.LBB21_258
# BB#259:                               # %if.then30.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movq	%rdi, %rbx
	movabsq	$.L.str582, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	callq	error
	movq	%rbx, %rdi
	movl	decode_g_option.level(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB21_261
# BB#260:                               # %if.then33.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movq	%rdi, %rbx
	movabsq	$.L.str583, %rdi
	callq	error
	movq	%rbx, %rdi
	movl	decode_g_option.level(%rip), %eax
	jmp	.LBB21_261
.LBB21_135:                             # %if.else38.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$13, %rdx
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str595, %rsi
	callq	strncmp
	movq	%rbx, %rsi
	movq	88(%rsp), %r12          # 8-byte Reload
	leaq	14(%r12), %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	testq	%rsi, %rax
	je	.LBB21_137
# BB#136:                               # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$13, %rdx
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movabsq	$.L.str596, %rsi
	callq	strncmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB21_144
.LBB21_137:                             # %if.then51.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	14(%r12), %eax
	testq	%rax, %rax
	je	.LBB21_142
# BB#138:                               # %while.body.i312.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	compiler_params(%rip), %rcx
	movl	8(%rcx), %ebx
	leaq	-1(%r12), %rcx
	addq	$15, %r12
.LBB21_140:                             # %while.body.i312.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_141
# BB#139:                               # %while.cond.i304.i.i.i
                                        #   in Loop: Header=BB21_140 Depth=2
	movzbl	(%r12), %eax
	incq	%r12
	testq	%rax, %rax
	jne	.LBB21_140
.LBB21_142:                             # %if.end11.i315.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	atoi
	movq	%rax, %rbx
	jmp	.LBB21_143
.LBB21_129:                             # %if.then23.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	(%r13), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	jne	.LBB21_131
# BB#130:                               # %if.then23.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$1, %rax
	jmp	.LBB21_131
.LBB21_258:                             #   in Loop: Header=BB21_32 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
.LBB21_261:                             # %if.end35.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rdi, %rax
	cmpq	$4, %rax
	jb	.LBB21_263
# BB#262:                               # %if.then38.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	leaq	-1(%r15), %rsi
	xorq	%rax, %rax
	movq	%rdi, %rbx
	movabsq	$.L.str584, %rdi
	callq	warning
	movq	%rbx, %rdi
	movl	debug_info_level(%rip), %eax
	movl	%eax, decode_g_option.level(%rip)
.LBB21_263:                             # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movabsq	$0, %rax
	jge	.LBB21_265
# BB#264:                               # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$1, %rax
.LBB21_265:                             # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rcx
	andq	%rdi, %rcx
	movq	$4, %rbx
	jne	.LBB21_267
# BB#266:                               # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$4, %r14
.LBB21_267:                             # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rcx, %rcx
	jne	.LBB21_269
# BB#268:                               # %if.end40.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	je	.LBB21_271
.LBB21_269:                             # %if.end53.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rdi, %r14
	movq	%r14, %rbx
	jne	.LBB21_271
# BB#270:                               # %if.then56.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	leaq	-1(%r15), %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rdi, %r14
	movabsq	$.L.str359, %rdi
	callq	warning
	movq	%r14, %rdi
.LBB21_271:                             # %if.end58.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	decode_g_option.type_explicitly_set_p(%rip), %eax
	movq	da(%rip), %r13
	testq	%rax, %rax
	movq	112(%rsp), %r14         # 8-byte Reload
	je	.LBB21_276
# BB#272:                               # %land.lhs.true60.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	8(%r13), %ecx
	movslq	decode_g_option.selected_debug_type(%rip), %rax
	testq	%rcx, %rcx
	jne	.LBB21_274
# BB#273:                               #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rbx
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_279
.LBB21_274:                             # %land.lhs.true64.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rcx
	andq	%rdi, %rcx
	movq	%rbx, %rdx
	andq	%rdi, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB21_275
.LBB21_276:                             # %if.else78.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	%ebx, decode_g_option.selected_debug_type(%rip)
	movl	8(%r13), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB21_278
# BB#277:                               # %if.else78.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$1, %rax
.LBB21_278:                             # %if.else78.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	%eax, decode_g_option.type_explicitly_set_p(%rip)
.LBB21_279:                             # %if.end82.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	decode_g_option.level(%rip), %eax
	testq	%rax, %rax
	movq	%rax, %rcx
	je	.LBB21_281
# BB#280:                               # %if.end82.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, %rcx
.LBB21_281:                             # %if.end82.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	%ecx, write_symbols(%rip)
	movl	12(%r13), %ecx
	movl	%ecx, use_gnu_debug_info_extensions(%rip)
	movl	%eax, debug_info_level(%rip)
.LBB21_285:                             # %decode_g_option.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	cmpq	$0, (%r13)
	movabsq	$0, %r15
	movq	%r15, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB21_287
# BB#286:                               # %decode_g_option.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$1, %r15
	movq	%r15, %rbx
.LBB21_287:                             # %decode_g_option.exit.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %r15
	jmp	.LBB21_389
.LBB21_275:                             # %if.then67.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	decode_g_option.debug_type_names(,%rax,8), %rdx
	movq	%r15, %rsi
	decq	%rsi
	xorq	%rax, %rax
	movabsq	$.L.str586, %rdi
	callq	warning
	movq	da(%rip), %r13
	jmp	.LBB21_284
.LBB21_144:                             # %if.else55.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %r13
	movq	$14, %rdx
	movq	%rsi, %rbx
	movq	%r14, %rdi
	movq	%r14, %r12
	movabsq	$.L.str598, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_145
# BB#146:                               # %if.else55.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_147
.LBB21_314:                             # %sw.bb5.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rax
	movl	%eax, graph_dump_format(%rip)
	jmp	.LBB21_317
.LBB21_315:                             # %sw.bb6.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rax
	movl	%eax, rtl_dump_and_exit(%rip)
	jmp	.LBB21_317
.LBB21_316:                             # %sw.bb7.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rdi
	movq	lang_hooks+144(%rip), %rax
	callq	*%rax
	jmp	.LBB21_317
.LBB21_293:                             # %while.body.i268.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB21_310 Depth 3
                                        #       Child Loop BB21_300 Depth 3
	movq	%r15, %r12
	movq	%rsi, %r15
	incq	%r14
	movq	$56, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	movq	$-512, %rcx             # imm = 0xFFFFFFFFFFFFFE00
	cmpq	$67, %rsi
	jg	.LBB21_296
# BB#294:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	%rsi, %rdi
	andq	%r15, %rdi
	cmpq	$65, %rdi
	jne	.LBB21_300
# BB#295:                               # %sw.bb2.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rax
	movl	%eax, flag_debug_asm(%rip)
	jmp	.LBB21_317
.LBB21_296:                             # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	cmpq	$72, %rsi
	jg	.LBB21_298
# BB#297:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	%rsi, %rdi
	andq	%r15, %rdi
	cmpq	$68, %rdi
	jne	.LBB21_300
	jmp	.LBB21_317
.LBB21_298:                             # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	cmpq	$79, %rsi
	jg	.LBB21_305
# BB#299:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	%rsi, %rdi
	andq	%r15, %rdi
	cmpq	$73, %rdi
	je	.LBB21_317
	jmp	.LBB21_300
.LBB21_305:                             # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	cmpq	$96, %rsi
	jg	.LBB21_308
# BB#306:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	%rsi, %rdi
	andq	%r15, %rdi
	cmpq	$80, %rdi
	jne	.LBB21_300
# BB#307:                               # %sw.bb4.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rax
	movl	%eax, flag_dump_rtl_in_asm(%rip)
	movl	%eax, flag_print_asm_name(%rip)
	jmp	.LBB21_317
.LBB21_308:                             # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	cmpq	$111, %rsi
	jg	.LBB21_311
# BB#309:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	%rsi, %rdi
	andq	%r15, %rdi
	cmpq	$97, %rdi
	movabsq	$dump_file+10, %rdi
	jne	.LBB21_300
.LBB21_310:                             # %vector.body
                                        #   Parent Loop BB21_32 Depth=1
                                        #     Parent Loop BB21_293 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$4, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$1, %rcx
	movb	%cl, (%rdi)
	movb	%cl, dump_file+26(%rax)
	movb	%cl, dump_file+42(%rax)
	movb	%cl, dump_file+58(%rax)
	addq	$4, %rdx
	addq	$64, %rdi
	cmpq	$32, %rdx
	jne	.LBB21_310
	jmp	.LBB21_317
.LBB21_311:                             # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	leaq	-112(%rsi), %rdi
	andq	%r15, %rdi
	cmpq	$9, %rdi
	ja	.LBB21_300
# BB#312:                               # %while.body.i268.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	jmpq	*.LJTI21_1(,%rdi,8)
.LBB21_313:                             # %sw.bb3.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movq	$1, %rax
	movl	%eax, flag_print_asm_name(%rip)
	jmp	.LBB21_317
	.align	16, 0x90
.LBB21_300:                             # %for.body12.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        #     Parent Loop BB21_293 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	dump_file+520(%rcx), %edi
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	%rdi, %rax
	jne	.LBB21_302
# BB#301:                               # %if.then.i273.i.i
                                        #   in Loop: Header=BB21_300 Depth=3
	movq	$1, %rdx
	movb	%dl, dump_file+522(%rcx)
.LBB21_302:                             # %for.inc21.i.i.i
                                        #   in Loop: Header=BB21_300 Depth=3
	addq	$16, %rcx
	jne	.LBB21_300
# BB#303:                               # %for.end23.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	testq	%r15, %rdx
	jne	.LBB21_317
# BB#304:                               # %if.then25.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str573, %rdi
	callq	warning
.LBB21_317:                             # %while.cond.backedge.i.i.i
                                        #   in Loop: Header=BB21_293 Depth=2
	movzbl	(%r14), %ebx
	testq	%rbx, %rbx
	movq	%r15, %rsi
	movq	%r12, %r15
	jne	.LBB21_293
.LBB21_386:                             # %sw.epilog.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rbx
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_389
.LBB21_141:                             # %if.then6.i313.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r14
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
.LBB21_143:                             # %read_integral_parameter.exit317.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.L.str597, %rdi
	movq	%rbx, %rsi
	callq	set_param_value
	movq	%r14, %rsi
	jmp	.LBB21_242
.LBB21_145:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	15(%r13), %rax
.LBB21_147:                             # %if.else55.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_148
# BB#149:                               # %if.else65.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$6, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str600, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_150
# BB#151:                               # %if.else65.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_152
.LBB21_148:                             # %if.then64.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movabsq	$.L.str599, %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	fix_sched_param
	jmp	.LBB21_241
.LBB21_150:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	7(%r13), %rax
.LBB21_152:                             # %if.else65.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_153
# BB#156:                               # %if.else75.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$10, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str601, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_157
# BB#158:                               # %if.else75.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_159
.LBB21_153:                             # %if.then74.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rsi
	jmp	.LBB21_154
.LBB21_157:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	11(%r13), %rax
.LBB21_159:                             # %if.else75.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_160
# BB#161:                               # %if.else85.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$11, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str602, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_162
# BB#163:                               # %if.else85.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_164
.LBB21_160:                             # %if.then84.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rsi, %rsi
.LBB21_154:                             # %if.end238.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rdx
	jmp	.LBB21_155
.LBB21_162:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	12(%r13), %rax
.LBB21_164:                             # %if.else85.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_165
# BB#166:                               # %if.else95.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$12, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str45, %rax
	movq	%rax, %rsi
	callq	strncmp
	leaq	13(%r13), %r15
	testq	%rbx, %rax
	movq	%r15, %rax
	je	.LBB21_168
# BB#167:                               # %if.else95.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
.LBB21_168:                             # %if.else95.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_169
# BB#174:                               # %if.else107.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$16, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str49, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_175
# BB#176:                               # %if.else107.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_177
.LBB21_165:                             # %if.then94.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
.LBB21_155:                             # %if.end238.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdi
	callq	fix_register
	jmp	.LBB21_241
.LBB21_169:                             # %if.then104.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB21_240
# BB#170:                               # %while.body.i328.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	align_loops(%rip), %r14d
	movq	%r13, %rsi
	leaq	-1(%rsi), %rcx
	addq	$14, %rsi
.LBB21_172:                             # %while.body.i328.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rdx
	movzbl	_sch_istable(%rdx,%rdx), %edx
	testq	$4, %rdx
	je	.LBB21_173
# BB#171:                               # %while.cond.i320.i.i.i
                                        #   in Loop: Header=BB21_172 Depth=2
	movzbl	(%rsi), %edx
	incq	%rsi
	testq	%rdx, %rdx
	jne	.LBB21_172
.LBB21_240:                             # %if.end11.i331.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdi
	callq	atoi
	movl	%eax, align_loops(%rip)
	jmp	.LBB21_241
.LBB21_175:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	17(%r13), %rax
.LBB21_177:                             # %if.else107.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_178
# BB#184:                               # %if.else119.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$12, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str47, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_186
# BB#185:                               # %if.else119.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%r15, %r15
.LBB21_186:                             # %if.else119.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%r15, %r15
	jne	.LBB21_187
# BB#193:                               # %if.else131.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$13, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str603, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_195
# BB#194:                               # %if.else131.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
.LBB21_195:                             # %if.else131.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	jne	.LBB21_196
# BB#202:                               # %if.else143.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$21, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str604, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_203
# BB#204:                               # %if.else143.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%r14, %r14
	jmp	.LBB21_205
.LBB21_173:                             # %if.then6.i329.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
	movl	%r14d, align_loops(%rip)
	jmp	.LBB21_241
.LBB21_178:                             # %if.then116.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB21_183
# BB#179:                               # %while.body.i344.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	align_functions(%rip), %r14d
	movq	%r13, %rsi
	leaq	-1(%rsi), %rcx
	addq	$18, %rsi
.LBB21_181:                             # %while.body.i344.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rdx
	movzbl	_sch_istable(%rdx,%rdx), %edx
	testq	$4, %rdx
	je	.LBB21_182
# BB#180:                               # %while.cond.i336.i.i.i
                                        #   in Loop: Header=BB21_181 Depth=2
	movzbl	(%rsi), %edx
	incq	%rsi
	testq	%rdx, %rdx
	jne	.LBB21_181
.LBB21_183:                             # %if.end11.i347.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdi
	callq	atoi
	movl	%eax, align_functions(%rip)
	jmp	.LBB21_241
.LBB21_187:                             # %if.then128.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB21_192
# BB#188:                               # %while.body.i360.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	align_jumps(%rip), %r14d
	movq	%r13, %rdx
	leaq	-1(%rdx), %rcx
	addq	$14, %rdx
.LBB21_190:                             # %while.body.i360.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_191
# BB#189:                               # %while.cond.i352.i.i.i
                                        #   in Loop: Header=BB21_190 Depth=2
	movzbl	(%rdx), %eax
	incq	%rdx
	testq	%rax, %rax
	jne	.LBB21_190
.LBB21_192:                             # %if.end11.i363.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, %rdi
	callq	atoi
	movl	%eax, align_jumps(%rip)
	jmp	.LBB21_241
.LBB21_196:                             # %if.then140.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB21_201
# BB#197:                               # %while.body.i376.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	align_labels(%rip), %r14d
	movq	%r13, %rdx
	leaq	-1(%rdx), %rcx
	addq	$15, %rdx
.LBB21_199:                             # %while.body.i376.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB21_200
# BB#198:                               # %while.cond.i368.i.i.i
                                        #   in Loop: Header=BB21_199 Depth=2
	movzbl	(%rdx), %eax
	incq	%rdx
	testq	%rax, %rax
	jne	.LBB21_199
.LBB21_201:                             # %if.end11.i379.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	atoi
	movl	%eax, align_labels(%rip)
	jmp	.LBB21_241
.LBB21_203:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	22(%r13), %r14
.LBB21_205:                             # %if.else143.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%r14, %r14
	jne	.LBB21_206
# BB#210:                               # %if.else162.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$19, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str606, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_211
# BB#212:                               # %if.else162.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_213
.LBB21_182:                             # %if.then6.i345.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
	movl	%r14d, align_functions(%rip)
	jmp	.LBB21_241
.LBB21_191:                             # %if.then6.i361.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
	movl	%r14d, align_jumps(%rip)
	jmp	.LBB21_241
.LBB21_200:                             # %if.then6.i377.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
	movl	%r14d, align_labels(%rip)
	jmp	.LBB21_241
.LBB21_206:                             # %if.then152.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdi
	callq	decode_reg_name
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB21_209
# BB#207:                               # %if.then156.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str605, %rcx
	movq	%rcx, %rdi
	movq	%r14, %rsi
	callq	error
	jmp	.LBB21_241
.LBB21_211:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	20(%r13), %rax
.LBB21_213:                             # %if.else162.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_214
# BB#215:                               # %if.else177.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$15, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str607, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_216
# BB#217:                               # %if.else177.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	jmp	.LBB21_218
.LBB21_209:                             # %if.else157.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, stack_limit_rtx(%rip)
	jmp	.LBB21_241
.LBB21_214:                             # %if.then171.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	ggc_alloc_string
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	$68, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	%rbx, %rsi
	movq	%rax, stack_limit_rtx(%rip)
	jmp	.LBB21_242
.LBB21_216:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	16(%r13), %rax
.LBB21_218:                             # %if.else177.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%rax, %rax
	jne	.LBB21_219
# BB#226:                               # %if.else189.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$26, %rdx
	movq	%r12, %rdi
	movabsq	$.L.str608, %rax
	movq	%rax, %rsi
	callq	strncmp
	testq	%rbx, %rax
	je	.LBB21_227
# BB#228:                               # %if.else189.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%r14, %r14
	jmp	.LBB21_229
.LBB21_219:                             # %if.then186.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	(%rax), %edx
	movq	global_dc(%rip), %r15
	testq	%rdx, %rdx
	je	.LBB21_224
# BB#220:                               # %while.body.i.i.i.i.preheader
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	12(%r15), %r14d
	movq	%r13, %rsi
	leaq	-1(%rsi), %rcx
	addq	$17, %rsi
.LBB21_222:                             # %while.body.i.i.i.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rdx
	movzbl	_sch_istable(%rdx,%rdx), %edx
	testq	$4, %rdx
	je	.LBB21_223
# BB#221:                               # %while.cond.i.i.i.i
                                        #   in Loop: Header=BB21_222 Depth=2
	movzbl	(%rsi), %edx
	incq	%rsi
	testq	%rdx, %rdx
	jne	.LBB21_222
.LBB21_224:                             # %if.end11.i.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rax, %rdi
	callq	atoi
	movq	%rax, %r14
	jmp	.LBB21_225
.LBB21_227:                             #   in Loop: Header=BB21_32 Depth=1
	leaq	27(%r13), %r14
.LBB21_229:                             # %if.else189.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	testq	%r14, %r14
	jne	.LBB21_230
# BB#236:                               # %if.else213.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %rdi
	movabsq	$.L.str611, %rax
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	jne	.LBB21_238
# BB#237:                               # %if.then216.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$0, stack_limit_rtx(%rip)
	jmp	.LBB21_241
.LBB21_223:                             # %if.then6.i.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
.LBB21_225:                             # %read_integral_parameter.exit.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	output_set_maximum_length
	jmp	.LBB21_241
.LBB21_230:                             # %if.then198.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdi
	movabsq	$.L.str609, %rax
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	jne	.LBB21_233
# BB#231:                               # %if.then201.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	global_dc(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 24(%rax)
	jmp	.LBB21_241
.LBB21_238:                             # %if.else217.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r12, %rdi
	movabsq	$.L.str612, %rax
	movq	%rax, %rsi
	callq	strcmp
	movq	%rbx, %rsi
	testq	%rsi, %rax
	je	.LBB21_242
# BB#239:                               #   in Loop: Header=BB21_32 Depth=1
	movq	112(%rsp), %r15         # 8-byte Reload
.LBB21_362:                             #   in Loop: Header=BB21_32 Depth=1
	movq	104(%rsp), %rdx         # 8-byte Reload
	xorq	%rbx, %rbx
	jmp	.LBB21_389
.LBB21_233:                             # %if.else203.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rdi
	movabsq	$.L.str610, %rax
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	jne	.LBB21_235
# BB#234:                               # %if.then206.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	global_dc(%rip), %rax
	movq	$2, %rcx
	movl	%ecx, 24(%rax)
	jmp	.LBB21_241
.LBB21_235:                             # %if.else209.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r13, %rdx
	decq	%rdx
	xorq	%rax, %rax
	movabsq	$.L.str360, %rcx
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	error
.LBB21_241:                             # %if.end238.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, %rsi
.LBB21_242:                             # %if.end238.i.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$1, %rbx
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB21_389
	.align	16, 0x90
.LBB21_32:                              # %for.body57.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_39 Depth 2
                                        #     Child Loop BB21_73 Depth 2
                                        #     Child Loop BB21_77 Depth 2
                                        #     Child Loop BB21_80 Depth 2
                                        #     Child Loop BB21_244 Depth 2
                                        #     Child Loop BB21_249 Depth 2
                                        #     Child Loop BB21_120 Depth 2
                                        #     Child Loop BB21_172 Depth 2
                                        #     Child Loop BB21_181 Depth 2
                                        #     Child Loop BB21_190 Depth 2
                                        #     Child Loop BB21_199 Depth 2
                                        #     Child Loop BB21_222 Depth 2
                                        #     Child Loop BB21_140 Depth 2
                                        #     Child Loop BB21_293 Depth 2
                                        #       Child Loop BB21_310 Depth 3
                                        #       Child Loop BB21_300 Depth 3
                                        #     Child Loop BB21_331 Depth 2
                                        #     Child Loop BB21_348 Depth 2
                                        #     Child Loop BB21_379 Depth 2
                                        #     Child Loop BB21_375 Depth 2
                                        #     Child Loop BB21_99 Depth 2
                                        #     Child Loop BB21_395 Depth 2
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	movq	%r15, %r12
	subq	%rsi, %r12
	movq	$32, %rcx
	movq	%rsi, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	128(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13,8), %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	lang_hooks+24(%rip), %rax
	callq	*%rax
	movq	%rax, %rdx
	movq	$32, %rcx
                                        # kill: RAX<def> RDX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB21_387
# BB#33:                                # %if.then62.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	(%rbx), %rcx
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movzbl	(%rcx), %eax
	xorq	%rbx, %rbx
	cmpq	$43, %rax
	je	.LBB21_388
# BB#34:                                # %if.then62.i
                                        #   in Loop: Header=BB21_32 Depth=1
	cmpq	$45, %rax
	jne	.LBB21_36
# BB#35:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB21_37
.LBB21_36:                              # %if.end.i.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rcx, filename(%rip)
	movq	$1, %rbx
	jmp	.LBB21_388
	.align	16, 0x90
.LBB21_387:                             # %if.else67.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	negq	%rdx
	xorq	%rbx, %rbx
.LBB21_388:                             # %if.end69.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r14, %rsi
.LBB21_389:                             # %if.end69.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, %rax
	orq	%rdx, %rax
	testq	%rsi, %rax
	jne	.LBB21_391
# BB#390:                               # %for.cond77.preheader.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	movabsq	$documented_lang_options, %r13
	xorq	%r15, %r15
	.align	16, 0x90
.LBB21_395:                             # %for.body81.i
                                        #   Parent Loop BB21_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13), %r12
	movq	%rsi, %r14
	movq	%r12, %rdi
	callq	strlen
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	strncmp
	movq	%r14, %rsi
	testq	%rsi, %rax
	movq	%rbx, %rax
	je	.LBB21_396
# BB#394:                               # %for.cond77.i
                                        #   in Loop: Header=BB21_395 Depth=2
	incq	%rbx
	incq	%r15
	movq	%rbx, %rax
	andq	%rsi, %rax
	addq	$16, %r13
	cmpq	$107, %rax
	movq	%r15, %rax
	jbe	.LBB21_395
.LBB21_396:                             # %for.end101.i
                                        #   in Loop: Header=BB21_32 Depth=1
	andq	%rsi, %rax
	movq	%rsi, %r14
	cmpq	$108, %rax
	jne	.LBB21_397
# BB#399:                               # %if.else114.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	136(%rsp), %rsi         # 8-byte Reload
	movzbl	(%rsi), %eax
	cmpq	$45, %rax
	jne	.LBB21_402
# BB#400:                               # %land.lhs.true121.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movzbl	1(%rsi), %eax
	cmpq	$103, %rax
	jne	.LBB21_402
# BB#401:                               # %if.then128.i
                                        #   in Loop: Header=BB21_32 Depth=1
	addq	$2, %rsi
	xorq	%rax, %rax
	movabsq	$.L.str359, %rdi
	callq	warning
	jmp	.LBB21_403
	.align	16, 0x90
.LBB21_391:                             # %if.then73.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rsi, %r14
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jge	.LBB21_393
# BB#392:                               # %if.then73.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	%rbx, %rdx
.LBB21_393:                             # %if.then73.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	120(%rsp), %rsi         # 8-byte Reload
	addq	%rdx, %rsi
	jmp	.LBB21_404
	.align	16, 0x90
.LBB21_397:                             # %if.then105.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_403
# BB#398:                               # %if.then107.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str357, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	warning
	jmp	.LBB21_403
.LBB21_402:                             # %if.else132.i
                                        #   in Loop: Header=BB21_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str360, %rdi
	callq	error
.LBB21_403:                             # %if.end136.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	120(%rsp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	112(%rsp), %r15         # 8-byte Reload
.LBB21_404:                             # %for.cond54.backedge.i
                                        #   in Loop: Header=BB21_32 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB21_32
.LBB21_405:                             # %for.end139.i
	movl	flag_no_inline(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB21_407
# BB#406:                               # %if.then142.i
	xorq	%rax, %rax
	movl	%eax, flag_no_inline(%rip)
	jmp	.LBB21_408
.LBB21_407:                             # %if.else143.i
	movl	%eax, flag_really_no_inline(%rip)
.LBB21_408:                             # %if.end144.i
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_411
# BB#409:                               # %if.then147.i
	movq	$1, %rax
	movl	warn_uninitialized(%rip), %ecx
	movl	%eax, flag_no_inline(%rip)
	xorq	%rax, %rax
	movl	%eax, warn_inline(%rip)
	cmpq	$1, %rcx
	jne	.LBB21_411
# BB#410:                               # %if.then150.i
	movabsq	$.L.str361, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB21_411:                             # %if.end152.i
	movl	flag_really_no_inline(%rip), %eax
	cmpq	$2, %rax
	jne	.LBB21_413
# BB#412:                               # %if.then155.i
	movl	flag_no_inline(%rip), %eax
	movl	%eax, flag_really_no_inline(%rip)
.LBB21_413:                             # %parse_options_and_default_flags.exit
	callq	*lang_hooks+32(%rip)
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB21_559
# BB#414:                               # %parse_options_and_default_flags.exit
	movzbl	exit_after_options(%rip), %edx
	testq	$1, %rdx
	jne	.LBB21_559
# BB#415:                               # %if.then
	callq	override_options
	movslq	align_loops(%rip), %rax
	testq	%rax, %rax
	jg	.LBB21_417
# BB#416:                               # %if.then.i.i
	movq	$1, %rax
	movl	%eax, align_loops(%rip)
.LBB21_417:                             # %if.end.i.i18
	movslq	align_loops_max_skip(%rip), %rcx
	cmpq	%rax, %rcx
	jg	.LBB21_419
# BB#418:                               # %if.end.i.i18
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_420
.LBB21_419:                             # %if.then2.i.i
	leaq	-1(%rax), %rcx
	movl	%ecx, align_loops_max_skip(%rip)
.LBB21_420:                             # %if.end3.i.i
	addq	%rax, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$-4294967296, %rdi      # imm = 0xFFFFFFFF00000000
	addq	%rdi, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$1, %rcx
	.align	16, 0x90
.LBB21_421:                             # %while.body.i.i.i21
                                        # =>This Inner Loop Header: Depth=1
	incq	%rdx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB21_421
# BB#422:                               # %floor_log2_wide.exit.i.i
	movslq	align_jumps(%rip), %rax
	movl	%edx, align_loops_log(%rip)
	testq	%rax, %rax
	jg	.LBB21_424
# BB#423:                               # %if.then7.i.i
	movq	$1, %rax
	movl	%eax, align_jumps(%rip)
.LBB21_424:                             # %if.end8.i.i
	movslq	align_jumps_max_skip(%rip), %rcx
	cmpq	%rax, %rcx
	jg	.LBB21_426
# BB#425:                               # %if.end8.i.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_427
.LBB21_426:                             # %if.then13.i.i23
	leaq	-1(%rax), %rcx
	movl	%ecx, align_jumps_max_skip(%rip)
.LBB21_427:                             # %if.end15.i.i
	addq	%rax, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	addq	%rdi, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$1, %rcx
	.align	16, 0x90
.LBB21_428:                             # %while.body.i150.i.i
                                        # =>This Inner Loop Header: Depth=1
	incq	%rdx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB21_428
# BB#429:                               # %floor_log2_wide.exit151.i.i
	movslq	align_labels(%rip), %rax
	movl	%edx, align_jumps_log(%rip)
	testq	%rax, %rax
	jg	.LBB21_431
# BB#430:                               # %if.then22.i.i
	movq	$1, %rax
	movl	%eax, align_labels(%rip)
.LBB21_431:                             # %if.end23.i.i
	leaq	(%rax,%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	addq	%rdi, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	$1, %rcx
	.align	16, 0x90
.LBB21_432:                             # %while.body.i157.i.i
                                        # =>This Inner Loop Header: Depth=1
	incq	%rsi
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB21_432
# BB#433:                               # %floor_log2_wide.exit158.i.i
	movslq	align_labels_max_skip(%rip), %rcx
	movl	%esi, align_labels_log(%rip)
	cmpq	%rax, %rcx
	jg	.LBB21_435
# BB#434:                               # %floor_log2_wide.exit158.i.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_436
.LBB21_435:                             # %if.then32.i.i
	decq	%rax
	movl	%eax, align_labels_max_skip(%rip)
.LBB21_436:                             # %if.end34.i.i
	movslq	align_functions(%rip), %rax
	testq	%rax, %rax
	jg	.LBB21_438
# BB#437:                               # %if.then37.i.i
	movq	$1, %rax
	movl	%eax, align_functions(%rip)
.LBB21_438:                             # %if.end38.i.i
	addq	%rax, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	addq	%rdi, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$1, %rcx
	.align	16, 0x90
.LBB21_439:                             # %while.body.i164.i.i
                                        # =>This Inner Loop Header: Depth=1
	incq	%rdx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB21_439
# BB#440:                               # %floor_log2_wide.exit165.i.i
	movl	flag_unroll_all_loops(%rip), %eax
	movl	%edx, align_functions_log(%rip)
	testq	%rax, %rax
	je	.LBB21_442
# BB#441:                               # %if.end45.thread.i.i
	movq	$1, %rax
	movl	%eax, flag_unroll_loops(%rip)
	jmp	.LBB21_443
.LBB21_442:                             # %if.end45.i.i
	movl	flag_unroll_loops(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_444
.LBB21_443:                             # %if.then47.i.i
	movq	$1, %rax
	movl	%eax, flag_strength_reduce(%rip)
	movl	%eax, flag_rerun_cse_after_loop(%rip)
.LBB21_444:                             # %if.end48.i.i
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_446
# BB#445:                               # %if.end51.thread.i.i
	movq	$1, %rax
	movl	%eax, flag_asynchronous_unwind_tables(%rip)
	jmp	.LBB21_447
.LBB21_446:                             # %if.end51.i.i
	movl	flag_asynchronous_unwind_tables(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_448
.LBB21_447:                             # %if.then53.i.i
	movq	$1, %rax
	movl	%eax, flag_unwind_tables(%rip)
.LBB21_448:                             # %if.end54.i.i
	movl	flag_delayed_branch(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_450
# BB#449:                               # %if.then56.i.i
	movabsq	$.L.str345, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB21_450:                             # %if.end57.i.i
	movl	profile_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_453
# BB#451:                               # %if.end57.i.i
	movl	flag_omit_frame_pointer(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB21_453
# BB#452:                               # %if.then60.i.i
	movabsq	$.L.str346, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
	movl	%ebx, flag_omit_frame_pointer(%rip)
.LBB21_453:                             # %if.end61.i.i
	movl	flag_leading_underscore(%rip), %eax
	movq	$.L.str28, user_label_prefix(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	cmpq	%r12, %rax
	je	.LBB21_458
# BB#454:                               # %if.then78.i.i
	testq	%rax, %rax
	jne	.LBB21_455
# BB#456:                               # %if.then78.i.i
	movabsq	$.L.str28, %rax
	jmp	.LBB21_457
.LBB21_455:
	movabsq	$.L.str347, %rax
.LBB21_457:                             # %if.then78.i.i
	movq	%rax, user_label_prefix(%rip)
.LBB21_458:                             # %if.end83.i.i
	movzbl	version_flag(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB21_461
# BB#459:                               # %if.then85.i.i
	movq	stderr(%rip), %rbx
	movq	lang_hooks(%rip), %r8
	movq	version_string(%rip), %r9
	movq	$.L.str344, 16(%rsp)
	movq	$.L.str28, 8(%rsp)
	movabsq	$.L.str342, %rsi
	movabsq	$.L.str28, %rdx
	xorq	%rax, %rax
	movq	$.L.str343, (%rsp)
	movq	%rbx, %rdi
	movq	%rdx, %rcx
	callq	fnotice
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_463
# BB#460:                               # %if.then87.i.i
	movabsq	$.L.str28, %rsi
	movq	%rbx, %rdi
	callq	print_switch_values
.LBB21_461:                             # %if.end89.i.i
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_463
# BB#462:                               # %if.then91.i.i27
	movq	$1, %rax
	movl	%eax, time_report(%rip)
.LBB21_463:                             # %if.end92.i.i
	movl	flag_syntax_only(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_465
# BB#464:                               # %if.end95.thread.i.i
	xorq	%rax, %rax
	movl	%eax, write_symbols(%rip)
	movl	%eax, profile_flag(%rip)
	jmp	.LBB21_469
.LBB21_465:                             # %if.end95.i.i
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	jne	.LBB21_466
# BB#468:                               # %if.then102.i.i
	movq	$dwarf2_debug_hooks, debug_hooks(%rip)
	jmp	.LBB21_469
.LBB21_466:                             # %if.end95.i.i
	cmpq	$1, %rax
	jne	.LBB21_469
# BB#467:                               # %if.then98.i.i
	movq	$dbx_debug_hooks, debug_hooks(%rip)
.LBB21_469:                             # %if.end103.i.i
	movl	flag_gen_aux_info(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_473
# BB#470:                               # %if.then105.i.i28
	movq	aux_info_file_name(%rip), %rdi
	movabsq	$.L.str22, %rsi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, aux_info_file(%rip)
	je	.LBB21_471
.LBB21_473:                             # %if.end111.i.i
	movzbl	targetm+328(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_478
# BB#474:                               # %if.then113.i.i29
	movl	flag_function_sections(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_476
# BB#475:                               # %if.then115.i.i
	movabsq	$.L.str351, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	movl	%ebx, flag_function_sections(%rip)
.LBB21_476:                             # %if.end116.i.i
	movl	flag_data_sections(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_478
# BB#477:                               # %if.then118.i.i
	movabsq	$.L.str352, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	movl	%ebx, flag_data_sections(%rip)
.LBB21_478:                             # %if.end120.i.i
	movl	flag_function_sections(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_481
# BB#479:                               # %if.end120.i.i
	movl	profile_flag(%rip), %eax
	testq	%r12, %rax
	je	.LBB21_481
# BB#480:                               # %if.then124.i.i
	movabsq	$.L.str353, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	movl	%ebx, flag_function_sections(%rip)
.LBB21_481:                             # %if.end125.i.i
	movl	flag_prefetch_loop_arrays(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_485
# BB#482:                               # %if.end125.i.i
	movl	x86_prefetch_sse(%rip), %ecx
	testq	%r12, %rcx
	jne	.LBB21_485
# BB#483:                               # %lor.lhs.false129.i.i
	movzbl	target_flags+2(%rip), %ecx
	testq	$16, %rcx
	je	.LBB21_484
.LBB21_485:                             # %if.end133.i.i
	testq	%rax, %rax
	je	.LBB21_489
# BB#486:                               # %if.end133.i.i
	movl	optimize_size(%rip), %eax
	testq	%r12, %rax
	je	.LBB21_489
# BB#487:                               # %if.then137.i.i
	movabsq	$.L.str355, %rdi
	jmp	.LBB21_488
.LBB21_484:                             # %if.end133.thread.i.i
	movabsq	$.L.str354, %rdi
.LBB21_488:                             # %process_options.exit.i
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	movl	%ebx, flag_prefetch_loop_arrays(%rip)
.LBB21_489:                             # %process_options.exit.i
	callq	init_timevar
	xorq	%rdi, %rdi
	callq	timevar_start
	movq	$decl_name, decl_printable_name(%rip)
	movq	lang_hooks+8(%rip), %rax
	movq	$do_abort, lang_expand_expr(%rip)
	movabsq	$34359738351, %rdx      # imm = 0x7FFFFFFEF
	addq	%rax, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movl	%edx, tree_code_length+4(%rip)
	callq	init_ggc
	movabsq	$stack_limit_rtx, %rdi
	movq	$1, %rsi
	movq	$1, %rbx
	callq	ggc_add_rtx_root
	movabsq	$current_function_decl, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	movabsq	$current_function_func_begin_label, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
	callq	init_stringpool
	callq	init_obstacks
	movl	debug_info_level(%rip), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$2, %rcx
	je	.LBB21_494
# BB#490:                               # %process_options.exit.i
	movl	flag_test_coverage(%rip), %eax
	testq	%r12, %rax
	jne	.LBB21_494
# BB#491:                               # %lor.rhs.i.i
	movl	warn_notreached(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_492
# BB#493:                               # %lor.rhs.i.i
	movabsq	$1, %rbx
	jmp	.LBB21_494
.LBB21_492:
	movabsq	$0, %rbx
.LBB21_494:                             # %lor.end.i.i
	movq	%rbx, %rdi
	callq	init_emit_once
	callq	init_regs
	callq	init_alias_once
	callq	init_stmt
	callq	init_loop
	callq	init_reload
	callq	init_function_once
	callq	init_stor_layout_once
	callq	init_varasm_once
	callq	init_EXPR_INSN_LIST_cache
	callq	init_dummy_function_start
	callq	init_expmed
	callq	init_expr_once
	movl	flag_caller_saves(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_496
# BB#495:                               # %if.then.i3.i
	callq	init_caller_save
.LBB21_496:                             # %lang_independent_init.exit.i
	callq	expand_dummy_function_end
	movq	filename(%rip), %rdi
	cmpq	$0, dump_base_name(%rip)
	jne	.LBB21_500
# BB#497:                               # %if.then.i8.i
	testq	%rdi, %rdi
	movq	%rdi, %rax
	jne	.LBB21_499
# BB#498:                               # %if.then.i8.i
	movabsq	$.L.str19, %rax
.LBB21_499:                             # %if.then.i8.i
	movq	%rax, dump_base_name(%rip)
.LBB21_500:                             # %if.end.i10.i
	callq	*lang_hooks+40(%rip)
	testq	%rax, %rax
	je	.LBB21_529
# BB#501:                               # %if.end3.i12.i
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	ggc_alloc_string
	movq	asm_file_name(%rip), %rbx
	movq	%rax, %r14
	movq	%r14, input_filename(%rip)
	movq	%r14, main_input_filename(%rip)
                                        # kill: RAX<def> R14<kill>
	orq	%rbx, %rax
	jne	.LBB21_535
# BB#502:                               # %if.then.i.i.i31
	movq	stdout(%rip), %rdi
	movq	%rdi, asm_out_file(%rip)
	jmp	.LBB21_503
.LBB21_535:                             # %if.else.i.i.i32
	testq	%rbx, %rbx
	jne	.LBB21_543
# BB#536:                               # %if.then3.i.i.i
	movq	dump_base_name(%rip), %rdi
	callq	strlen
	movq	%rax, %r13
	movq	$32, %rcx
	movq	%r13, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	sarq	%cl, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movabsq	$25769803776, %rdi      # imm = 0x600000000
	addq	%r12, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	dump_base_name(%rip), %rsi
	movabsq	$4294967296, %rax       # imm = 0x100000000
	leaq	(%r12,%rax), %rdx
	movq	%rax, %r12
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rbx, %rdi
	callq	memcpy
	cmpq	$3, 136(%rsp)           # 8-byte Folded Reload
	movabsq	$-4294967296, %r8       # imm = 0xFFFFFFFF00000000
	jl	.LBB21_542
# BB#537:                               # %for.body.i.i.i.i.preheader
	imulq	%r13, %r12
	movabsq	$-8589934592, %rax      # imm = 0xFFFFFFFE00000000
	addq	%r12, %rax
	movq	$3, %rdx
	.align	16, 0x90
.LBB21_540:                             # %for.body.i.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movzbl	(%rbx,%rsi), %ecx
	cmpq	$46, %rcx
	je	.LBB21_541
# BB#538:                               # %for.cond.i.i.i.i
                                        #   in Loop: Header=BB21_540 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$5, %rsi
	jg	.LBB21_542
# BB#539:                               # %for.cond.i.i.i.i
                                        #   in Loop: Header=BB21_540 Depth=1
	addq	%r8, %rax
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	leaq	1(%rdx), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	movq	%rdi, %rdx
	jl	.LBB21_540
	jmp	.LBB21_542
.LBB21_541:                             # %if.then.i.i.i.i
	xorq	%rax, %rax
	movb	%al, (%rbx,%rsi)
.LBB21_542:                             # %strip_off_ending.exit.i.i.i
	movq	%rbx, %rdi
	callq	strlen
	xorq	%rcx, %rcx
	movb	%cl, 2(%rbx,%rax)
	movq	$29486, %rcx            # imm = 0x732E
	movw	%cx, (%rbx,%rax)
	movq	%rbx, asm_file_name(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB21_543:                             # %if.end.i.i.i38
	movabsq	$.L.str21, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB21_544
# BB#545:                               # %if.else11.i.i.i
	movabsq	$.L.str22, %rsi
	movq	%rbx, %rdi
	callq	fopen
	movq	%rax, %rdi
	jmp	.LBB21_546
.LBB21_544:                             # %if.then10.i.i.i
	movq	stdout(%rip), %rdi
.LBB21_546:                             # %if.end13.i.i.i
	movq	%rdi, asm_out_file(%rip)
	testq	%rdi, %rdi
	je	.LBB21_547
.LBB21_503:                             # %if.end18.i.i.i
	movl	flag_syntax_only(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_508
# BB#504:                               # %do.body.i.i.i
	movq	main_input_filename(%rip), %rsi
	callq	output_file_directive
	movl	ix86_asm_dialect(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB21_506
# BB#505:                               # %if.then23.i.i.i40
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str24, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB21_506:                             # %do.end.i.i.i
	movl	flag_verbose_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_508
# BB#507:                               # %if.then27.i.i.i
	movq	asm_out_file(%rip), %rdi
	movq	lang_hooks(%rip), %r8
	movq	version_string(%rip), %r9
	movq	$.L.str344, 16(%rsp)
	movq	$.L.str25, 8(%rsp)
	movabsq	$.L.str342, %rsi
	movabsq	$.L.str25, %rbx
	movabsq	$.L.str26, %rcx
	xorq	%rax, %rax
	movq	$.L.str343, (%rsp)
	movq	%rbx, %rdx
	callq	fnotice
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	print_switch_values
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB21_508:                             # %init_asm_output.exit.i.i
	callq	init_eh
	callq	init_optabs
	movq	input_file_stack(%rip), %rax
	movq	input_filename(%rip), %rbx
	testq	%rax, %rax
	je	.LBB21_510
# BB#509:                               # %if.then.i17.i.i
	movq	%rbx, (%rax)
	movl	lineno(%rip), %eax
	movq	input_file_stack(%rip), %rcx
	movl	%eax, 16(%rcx)
.LBB21_510:                             # %push_srcloc.exit.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	%rbx, input_filename(%rip)
	movq	%rbx, (%rax)
	xorq	%rcx, %rcx
	movl	%ecx, lineno(%rip)
	movq	$0, 16(%rax)
	movl	input_file_stack_tick(%rip), %ecx
	movq	input_file_stack(%rip), %rdx
	movq	%rdx, 8(%rax)
	movq	%rax, input_file_stack(%rip)
	incq	%rcx
	movl	%ecx, input_file_stack_tick(%rip)
	movq	$43, %rdi
	callq	timevar_push
	callq	dwarf2out_do_frame
	testq	%r15, %rax
	je	.LBB21_512
# BB#511:                               # %if.then7.i15.i
	callq	dwarf2out_frame_init
.LBB21_512:                             # %if.then.i41
	movq	debug_hooks(%rip), %rax
	movq	%r14, %rdi
	callq	*(%rax)
	movq	$43, %rdi
	callq	timevar_pop
	movq	main_input_filename(%rip), %rdi
	callq	init_final
	movq	dump_base_name(%rip), %rdi
	callq	init_branch_prob
	movq	$9, %rdi
	callq	timevar_push
	callq	yyparse
	callq	*lang_hooks+56(%rip)
	movq	$9, %rdi
	callq	timevar_pop
	movl	flag_syntax_only(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_529
# BB#513:                               # %if.end.i21.i
	callq	getdecls
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	list_length
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$3, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r12
	testq	%r13, %r13
	jle	.LBB21_516
# BB#514:                               # %for.body.lr.ph.i.i
	leaq	-1(%r14), %rax
	movq	$1, %rdx
	.align	16, 0x90
.LBB21_515:                             # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rdx), %rcx
	movq	%rax, %rsi
	subq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rbx, (%r12,%rsi,8)
	movq	(%rbx), %rbx
	movq	%rdx, %rcx
	andq	%r15, %rcx
	movq	%r14, %rsi
	andq	%r15, %rsi
	incq	%rdx
	cmpq	%rsi, %rcx
	jne	.LBB21_515
.LBB21_516:                             # %for.end.i.i
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	wrapup_global_declarations
	callq	output_func_start_profiler
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	check_global_declarations
	movq	%r12, %rdi
	callq	free
	callq	weak_finish
	movq	$43, %rdi
	callq	timevar_push
	callq	dwarf2out_do_frame
	testq	%r15, %rax
	je	.LBB21_518
# BB#517:                               # %if.then9.i.i46
	callq	dwarf2out_frame_finish
.LBB21_518:                             # %if.end10.i.i
	movq	debug_hooks(%rip), %rax
	movq	main_input_filename(%rip), %rdi
	callq	*8(%rax)
	movq	$43, %rdi
	callq	timevar_pop
	callq	dw2_output_indirect_constants
	movq	dump_base_name(%rip), %rdi
	callq	end_final
	movl	profile_arc_flag(%rip), %eax
	movl	flag_test_coverage(%rip), %ecx
	movl	flag_branch_probabilities(%rip), %edx
	orq	%rax, %rcx
	orq	%rcx, %rdx
	testq	%r15, %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB21_522
# BB#519:                               # %if.then15.i.i
	movq	$2, %rdi
	callq	timevar_push
	movq	$14, %rdi
	xorq	%rsi, %rsi
	callq	open_dump_file
	callq	end_branch_prob
	cmpq	$0, rtl_dump_file(%rip)
	je	.LBB21_521
# BB#520:                               # %if.end.i.i24.i
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	callq	fflush
	movq	rtl_dump_file(%rip), %rdi
	callq	fclose
	movq	$0, rtl_dump_file(%rip)
	movq	$2, %rdi
	callq	timevar_pop
.LBB21_521:                             # %close_dump_file.exit.i.i
	movq	$2, %rdi
	callq	timevar_pop
.LBB21_522:                             # %if.end17.i.i
	movq	asm_out_file(%rip), %rdi
	callq	ix86_asm_file_end
	movl	flag_no_ident(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_524
# BB#523:                               # %if.then19.i.i
	movq	asm_out_file(%rip), %rdi
	movq	version_string(%rip), %rcx
	movabsq	$.L.str17, %rsi
	movabsq	$.L.str18, %rdx
	xorq	%rax, %rax
	callq	fprintf
.LBB21_524:                             # %if.end21.i.i
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB21_529
# BB#525:                               # %land.lhs.true.i.i
	movq	$16, %rdi
	xorq	%rsi, %rsi
	callq	open_dump_file
	testq	%r15, %rax
	je	.LBB21_529
# BB#526:                               # %if.then26.i.i47
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	callq	dump_combine_total_stats
	cmpq	$0, rtl_dump_file(%rip)
	je	.LBB21_528
# BB#527:                               # %if.end.i42.i.i
	movq	$2, %rdi
	callq	timevar_push
	movq	rtl_dump_file(%rip), %rdi
	callq	fflush
	movq	rtl_dump_file(%rip), %rdi
	callq	fclose
	movq	$0, rtl_dump_file(%rip)
	movq	$2, %rdi
	callq	timevar_pop
.LBB21_528:                             # %close_dump_file.exit45.i.i
	movq	$2, %rdi
	callq	timevar_pop
.LBB21_529:                             # %if.end.i
	movl	flag_gen_aux_info(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_532
# BB#530:                               # %if.then.i28.i
	movq	aux_info_file(%rip), %rdi
	callq	fclose
	movq	global_dc(%rip), %rax
	movl	60(%rax), %eax
	testq	%rax, %rax
	je	.LBB21_532
# BB#531:                               # %if.then2.i30.i
	movq	aux_info_file_name(%rip), %rdi
	callq	unlink
.LBB21_532:                             # %if.end4.i.i
	movq	asm_out_file(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB21_550
# BB#533:                               # %if.then6.i.i48
	movq	%rbx, %rdi
	callq	ferror
	testq	%r12, %rax
	jne	.LBB21_534
# BB#548:                               # %if.end9.i.i
	movq	%rbx, %rdi
	callq	fclose
	testq	%r12, %rax
	jne	.LBB21_549
.LBB21_550:                             # %if.end14.i.i49
	movl	graph_dump_format(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_556
# BB#551:
	xorq	%r13, %r13
	movabsq	$dump_file+11, %rbx
	movabsq	$.L.str16, %r14
	leaq	144(%rsp), %r15
	.align	16, 0x90
.LBB21_552:                             # %for.body.i34.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB21_555
# BB#553:                               # %land.lhs.true.i35.i
                                        #   in Loop: Header=BB21_552 Depth=1
	movzbl	-2(%rbx), %eax
	testq	%rax, %rax
	je	.LBB21_555
# BB#554:                               # %if.then24.i.i
                                        #   in Loop: Header=BB21_552 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	sprintf
	movq	-11(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	concat
	movq	dump_base_name(%rip), %rdi
	movq	%rax, %r12
	movq	%r12, %rsi
	callq	finish_graph_dump_file
	movq	%r12, %rdi
	callq	free
.LBB21_555:                             # %for.inc.i.i
                                        #   in Loop: Header=BB21_552 Depth=1
	incq	%r13
	addq	$16, %rbx
	cmpq	$32, %r13
	jne	.LBB21_552
.LBB21_556:                             # %if.end31.i.i
	movl	mem_report(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_558
# BB#557:                               # %if.then33.i.i
	callq	ggc_print_statistics
	callq	stringpool_statistics
	callq	dump_tree_statistics
.LBB21_558:                             # %do_compile.exit
	callq	free_reg_info
	callq	*lang_hooks+48(%rip)
	xorq	%rdi, %rdi
	callq	timevar_stop
	movq	stderr(%rip), %rdi
	callq	timevar_print
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
.LBB21_559:                             # %if.end
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB21_560
# BB#564:                               # %return
	movq	$1, %rax
	jmp	.LBB21_563
.LBB21_560:                             # %lor.lhs.false
	movl	56(%rax), %eax
	testq	%rax, %rax
	je	.LBB21_561
# BB#562:                               # %lor.lhs.false
	movabsq	$1, %rax
	jmp	.LBB21_563
.LBB21_561:
	movabsq	$0, %rax
.LBB21_563:                             # %lor.lhs.false
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB21_471:                             # %if.then109.i.i
	movq	aux_info_file_name(%rip), %rcx
	movabsq	$.L.str350, %rdx
	jmp	.LBB21_472
.LBB21_547:                             # %if.then16.i.i.i
	movq	asm_file_name(%rip), %rcx
	movabsq	$.L.str23, %rdx
	jmp	.LBB21_472
.LBB21_534:                             # %if.then8.i.i
	movq	asm_file_name(%rip), %rcx
	movabsq	$.L.str14, %rdx
	jmp	.LBB21_472
.LBB21_549:                             # %if.then12.i.i
	movq	asm_file_name(%rip), %rcx
	movabsq	$.L.str15, %rdx
.LBB21_472:                             # %if.then109.i.i
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	fatal_io_error
.Ltmp208:
	.size	toplev_main, .Ltmp208-toplev_main
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI21_0:
	.quad	.LBB21_361
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_288
	.quad	.LBB21_389
	.quad	.LBB21_119
	.quad	.LBB21_243
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_385
	.quad	.LBB21_389
	.quad	.LBB21_368
	.quad	.LBB21_318
	.quad	.LBB21_324
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_389
	.quad	.LBB21_326
	.quad	.LBB21_328
.LJTI21_1:
	.quad	.LBB21_313
	.quad	.LBB21_300
	.quad	.LBB21_300
	.quad	.LBB21_300
	.quad	.LBB21_300
	.quad	.LBB21_300
	.quad	.LBB21_314
	.quad	.LBB21_300
	.quad	.LBB21_315
	.quad	.LBB21_316

	.text
	.align	16, 0x90
	.type	print_switch_values,@function
print_switch_values:                    # @print_switch_values
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp215:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp216:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp217:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp218:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp219:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp220:
	.cfi_def_cfa_offset 336
.Ltmp221:
	.cfi_offset %rbx, -48
.Ltmp222:
	.cfi_offset %r12, -40
.Ltmp223:
	.cfi_offset %r13, -32
.Ltmp224:
	.cfi_offset %r14, -24
.Ltmp225:
	.cfi_offset %r15, -16
	movzbl	(%rsi), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB22_2
# BB#1:
	movabsq	$.L.str26, %r15
	jmp	.LBB22_3
.LBB22_2:                               # %select.mid
	movabsq	$.L.str28, %r15
.LBB22_3:                               # %select.end
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %r12
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%r14, %rsi
	callq	fputs
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r13
	movabsq	$.L.str341, %rsi
	movabsq	$.L.str29, %rcx
	movabsq	$.L.str28, %r8
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	fprintf
	movq	save_argv(%rip), %r14
	movq	8(%r14), %r15
	leaq	16(%r12,%r13), %r13
	testq	%r15, %r15
	je	.LBB22_15
# BB#4:
	leaq	8(%r14), %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB22_5:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_6 Depth 2
	movq	%r14, %rbx
	movq	%rax, %r14
	.align	16, 0x90
.LBB22_6:                               # %for.body
                                        #   Parent Loop BB22_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r15), %eax
	cmpq	$45, %rax
	jne	.LBB22_7
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB22_6 Depth=2
	movq	%r15, %rdi
	movabsq	$.L.str30, %rsi
	callq	strcmp
	testq	%r12, %rax
	jne	.LBB22_11
# BB#9:                                 # %if.then8
                                        #   in Loop: Header=BB22_6 Depth=2
	cmpq	$0, 16(%rbx)
	je	.LBB22_7
# BB#10:                                # %select.mid28
                                        #   in Loop: Header=BB22_6 Depth=2
	addq	$16, %rbx
	movq	%rbx, %r14
	jmp	.LBB22_7
	.align	16, 0x90
.LBB22_11:                              # %if.end13
                                        #   in Loop: Header=BB22_6 Depth=2
	movq	%r15, %rdi
	movabsq	$.L.str31, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB22_7
# BB#12:                                # %if.end18
                                        #   in Loop: Header=BB22_6 Depth=2
	movq	%r15, %rdi
	movabsq	$.L.str32, %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB22_7
# BB#13:                                # %if.end23
                                        #   in Loop: Header=BB22_6 Depth=2
	movzbl	1(%r15), %eax
	cmpq	$100, %rax
	jne	.LBB22_14
	.align	16, 0x90
.LBB22_7:                               # %for.cond.backedge
                                        #   in Loop: Header=BB22_6 Depth=2
	movq	%r14, %rbx
	movq	8(%rbx), %r15
	leaq	8(%rbx), %r14
	testq	%r15, %r15
	jne	.LBB22_6
	jmp	.LBB22_15
.LBB22_14:                              # %if.end29
                                        #   in Loop: Header=BB22_5 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$.L.str26, %rcx
	movq	%r15, %r8
	movabsq	$.L.str28, %r9
	callq	print_single_switch
	movq	8(%r14), %r15
	movq	%rax, %r13
	leaq	8(%r14), %rax
	testq	%r15, %r15
	jne	.LBB22_5
.LBB22_15:                              # %for.end
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	jle	.LBB22_17
# BB#16:                                # %if.then35
	movq	$10, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	fputc
.LBB22_17:                              # %if.end37
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	%rax, %rax
	movabsq	$.L.str28, %r13
	je	.LBB22_19
# BB#18:
	movabsq	$.L.str26, %r13
.LBB22_19:                              # %select.end29
	movq	%r13, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	fputs
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r12
	movabsq	$.L.str341, %rsi
	movabsq	$.L.str33, %rcx
	movabsq	$.L.str28, %r8
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	fprintf
	leaq	17(%r15,%r12), %r15
	movq	$-3104, %rbx            # imm = 0xFFFFFFFFFFFFF3E0
	movabsq	$.L.str34, %r12
	movabsq	$target_switches+8, %r13
	.align	16, 0x90
.LBB22_20:                              # %for.body45
                                        # =>This Inner Loop Header: Depth=1
	movq	f_options+3112(%rbx), %rax
	movl	(%rax), %eax
	movl	f_options+3120(%rbx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB22_22
# BB#21:                                # %if.then50
                                        #   in Loop: Header=BB22_20 Depth=1
	movq	f_options+3104(%rbx), %r9
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$.L.str26, %rcx
	movq	%r12, %r8
	callq	print_single_switch
	movq	%rax, %r15
.LBB22_22:                              # %for.inc54
                                        #   in Loop: Header=BB22_20 Depth=1
	addq	$32, %rbx
	jne	.LBB22_20
# BB#23:
	xorq	%rbx, %rbx
	movabsq	$57260077759506453, %rdx # imm = 0xCB6DBCC2AAA815
	.align	16, 0x90
.LBB22_24:                              # %for.body59
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r13), %r9
	movzbl	(%r9), %eax
	testq	%rax, %rax
	je	.LBB22_29
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	%rdx, %rax
	movq	%rbx, %rcx
	shrq	%cl, %rax
	testq	$1, %rax
	je	.LBB22_29
# BB#26:                                # %land.lhs.true68
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	%rdx, %r12
	movl	(%r13), %eax
	movl	target_flags(%rip), %ecx
	andq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB22_28
# BB#27:                                # %if.then75
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$.L.str26, %rcx
	movabsq	$.L.str35, %r8
	callq	print_single_switch
	movq	%rax, %r15
.LBB22_28:                              # %for.inc80
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	%r12, %rdx
.LBB22_29:                              # %for.inc80
                                        #   in Loop: Header=BB22_24 Depth=1
	incq	%rbx
	addq	$24, %r13
	cmpq	$56, %rbx
	jne	.LBB22_24
# BB#30:                                # %for.cond83.preheader
	movq	$-312, %r12             # imm = 0xFFFFFFFFFFFFFEC8
	leaq	32(%rsp), %rbx
	.align	16, 0x90
.LBB22_31:                              # %for.body86
                                        # =>This Inner Loop Header: Depth=1
	movq	target_options+320(%r12), %r13
	cmpq	$0, (%r13)
	je	.LBB22_33
# BB#32:                                # %if.then91
                                        #   in Loop: Header=BB22_31 Depth=1
	movq	target_options+312(%r12), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str36, %rsi
	callq	sprintf
	movq	(%r13), %r9
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$.L.str26, %rcx
	movq	%rbx, %r8
	callq	print_single_switch
	movq	%rax, %r15
.LBB22_33:                              # %for.inc100
                                        #   in Loop: Header=BB22_31 Depth=1
	addq	$24, %r12
	jne	.LBB22_31
# BB#34:                                # %for.end102
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp226:
	.size	print_switch_values, .Ltmp226-print_switch_values
	.cfi_endproc

	.align	16, 0x90
	.type	print_single_switch,@function
print_single_switch:                    # @print_single_switch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp233:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp234:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp235:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp236:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp237:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp238:
	.cfi_def_cfa_offset 96
.Ltmp239:
	.cfi_offset %rbx, -48
.Ltmp240:
	.cfi_offset %r12, -40
.Ltmp241:
	.cfi_offset %r13, -32
.Ltmp242:
	.cfi_offset %r14, -24
.Ltmp243:
	.cfi_offset %r15, -16
	movq	%r9, %r12
	movq	%r12, 40(%rsp)          # 8-byte Spill
	movq	%r8, %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r13
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r15
	addq	%r14, %r15
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r14
	addq	%r15, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB23_3
# BB#1:                                 # %entry
	leaq	(%r14,%r13), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$76, %rdx
	jl	.LBB23_3
# BB#2:                                 # %if.end.thread
	movq	$10, %rdi
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	fputc
	jmp	.LBB23_4
.LBB23_3:                               # %if.end
	testq	%rax, %r13
	movq	16(%rsp), %r15          # 8-byte Reload
	jne	.LBB23_5
.LBB23_4:                               # %if.then11
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	fputs
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r13
.LBB23_5:                               # %if.end15
	movabsq	$.L.str341, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	fprintf
	addq	%r14, %r13
	movq	%r13, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp244:
	.size	print_single_switch, .Ltmp244-print_single_switch
	.cfi_endproc

	.align	16, 0x90
	.type	decl_name,@function
decl_name:                              # @decl_name
	.cfi_startproc
# BB#0:                                 # %entry
	movq	72(%rdi), %rax
	movq	32(%rax), %rax
	retq
.Ltmp245:
	.size	decl_name, .Ltmp245-decl_name
	.cfi_endproc

	.align	16, 0x90
	.type	set_target_switch,@function
set_target_switch:                      # @set_target_switch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp252:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp253:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp254:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp255:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp256:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp257:
	.cfi_def_cfa_offset 64
.Ltmp258:
	.cfi_offset %rbx, -48
.Ltmp259:
	.cfi_offset %r12, -40
.Ltmp260:
	.cfi_offset %r13, -32
.Ltmp261:
	.cfi_offset %r14, -24
.Ltmp262:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%r15, %r15
	movabsq	$target_switches+8, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%r13, %r13
	.align	16, 0x90
.LBB25_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rbx), %rdi
	movq	%r14, %rsi
	callq	strcmp
	testq	%r12, %rax
	jne	.LBB25_6
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB25_1 Depth=1
	movl	(%rbx), %eax
	movabsq	$14797515875766282, %rdx # imm = 0x3492432555500A
	movq	%r15, %rcx
	shrq	%cl, %rdx
	testq	$1, %rdx
	jne	.LBB25_3
# BB#4:                                 # %if.else
                                        #   in Loop: Header=BB25_1 Depth=1
	movl	target_flags(%rip), %ecx
	orq	%rax, %rcx
	jmp	.LBB25_5
	.align	16, 0x90
.LBB25_3:                               # %if.then4
                                        #   in Loop: Header=BB25_1 Depth=1
	movl	target_flags(%rip), %ecx
	decq	%rax
	andq	%rax, %rcx
.LBB25_5:                               # %if.end
                                        #   in Loop: Header=BB25_1 Depth=1
	movl	%ecx, target_flags(%rip)
	movq	$1, %r13
.LBB25_6:                               # %for.inc
                                        #   in Loop: Header=BB25_1 Depth=1
	incq	%r15
	addq	$24, %rbx
	cmpq	$56, %r15
	jne	.LBB25_1
# BB#7:                                 # %for.end
	testq	%r12, %r13
	jne	.LBB25_13
# BB#8:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movq	$-312, %r13             # imm = 0xFFFFFFFFFFFFFEC8
	.align	16, 0x90
.LBB25_9:                               # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	movq	target_options+312(%r13), %r15
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	strncmp
	testq	%r12, %rax
	jne	.LBB25_11
# BB#10:                                # %if.then22
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	target_options+320(%r13), %rax
	addq	%r14, %rbx
	movq	%rbx, (%rax)
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
.LBB25_11:                              # %for.inc25
                                        #   in Loop: Header=BB25_9 Depth=1
	addq	$24, %r13
	jne	.LBB25_9
# BB#12:                                # %if.end28
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r12, %rax
	je	.LBB25_14
.LBB25_13:                              # %if.end31
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_14:                              # %if.then30
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	error  # TAILCALL
.Ltmp263:
	.size	set_target_switch, .Ltmp263-set_target_switch
	.cfi_endproc

	.align	16, 0x90
	.type	display_target_options,@function
display_target_options:                 # @display_target_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp270:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp271:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp272:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp273:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp274:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp275:
	.cfi_def_cfa_offset 64
.Ltmp276:
	.cfi_offset %rbx, -48
.Ltmp277:
	.cfi_offset %r12, -40
.Ltmp278:
	.cfi_offset %r13, -32
.Ltmp279:
	.cfi_offset %r14, -24
.Ltmp280:
	.cfi_offset %r15, -16
	movzbl	display_target_options.displayed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB26_22
# BB#1:                                 # %if.end
	movq	$1, %rax
	movb	%al, display_target_options.displayed(%rip)
	movabsq	$.Lstr719, %rdi
	callq	puts
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movq	$55, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$36028797421617152, %r14 # imm = 0x80000018000000
	xorq	%r13, %r13
.LBB26_2:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB26_3 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	(%r15,%r15,2), %rax
	leaq	-1(%r15), %r15
	leaq	target_switches+16(,%rax,8), %rax
	.align	16, 0x90
.LBB26_3:                               # %for.body
                                        #   Parent Loop BB26_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-16(%rax), %rsi
	movzbl	(%rsi), %ecx
	leaq	1(%r15), %rbx
	testq	%rcx, %rcx
	je	.LBB26_4
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB26_3 Depth=2
	movq	%r14, %rdx
	movq	%rbx, %rcx
	shrq	%cl, %rdx
	testq	$1, %rdx
	jne	.LBB26_6
# BB#9:                                 # %if.else15
                                        #   in Loop: Header=BB26_3 Depth=2
	movq	(%rax), %rdx
	movzbl	(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB26_10
.LBB26_4:                               # %for.cond.backedge
                                        #   in Loop: Header=BB26_3 Depth=2
	decq	%r15
	addq	$-24, %rax
	testq	%r12, %rbx
	jne	.LBB26_3
	jmp	.LBB26_11
	.align	16, 0x90
.LBB26_6:                               # %if.then10
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB26_8
# BB#7:                                 # %if.then12
                                        #   in Loop: Header=BB26_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str614, %rdi
	callq	printf
.LBB26_8:                               # %for.cond.outer89.backedge
                                        #   in Loop: Header=BB26_2 Depth=1
	testq	%r12, %rbx
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jne	.LBB26_2
	jmp	.LBB26_11
	.align	16, 0x90
.LBB26_10:                              # %if.then19
                                        #   in Loop: Header=BB26_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str615, %rdi
	callq	printf
	addq	%rax, %r13
	testq	%r12, %rbx
	jne	.LBB26_2
.LBB26_11:                              # %for.cond24.preheader
	movq	$12, %rbx
	movabsq	$.L.str615, %r14
.LBB26_20:                              # %for.body27.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB26_12 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx,2), %rax
	leaq	-1(%rbx), %rbx
	leaq	target_options+16(,%rax,8), %rax
	.align	16, 0x90
.LBB26_12:                              # %for.body27
                                        #   Parent Loop BB26_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-16(%rax), %rsi
	movzbl	(%rsi), %ecx
	leaq	1(%rbx), %r15
	testq	%rcx, %rcx
	je	.LBB26_13
# BB#18:                                # %if.else50
                                        #   in Loop: Header=BB26_12 Depth=2
	movq	(%rax), %rdx
	movzbl	(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB26_19
.LBB26_13:                              # %for.cond24.backedge
                                        #   in Loop: Header=BB26_12 Depth=2
	decq	%rbx
	addq	$-24, %rax
	testq	%r12, %r15
	jne	.LBB26_12
	jmp	.LBB26_14
	.align	16, 0x90
.LBB26_19:                              # %if.then54
                                        #   in Loop: Header=BB26_20 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	addq	%rax, %r13
	testq	%r12, %r15
	jne	.LBB26_20
.LBB26_14:                              # %for.end60
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r12, %rax
	je	.LBB26_22
# BB#15:                                # %if.then62
	testq	%r12, %r13
	je	.LBB26_21
# BB#16:                                # %if.then64
	movabsq	$.Lstr721, %rdi
	jmp	.LBB26_17
.LBB26_22:                              # %if.end69
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_21:                              # %if.else66
	movabsq	$.Lstr720, %rdi
.LBB26_17:                              # %if.then64
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	puts  # TAILCALL
.Ltmp281:
	.size	display_target_options, .Ltmp281-display_target_options
	.cfi_endproc

	.align	16, 0x90
	.type	float_signal,@function
float_signal:                           # @float_signal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp283:
	.cfi_def_cfa_offset 16
	movl	float_handled(%rip), %eax
	testq	%rax, %rax
	jne	.LBB27_2
# BB#1:                                 # %if.then
	callq	crash_signal
.LBB27_2:                               # %if.end
	xorq	%rax, %rax
	movl	%eax, float_handled(%rip)
	movabsq	$float_signal, %rsi
	movq	$8, %rdi
	callq	signal
	movabsq	$float_handler, %rdi
	movq	$1, %rsi
	callq	longjmp
.Ltmp284:
	.size	float_signal, .Ltmp284-float_signal
	.cfi_endproc

	.align	16, 0x90
	.type	crash_signal,@function
crash_signal:                           # @crash_signal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp286:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movabsq	$.L.str664, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	internal_error
.Ltmp287:
	.size	crash_signal, .Ltmp287-crash_signal
	.cfi_endproc

	.type	debug_hooks,@object     # @debug_hooks
	.data
	.globl	debug_hooks
	.align	8
debug_hooks:
	.quad	do_nothing_debug_hooks
	.size	debug_hooks, 8

	.type	write_symbols,@object   # @write_symbols
	.bss
	.globl	write_symbols
	.align	4
write_symbols:
	.long	0                       # 0x0
	.size	write_symbols, 4

	.type	debug_info_level,@object # @debug_info_level
	.globl	debug_info_level
	.align	4
debug_info_level:
	.long	0                       # 0x0
	.size	debug_info_level, 4

	.type	use_gnu_debug_info_extensions,@object # @use_gnu_debug_info_extensions
	.globl	use_gnu_debug_info_extensions
	.align	4
use_gnu_debug_info_extensions:
	.long	0                       # 0x0
	.size	use_gnu_debug_info_extensions, 4

	.type	optimize,@object        # @optimize
	.data
	.globl	optimize
	.align	4
optimize:
	.long	3                       # 0x3
	.size	optimize, 4

	.type	optimize_size,@object   # @optimize_size
	.bss
	.globl	optimize_size
	.align	4
optimize_size:
	.long	0                       # 0x0
	.size	optimize_size, 4

	.type	lang_expand_expr,@object # @lang_expand_expr
	.globl	lang_expand_expr
	.align	8
lang_expand_expr:
	.quad	0
	.size	lang_expand_expr, 8

	.type	incomplete_decl_finalize_hook,@object # @incomplete_decl_finalize_hook
	.globl	incomplete_decl_finalize_hook
	.align	8
incomplete_decl_finalize_hook:
	.quad	0
	.size	incomplete_decl_finalize_hook, 8

	.type	flag_eliminate_dwarf2_dups,@object # @flag_eliminate_dwarf2_dups
	.globl	flag_eliminate_dwarf2_dups
	.align	4
flag_eliminate_dwarf2_dups:
	.long	0                       # 0x0
	.size	flag_eliminate_dwarf2_dups, 4

	.type	profile_flag,@object    # @profile_flag
	.globl	profile_flag
	.align	4
profile_flag:
	.long	0                       # 0x0
	.size	profile_flag, 4

	.type	profile_arc_flag,@object # @profile_arc_flag
	.globl	profile_arc_flag
	.align	4
profile_arc_flag:
	.long	0                       # 0x0
	.size	profile_arc_flag, 4

	.type	flag_test_coverage,@object # @flag_test_coverage
	.globl	flag_test_coverage
	.align	4
flag_test_coverage:
	.long	0                       # 0x0
	.size	flag_test_coverage, 4

	.type	flag_branch_probabilities,@object # @flag_branch_probabilities
	.globl	flag_branch_probabilities
	.align	4
flag_branch_probabilities:
	.long	0                       # 0x0
	.size	flag_branch_probabilities, 4

	.type	flag_reorder_blocks,@object # @flag_reorder_blocks
	.globl	flag_reorder_blocks
	.align	4
flag_reorder_blocks:
	.long	0                       # 0x0
	.size	flag_reorder_blocks, 4

	.type	flag_rename_registers,@object # @flag_rename_registers
	.globl	flag_rename_registers
	.align	4
flag_rename_registers:
	.long	0                       # 0x0
	.size	flag_rename_registers, 4

	.type	flag_cprop_registers,@object # @flag_cprop_registers
	.globl	flag_cprop_registers
	.align	4
flag_cprop_registers:
	.long	0                       # 0x0
	.size	flag_cprop_registers, 4

	.type	pedantic,@object        # @pedantic
	.globl	pedantic
	.align	4
pedantic:
	.long	0                       # 0x0
	.size	pedantic, 4

	.type	in_system_header,@object # @in_system_header
	.globl	in_system_header
	.align	4
in_system_header:
	.long	0                       # 0x0
	.size	in_system_header, 4

	.type	quiet_flag,@object      # @quiet_flag
	.data
	.globl	quiet_flag
	.align	4
quiet_flag:
	.long	1                       # 0x1
	.size	quiet_flag, 4

	.type	time_report,@object     # @time_report
	.bss
	.globl	time_report
	.align	4
time_report:
	.long	0                       # 0x0
	.size	time_report, 4

	.type	mem_report,@object      # @mem_report
	.globl	mem_report
	.align	4
mem_report:
	.long	0                       # 0x0
	.size	mem_report, 4

	.type	flag_detailed_statistics,@object # @flag_detailed_statistics
	.globl	flag_detailed_statistics
	.align	4
flag_detailed_statistics:
	.long	0                       # 0x0
	.size	flag_detailed_statistics, 4

	.type	flag_caller_saves,@object # @flag_caller_saves
	.globl	flag_caller_saves
	.align	4
flag_caller_saves:
	.long	0                       # 0x0
	.size	flag_caller_saves, 4

	.type	flag_pcc_struct_return,@object # @flag_pcc_struct_return
	.data
	.globl	flag_pcc_struct_return
	.align	4
flag_pcc_struct_return:
	.long	1                       # 0x1
	.size	flag_pcc_struct_return, 4

	.type	flag_force_mem,@object  # @flag_force_mem
	.bss
	.globl	flag_force_mem
	.align	4
flag_force_mem:
	.long	0                       # 0x0
	.size	flag_force_mem, 4

	.type	flag_force_addr,@object # @flag_force_addr
	.data
	.globl	flag_force_addr
	.align	4
flag_force_addr:
	.long	1                       # 0x1
	.size	flag_force_addr, 4

	.type	flag_defer_pop,@object  # @flag_defer_pop
	.bss
	.globl	flag_defer_pop
	.align	4
flag_defer_pop:
	.long	0                       # 0x0
	.size	flag_defer_pop, 4

	.type	flag_float_store,@object # @flag_float_store
	.globl	flag_float_store
	.align	4
flag_float_store:
	.long	0                       # 0x0
	.size	flag_float_store, 4

	.type	flag_strength_reduce,@object # @flag_strength_reduce
	.globl	flag_strength_reduce
	.align	4
flag_strength_reduce:
	.long	0                       # 0x0
	.size	flag_strength_reduce, 4

	.type	flag_move_all_movables,@object # @flag_move_all_movables
	.globl	flag_move_all_movables
	.align	4
flag_move_all_movables:
	.long	0                       # 0x0
	.size	flag_move_all_movables, 4

	.type	flag_reduce_all_givs,@object # @flag_reduce_all_givs
	.globl	flag_reduce_all_givs
	.align	4
flag_reduce_all_givs:
	.long	0                       # 0x0
	.size	flag_reduce_all_givs, 4

	.type	flag_regmove,@object    # @flag_regmove
	.globl	flag_regmove
	.align	4
flag_regmove:
	.long	0                       # 0x0
	.size	flag_regmove, 4

	.type	flag_writable_strings,@object # @flag_writable_strings
	.globl	flag_writable_strings
	.align	4
flag_writable_strings:
	.long	0                       # 0x0
	.size	flag_writable_strings, 4

	.type	flag_no_function_cse,@object # @flag_no_function_cse
	.globl	flag_no_function_cse
	.align	4
flag_no_function_cse:
	.long	0                       # 0x0
	.size	flag_no_function_cse, 4

	.type	flag_omit_frame_pointer,@object # @flag_omit_frame_pointer
	.data
	.globl	flag_omit_frame_pointer
	.align	4
flag_omit_frame_pointer:
	.long	1                       # 0x1
	.size	flag_omit_frame_pointer, 4

	.type	flag_function_sections,@object # @flag_function_sections
	.bss
	.globl	flag_function_sections
	.align	4
flag_function_sections:
	.long	0                       # 0x0
	.size	flag_function_sections, 4

	.type	flag_data_sections,@object # @flag_data_sections
	.globl	flag_data_sections
	.align	4
flag_data_sections:
	.long	0                       # 0x0
	.size	flag_data_sections, 4

	.type	flag_no_peephole,@object # @flag_no_peephole
	.globl	flag_no_peephole
	.align	4
flag_no_peephole:
	.long	0                       # 0x0
	.size	flag_no_peephole, 4

	.type	flag_optimize_sibling_calls,@object # @flag_optimize_sibling_calls
	.globl	flag_optimize_sibling_calls
	.align	4
flag_optimize_sibling_calls:
	.long	0                       # 0x0
	.size	flag_optimize_sibling_calls, 4

	.type	flag_errno_math,@object # @flag_errno_math
	.data
	.globl	flag_errno_math
	.align	4
flag_errno_math:
	.long	1                       # 0x1
	.size	flag_errno_math, 4

	.type	flag_unsafe_math_optimizations,@object # @flag_unsafe_math_optimizations
	.globl	flag_unsafe_math_optimizations
	.align	4
flag_unsafe_math_optimizations:
	.long	1                       # 0x1
	.size	flag_unsafe_math_optimizations, 4

	.type	flag_trapping_math,@object # @flag_trapping_math
	.globl	flag_trapping_math
	.align	4
flag_trapping_math:
	.long	1                       # 0x1
	.size	flag_trapping_math, 4

	.type	flag_complex_divide_method,@object # @flag_complex_divide_method
	.bss
	.globl	flag_complex_divide_method
	.align	4
flag_complex_divide_method:
	.long	0                       # 0x0
	.size	flag_complex_divide_method, 4

	.type	flag_syntax_only,@object # @flag_syntax_only
	.globl	flag_syntax_only
	.align	4
flag_syntax_only:
	.long	0                       # 0x0
	.size	flag_syntax_only, 4

	.type	flag_gcse_lm,@object    # @flag_gcse_lm
	.data
	.globl	flag_gcse_lm
	.align	4
flag_gcse_lm:
	.long	1                       # 0x1
	.size	flag_gcse_lm, 4

	.type	flag_gcse_sm,@object    # @flag_gcse_sm
	.globl	flag_gcse_sm
	.align	4
flag_gcse_sm:
	.long	1                       # 0x1
	.size	flag_gcse_sm, 4

	.type	flag_rerun_loop_opt,@object # @flag_rerun_loop_opt
	.globl	flag_rerun_loop_opt
	.align	4
flag_rerun_loop_opt:
	.long	1                       # 0x1
	.size	flag_rerun_loop_opt, 4

	.type	flag_no_inline,@object  # @flag_no_inline
	.globl	flag_no_inline
	.align	4
flag_no_inline:
	.long	2                       # 0x2
	.size	flag_no_inline, 4

	.type	flag_really_no_inline,@object # @flag_really_no_inline
	.globl	flag_really_no_inline
	.align	4
flag_really_no_inline:
	.long	2                       # 0x2
	.size	flag_really_no_inline, 4

	.type	flag_keep_static_consts,@object # @flag_keep_static_consts
	.globl	flag_keep_static_consts
	.align	4
flag_keep_static_consts:
	.long	1                       # 0x1
	.size	flag_keep_static_consts, 4

	.type	flag_gen_aux_info,@object # @flag_gen_aux_info
	.bss
	.globl	flag_gen_aux_info
	.align	4
flag_gen_aux_info:
	.long	0                       # 0x0
	.size	flag_gen_aux_info, 4

	.type	flag_unwind_tables,@object # @flag_unwind_tables
	.globl	flag_unwind_tables
	.align	4
flag_unwind_tables:
	.long	0                       # 0x0
	.size	flag_unwind_tables, 4

	.type	flag_asynchronous_unwind_tables,@object # @flag_asynchronous_unwind_tables
	.globl	flag_asynchronous_unwind_tables
	.align	4
flag_asynchronous_unwind_tables:
	.long	0                       # 0x0
	.size	flag_asynchronous_unwind_tables, 4

	.type	flag_pedantic_errors,@object # @flag_pedantic_errors
	.globl	flag_pedantic_errors
	.align	4
flag_pedantic_errors:
	.long	0                       # 0x0
	.size	flag_pedantic_errors, 4

	.type	flag_schedule_insns,@object # @flag_schedule_insns
	.globl	flag_schedule_insns
	.align	4
flag_schedule_insns:
	.long	0                       # 0x0
	.size	flag_schedule_insns, 4

	.type	flag_schedule_insns_after_reload,@object # @flag_schedule_insns_after_reload
	.globl	flag_schedule_insns_after_reload
	.align	4
flag_schedule_insns_after_reload:
	.long	0                       # 0x0
	.size	flag_schedule_insns_after_reload, 4

	.type	flag_schedule_interblock,@object # @flag_schedule_interblock
	.data
	.globl	flag_schedule_interblock
	.align	4
flag_schedule_interblock:
	.long	1                       # 0x1
	.size	flag_schedule_interblock, 4

	.type	flag_schedule_speculative,@object # @flag_schedule_speculative
	.globl	flag_schedule_speculative
	.align	4
flag_schedule_speculative:
	.long	1                       # 0x1
	.size	flag_schedule_speculative, 4

	.type	flag_schedule_speculative_load,@object # @flag_schedule_speculative_load
	.bss
	.globl	flag_schedule_speculative_load
	.align	4
flag_schedule_speculative_load:
	.long	0                       # 0x0
	.size	flag_schedule_speculative_load, 4

	.type	flag_schedule_speculative_load_dangerous,@object # @flag_schedule_speculative_load_dangerous
	.globl	flag_schedule_speculative_load_dangerous
	.align	4
flag_schedule_speculative_load_dangerous:
	.long	0                       # 0x0
	.size	flag_schedule_speculative_load_dangerous, 4

	.type	flag_branch_on_count_reg,@object # @flag_branch_on_count_reg
	.data
	.globl	flag_branch_on_count_reg
	.align	4
flag_branch_on_count_reg:
	.long	1                       # 0x1
	.size	flag_branch_on_count_reg, 4

	.type	flag_inhibit_size_directive,@object # @flag_inhibit_size_directive
	.bss
	.globl	flag_inhibit_size_directive
	.align	4
flag_inhibit_size_directive:
	.long	0                       # 0x0
	.size	flag_inhibit_size_directive, 4

	.type	flag_verbose_asm,@object # @flag_verbose_asm
	.globl	flag_verbose_asm
	.align	4
flag_verbose_asm:
	.long	0                       # 0x0
	.size	flag_verbose_asm, 4

	.type	flag_debug_asm,@object  # @flag_debug_asm
	.globl	flag_debug_asm
	.align	4
flag_debug_asm:
	.long	0                       # 0x0
	.size	flag_debug_asm, 4

	.type	flag_dump_rtl_in_asm,@object # @flag_dump_rtl_in_asm
	.globl	flag_dump_rtl_in_asm
	.align	4
flag_dump_rtl_in_asm:
	.long	0                       # 0x0
	.size	flag_dump_rtl_in_asm, 4

	.type	flag_gnu_linker,@object # @flag_gnu_linker
	.data
	.globl	flag_gnu_linker
	.align	4
flag_gnu_linker:
	.long	1                       # 0x1
	.size	flag_gnu_linker, 4

	.type	flag_ssa,@object        # @flag_ssa
	.globl	flag_ssa
	.align	4
flag_ssa:
	.long	1                       # 0x1
	.size	flag_ssa, 4

	.type	flag_ssa_ccp,@object    # @flag_ssa_ccp
	.bss
	.globl	flag_ssa_ccp
	.align	4
flag_ssa_ccp:
	.long	0                       # 0x0
	.size	flag_ssa_ccp, 4

	.type	flag_ssa_dce,@object    # @flag_ssa_dce
	.data
	.globl	flag_ssa_dce
	.align	4
flag_ssa_dce:
	.long	1                       # 0x1
	.size	flag_ssa_dce, 4

	.type	flag_pack_struct,@object # @flag_pack_struct
	.bss
	.globl	flag_pack_struct
	.align	4
flag_pack_struct:
	.long	0                       # 0x0
	.size	flag_pack_struct, 4

	.type	flag_argument_noalias,@object # @flag_argument_noalias
	.globl	flag_argument_noalias
	.align	4
flag_argument_noalias:
	.long	0                       # 0x0
	.size	flag_argument_noalias, 4

	.type	flag_strict_aliasing,@object # @flag_strict_aliasing
	.globl	flag_strict_aliasing
	.align	4
flag_strict_aliasing:
	.long	0                       # 0x0
	.size	flag_strict_aliasing, 4

	.type	flag_instrument_function_entry_exit,@object # @flag_instrument_function_entry_exit
	.globl	flag_instrument_function_entry_exit
	.align	4
flag_instrument_function_entry_exit:
	.long	0                       # 0x0
	.size	flag_instrument_function_entry_exit, 4

	.type	flag_no_ident,@object   # @flag_no_ident
	.globl	flag_no_ident
	.align	4
flag_no_ident:
	.long	0                       # 0x0
	.size	flag_no_ident, 4

	.type	flag_peephole2,@object  # @flag_peephole2
	.globl	flag_peephole2
	.align	4
flag_peephole2:
	.long	0                       # 0x0
	.size	flag_peephole2, 4

	.type	flag_guess_branch_prob,@object # @flag_guess_branch_prob
	.globl	flag_guess_branch_prob
	.align	4
flag_guess_branch_prob:
	.long	0                       # 0x0
	.size	flag_guess_branch_prob, 4

	.type	flag_bounded_pointers,@object # @flag_bounded_pointers
	.globl	flag_bounded_pointers
	.align	4
flag_bounded_pointers:
	.long	0                       # 0x0
	.size	flag_bounded_pointers, 4

	.type	flag_bounds_check,@object # @flag_bounds_check
	.globl	flag_bounds_check
	.align	4
flag_bounds_check:
	.long	0                       # 0x0
	.size	flag_bounds_check, 4

	.type	flag_merge_constants,@object # @flag_merge_constants
	.data
	.globl	flag_merge_constants
	.align	4
flag_merge_constants:
	.long	1                       # 0x1
	.size	flag_merge_constants, 4

	.type	flag_renumber_insns,@object # @flag_renumber_insns
	.globl	flag_renumber_insns
	.align	4
flag_renumber_insns:
	.long	1                       # 0x1
	.size	flag_renumber_insns, 4

	.type	flag_trapv,@object      # @flag_trapv
	.bss
	.globl	flag_trapv
	.align	4
flag_trapv:
	.long	0                       # 0x0
	.size	flag_trapv, 4

	.type	flag_leading_underscore,@object # @flag_leading_underscore
	.data
	.globl	flag_leading_underscore
	.align	4
flag_leading_underscore:
	.long	4294967295              # 0xffffffff
	.size	flag_leading_underscore, 4

	.type	inhibit_warnings,@object # @inhibit_warnings
	.bss
	.globl	inhibit_warnings
	.align	4
inhibit_warnings:
	.long	0                       # 0x0
	.size	inhibit_warnings, 4

	.type	warn_system_headers,@object # @warn_system_headers
	.globl	warn_system_headers
	.align	4
warn_system_headers:
	.long	0                       # 0x0
	.size	warn_system_headers, 4

	.type	extra_warnings,@object  # @extra_warnings
	.globl	extra_warnings
	.align	4
extra_warnings:
	.long	0                       # 0x0
	.size	extra_warnings, 4

	.type	warnings_are_errors,@object # @warnings_are_errors
	.globl	warnings_are_errors
	.align	4
warnings_are_errors:
	.long	0                       # 0x0
	.size	warnings_are_errors, 4

	.type	warn_deprecated_decl,@object # @warn_deprecated_decl
	.data
	.globl	warn_deprecated_decl
	.align	4
warn_deprecated_decl:
	.long	1                       # 0x1
	.size	warn_deprecated_decl, 4

	.type	warn_unused_function,@object # @warn_unused_function
	.comm	warn_unused_function,4,4
	.type	warn_unused_label,@object # @warn_unused_label
	.comm	warn_unused_label,4,4
	.type	warn_unused_parameter,@object # @warn_unused_parameter
	.comm	warn_unused_parameter,4,4
	.type	warn_unused_variable,@object # @warn_unused_variable
	.comm	warn_unused_variable,4,4
	.type	warn_unused_value,@object # @warn_unused_value
	.comm	warn_unused_value,4,4
	.type	rtl_dump_file,@object   # @rtl_dump_file
	.bss
	.globl	rtl_dump_file
	.align	8
rtl_dump_file:
	.quad	0
	.size	rtl_dump_file, 8

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"invalid option `%s'"
	.size	.L.str, 20

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"toplev.c"
	.size	.L.str1, 9

	.type	.L__FUNCTION__.do_abort,@object # @__FUNCTION__.do_abort
.L__FUNCTION__.do_abort:
	.asciz	"do_abort"
	.size	.L__FUNCTION__.do_abort, 9

	.type	.L__FUNCTION__.botch,@object # @__FUNCTION__.botch
.L__FUNCTION__.botch:
	.asciz	"botch"
	.size	.L__FUNCTION__.botch, 6

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\\%03o"
	.size	.L.str2, 6

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"\t.file\t"
	.size	.L.str3, 8

	.type	flag_keep_inline_functions,@object # @flag_keep_inline_functions
	.comm	flag_keep_inline_functions,4,4
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"`%s' used but never defined"
	.size	.L.str4, 28

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"`%s' declared `static' but never defined"
	.size	.L.str5, 41

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"`%s' defined but not used"
	.size	.L.str6, 26

	.type	input_file_stack,@object # @input_file_stack
	.comm	input_file_stack,8,8
	.type	input_filename,@object  # @input_filename
	.comm	input_filename,8,8
	.type	lineno,@object          # @lineno
	.comm	lineno,4,4
	.type	input_file_stack_tick,@object # @input_file_stack_tick
	.comm	input_file_stack_tick,4,4
	.type	.L__FUNCTION__.pop_srcloc,@object # @__FUNCTION__.pop_srcloc
.L__FUNCTION__.pop_srcloc:
	.asciz	"pop_srcloc"
	.size	.L__FUNCTION__.pop_srcloc, 11

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"alias"
	.size	.L.str7, 6

	.type	asm_out_file,@object    # @asm_out_file
	.comm	asm_out_file,8,8
	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\t.size\t"
	.size	.L.str9, 8

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%lld"
	.size	.L.str10, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"invalid register name `%s' for register variable"
	.size	.L.str11, 49

	.type	current_function_decl,@object # @current_function_decl
	.comm	current_function_decl,8,8
	.type	flag_inline_functions,@object # @flag_inline_functions
	.comm	flag_inline_functions,4,4
	.type	warn_inline,@object     # @warn_inline
	.comm	warn_inline,4,4
	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	";; (integrable)\n\n"
	.size	.L.str12, 18

	.type	warn_return_type,@object # @warn_return_type
	.comm	warn_return_type,4,4
	.type	rtl_dump_and_exit,@object # @rtl_dump_and_exit
	.comm	rtl_dump_and_exit,4,4
	.type	flag_pic,@object        # @flag_pic
	.comm	flag_pic,4,4
	.type	flag_thread_jumps,@object # @flag_thread_jumps
	.comm	flag_thread_jumps,4,4
	.type	flag_delete_null_pointer_checks,@object # @flag_delete_null_pointer_checks
	.local	flag_delete_null_pointer_checks
	.comm	flag_delete_null_pointer_checks,4,4
	.type	flag_rerun_cse_after_loop,@object # @flag_rerun_cse_after_loop
	.data
	.align	4
flag_rerun_cse_after_loop:
	.long	1                       # 0x1
	.size	flag_rerun_cse_after_loop, 4

	.type	flag_gcse,@object       # @flag_gcse
	.local	flag_gcse
	.comm	flag_gcse,4,4
	.type	flag_cse_skip_blocks,@object # @flag_cse_skip_blocks
	.comm	flag_cse_skip_blocks,4,4
	.type	flag_cse_follow_jumps,@object # @flag_cse_follow_jumps
	.comm	flag_cse_follow_jumps,4,4
	.type	flag_expensive_optimizations,@object # @flag_expensive_optimizations
	.comm	flag_expensive_optimizations,4,4
	.type	flag_unroll_loops,@object # @flag_unroll_loops
	.comm	flag_unroll_loops,4,4
	.type	flag_prefetch_loop_arrays,@object # @flag_prefetch_loop_arrays
	.comm	flag_prefetch_loop_arrays,4,4
	.type	no_new_pseudos,@object  # @no_new_pseudos
	.comm	no_new_pseudos,4,4
	.type	warn_uninitialized,@object # @warn_uninitialized
	.comm	warn_uninitialized,4,4
	.type	warn_disabled_optimization,@object # @warn_disabled_optimization
	.comm	warn_disabled_optimization,4,4
	.type	.L.str13,@object        # @.str13
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str13:
	.asciz	"crossjump disabled: %d > 1000 basic blocks and %d >= 20 edges/basic block"
	.size	.L.str13, 74

	.type	.L__FUNCTION__.rest_of_compilation,@object # @__FUNCTION__.rest_of_compilation
.L__FUNCTION__.rest_of_compilation:
	.asciz	"rest_of_compilation"
	.size	.L__FUNCTION__.rest_of_compilation, 20

	.type	exit_after_options,@object # @exit_after_options
	.local	exit_after_options
	.comm	exit_after_options,1,1
	.type	progname,@object        # @progname
	.comm	progname,8,8
	.type	save_argc,@object       # @save_argc
	.comm	save_argc,4,4
	.type	save_argv,@object       # @save_argv
	.comm	save_argv,8,8
	.type	main_input_filename,@object # @main_input_filename
	.comm	main_input_filename,8,8
	.type	dump_base_name,@object  # @dump_base_name
	.comm	dump_base_name,8,8
	.type	flag_print_asm_name,@object # @flag_print_asm_name
	.comm	flag_print_asm_name,4,4
	.type	graph_dump_format,@object # @graph_dump_format
	.comm	graph_dump_format,4,4
	.type	asm_file_name,@object   # @asm_file_name
	.comm	asm_file_name,8,8
	.type	g_switch_value,@object  # @g_switch_value
	.comm	g_switch_value,4,4
	.type	g_switch_set,@object    # @g_switch_set
	.comm	g_switch_set,4,4
	.type	current_function_func_begin_label,@object # @current_function_func_begin_label
	.comm	current_function_func_begin_label,8,8
	.type	decl_printable_name,@object # @decl_printable_name
	.comm	decl_printable_name,8,8
	.type	flag_signed_char,@object # @flag_signed_char
	.comm	flag_signed_char,4,4
	.type	flag_short_enums,@object # @flag_short_enums
	.comm	flag_short_enums,4,4
	.type	flag_unroll_all_loops,@object # @flag_unroll_all_loops
	.comm	flag_unroll_all_loops,4,4
	.type	flag_volatile,@object   # @flag_volatile
	.comm	flag_volatile,4,4
	.type	flag_volatile_global,@object # @flag_volatile_global
	.comm	flag_volatile_global,4,4
	.type	flag_volatile_static,@object # @flag_volatile_static
	.comm	flag_volatile_static,4,4
	.type	flag_shared_data,@object # @flag_shared_data
	.comm	flag_shared_data,4,4
	.type	flag_delayed_branch,@object # @flag_delayed_branch
	.comm	flag_delayed_branch,4,4
	.type	flag_exceptions,@object # @flag_exceptions
	.comm	flag_exceptions,4,4
	.type	flag_no_common,@object  # @flag_no_common
	.comm	flag_no_common,4,4
	.type	flag_pretend_float,@object # @flag_pretend_float
	.comm	flag_pretend_float,4,4
	.type	flag_single_precision_constant,@object # @flag_single_precision_constant
	.comm	flag_single_precision_constant,4,4
	.type	flag_stack_check,@object # @flag_stack_check
	.comm	flag_stack_check,4,4
	.type	stack_limit_rtx,@object # @stack_limit_rtx
	.comm	stack_limit_rtx,8,8
	.type	align_loops,@object     # @align_loops
	.comm	align_loops,4,4
	.type	align_loops_log,@object # @align_loops_log
	.comm	align_loops_log,4,4
	.type	align_loops_max_skip,@object # @align_loops_max_skip
	.comm	align_loops_max_skip,4,4
	.type	align_jumps,@object     # @align_jumps
	.comm	align_jumps,4,4
	.type	align_jumps_log,@object # @align_jumps_log
	.comm	align_jumps_log,4,4
	.type	align_jumps_max_skip,@object # @align_jumps_max_skip
	.comm	align_jumps_max_skip,4,4
	.type	align_labels,@object    # @align_labels
	.comm	align_labels,4,4
	.type	align_labels_log,@object # @align_labels_log
	.comm	align_labels_log,4,4
	.type	align_labels_max_skip,@object # @align_labels_max_skip
	.comm	align_labels_max_skip,4,4
	.type	align_functions,@object # @align_functions
	.comm	align_functions,4,4
	.type	align_functions_log,@object # @align_functions_log
	.comm	align_functions_log,4,4
	.type	user_label_prefix,@object # @user_label_prefix
	.comm	user_label_prefix,8,8
	.type	warn_notreached,@object # @warn_notreached
	.comm	warn_notreached,4,4
	.type	warn_shadow,@object     # @warn_shadow
	.comm	warn_shadow,4,4
	.type	warn_switch,@object     # @warn_switch
	.comm	warn_switch,4,4
	.type	warn_cast_align,@object # @warn_cast_align
	.comm	warn_cast_align,4,4
	.type	warn_larger_than,@object # @warn_larger_than
	.comm	warn_larger_than,4,4
	.type	larger_than_size,@object # @larger_than_size
	.comm	larger_than_size,8,8
	.type	warn_aggregate_return,@object # @warn_aggregate_return
	.comm	warn_aggregate_return,4,4
	.type	warn_packed,@object     # @warn_packed
	.comm	warn_packed,4,4
	.type	warn_padded,@object     # @warn_padded
	.comm	warn_padded,4,4
	.type	warn_missing_noreturn,@object # @warn_missing_noreturn
	.comm	warn_missing_noreturn,4,4
	.type	aux_info_file,@object   # @aux_info_file
	.comm	aux_info_file,8,8
	.type	float_handled,@object   # @float_handled
	.comm	float_handled,4,4
	.type	float_handler,@object   # @float_handler
	.comm	float_handler,200,16
	.type	filename,@object        # @filename
	.local	filename
	.comm	filename,8,8
	.type	aux_info_file_name,@object # @aux_info_file_name
	.local	aux_info_file_name
	.comm	aux_info_file_name,8,8
	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"error writing to %s"
	.size	.L.str14, 20

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"error closing %s"
	.size	.L.str15, 17

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	".%02d."
	.size	.L.str16, 7

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"%s\"GCC: (GNU) %s\"\n"
	.size	.L.str17, 19

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"\t.ident\t"
	.size	.L.str18, 9

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"gccdump"
	.size	.L.str19, 8

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	".s"
	.size	.L.str20, 3

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"-"
	.size	.L.str21, 2

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"w"
	.size	.L.str22, 2

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"can't open %s for writing"
	.size	.L.str23, 26

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"\t.intel_syntax\n"
	.size	.L.str24, 16

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"#"
	.size	.L.str25, 2

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	" "
	.size	.L.str26, 2

	.type	.L.str28,@object        # @.str28
.L.str28:
	.zero	1
	.size	.L.str28, 1

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"options passed: "
	.size	.L.str29, 17

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"-o"
	.size	.L.str30, 3

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"-quiet"
	.size	.L.str31, 7

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"-version"
	.size	.L.str32, 9

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"options enabled: "
	.size	.L.str33, 18

	.type	f_options,@object       # @f_options
	.section	.rodata,"a",@progbits
	.align	16
f_options:
	.quad	.L.str148
	.quad	flag_eliminate_dwarf2_dups
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str149
	.quad	.L.str150
	.quad	flag_float_store
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str151
	.quad	.L.str152
	.quad	flag_volatile
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str153
	.quad	.L.str154
	.quad	flag_volatile_global
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str155
	.quad	.L.str156
	.quad	flag_volatile_static
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str157
	.quad	.L.str158
	.quad	flag_defer_pop
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str159
	.quad	.L.str160
	.quad	flag_omit_frame_pointer
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str161
	.quad	.L.str162
	.quad	flag_optimize_sibling_calls
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str163
	.quad	.L.str164
	.quad	flag_cse_follow_jumps
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str165
	.quad	.L.str166
	.quad	flag_cse_skip_blocks
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str167
	.quad	.L.str168
	.quad	flag_expensive_optimizations
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str169
	.quad	.L.str170
	.quad	flag_thread_jumps
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str171
	.quad	.L.str172
	.quad	flag_strength_reduce
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str173
	.quad	.L.str174
	.quad	flag_unroll_loops
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str175
	.quad	.L.str176
	.quad	flag_unroll_all_loops
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str177
	.quad	.L.str178
	.quad	flag_prefetch_loop_arrays
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str179
	.quad	.L.str180
	.quad	flag_move_all_movables
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str181
	.quad	.L.str182
	.quad	flag_reduce_all_givs
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str183
	.quad	.L.str184
	.quad	flag_writable_strings
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str185
	.quad	.L.str186
	.quad	flag_no_peephole
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str187
	.quad	.L.str188
	.quad	flag_force_mem
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str189
	.quad	.L.str190
	.quad	flag_force_addr
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str191
	.quad	.L.str192
	.quad	flag_no_function_cse
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str193
	.quad	.L.str194
	.quad	flag_inline_functions
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str195
	.quad	.L.str196
	.quad	flag_keep_inline_functions
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str197
	.quad	.L.str198
	.quad	flag_no_inline
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str199
	.quad	.L.str200
	.quad	flag_keep_static_consts
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str201
	.quad	.L.str202
	.quad	flag_syntax_only
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str203
	.quad	.L.str204
	.quad	flag_shared_data
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str205
	.quad	.L.str206
	.quad	flag_caller_saves
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str207
	.quad	.L.str208
	.quad	flag_pcc_struct_return
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str209
	.quad	.L.str210
	.quad	flag_pcc_struct_return
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str211
	.quad	.L.str212
	.quad	flag_delayed_branch
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str213
	.quad	.L.str214
	.quad	flag_gcse
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str215
	.quad	.L.str216
	.quad	flag_gcse_lm
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str217
	.quad	.L.str218
	.quad	flag_gcse_sm
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str219
	.quad	.L.str220
	.quad	flag_rerun_cse_after_loop
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str221
	.quad	.L.str222
	.quad	flag_rerun_loop_opt
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str223
	.quad	.L.str224
	.quad	flag_delete_null_pointer_checks
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str225
	.quad	.L.str226
	.quad	flag_pretend_float
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str227
	.quad	.L.str228
	.quad	flag_schedule_insns
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str229
	.quad	.L.str230
	.quad	flag_schedule_insns_after_reload
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str231
	.quad	.L.str232
	.quad	flag_schedule_interblock
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str233
	.quad	.L.str234
	.quad	flag_schedule_speculative
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str235
	.quad	.L.str236
	.quad	flag_schedule_speculative_load
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str237
	.quad	.L.str238
	.quad	flag_schedule_speculative_load_dangerous
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str239
	.quad	.L.str240
	.quad	flag_branch_on_count_reg
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str241
	.quad	.L.str242
	.quad	flag_pic
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str243
	.quad	.L.str244
	.quad	flag_pic
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str28
	.quad	.L.str245
	.quad	flag_exceptions
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str246
	.quad	.L.str247
	.quad	flag_unwind_tables
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str248
	.quad	.L.str249
	.quad	flag_asynchronous_unwind_tables
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str250
	.quad	.L.str251
	.quad	flag_non_call_exceptions
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str252
	.quad	.L.str253
	.quad	profile_arc_flag
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str254
	.quad	.L.str255
	.quad	flag_test_coverage
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str256
	.quad	.L.str257
	.quad	flag_branch_probabilities
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str258
	.quad	.L.str259
	.quad	flag_reorder_blocks
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str260
	.quad	.L.str261
	.quad	flag_rename_registers
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str262
	.quad	.L.str263
	.quad	flag_cprop_registers
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str264
	.quad	.L.str265
	.quad	flag_no_common
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str266
	.quad	.L.str267
	.quad	flag_inhibit_size_directive
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str268
	.quad	.L.str269
	.quad	flag_function_sections
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str270
	.quad	.L.str271
	.quad	flag_data_sections
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str272
	.quad	.L.str273
	.quad	flag_verbose_asm
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str274
	.quad	.L.str275
	.quad	flag_gnu_linker
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str276
	.quad	.L.str277
	.quad	flag_regmove
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str278
	.quad	.L.str279
	.quad	flag_regmove
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str280
	.quad	.L.str281
	.quad	flag_pack_struct
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str282
	.quad	.L.str283
	.quad	flag_stack_check
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str284
	.quad	.L.str285
	.quad	flag_argument_noalias
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str286
	.quad	.L.str287
	.quad	flag_argument_noalias
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str288
	.quad	.L.str289
	.quad	flag_argument_noalias
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str290
	.quad	.L.str291
	.quad	flag_strict_aliasing
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str292
	.quad	.L.str293
	.quad	align_loops
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str294
	.quad	.L.str295
	.quad	align_jumps
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str296
	.quad	.L.str297
	.quad	align_labels
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str298
	.quad	.L.str299
	.quad	align_functions
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str300
	.quad	.L.str301
	.quad	flag_merge_constants
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str302
	.quad	.L.str303
	.quad	flag_merge_constants
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str304
	.quad	.L.str305
	.quad	flag_dump_unnumbered
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str306
	.quad	.L.str307
	.quad	flag_instrument_function_entry_exit
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str308
	.quad	.L.str309
	.quad	flag_ssa
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str310
	.quad	.L.str311
	.quad	flag_ssa_ccp
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str312
	.quad	.L.str313
	.quad	flag_ssa_dce
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str314
	.quad	.L.str315
	.quad	flag_leading_underscore
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str316
	.quad	.L.str317
	.quad	flag_no_ident
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str318
	.quad	.L.str319
	.quad	flag_peephole2
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str320
	.quad	.L.str321
	.quad	flag_guess_branch_prob
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str322
	.quad	.L.str323
	.quad	flag_errno_math
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str324
	.quad	.L.str325
	.quad	flag_trapping_math
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str326
	.quad	.L.str327
	.quad	flag_unsafe_math_optimizations
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str328
	.quad	.L.str329
	.quad	flag_bounded_pointers
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str330
	.quad	.L.str331
	.quad	flag_bounds_check
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str332
	.quad	.L.str333
	.quad	flag_single_precision_constant
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str334
	.quad	.L.str335
	.quad	time_report
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str336
	.quad	.L.str337
	.quad	mem_report
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str338
	.quad	.L.str339
	.quad	flag_trapv
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str340
	.size	f_options, 3104

	.type	.L.str34,@object        # @.str34
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str34:
	.asciz	"-f"
	.size	.L.str34, 3

	.type	target_switches,@object # @target_switches
	.section	.rodata,"a",@progbits
	.align	16
target_switches:
	.quad	.L.str61
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str62
	.quad	.L.str63
	.long	4294967295              # 0xffffffff
	.zero	4
	.quad	.L.str64
	.quad	.L.str65
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str62
	.quad	.L.str66
	.long	4294967295              # 0xffffffff
	.zero	4
	.quad	.L.str64
	.quad	.L.str67
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str62
	.quad	.L.str68
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str69
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str70
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str71
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str72
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str73
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str28
	.quad	.L.str74
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str75
	.quad	.L.str76
	.long	4294967294              # 0xfffffffe
	.zero	4
	.quad	.L.str77
	.quad	.L.str78
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str79
	.quad	.L.str80
	.long	4294967292              # 0xfffffffc
	.zero	4
	.quad	.L.str81
	.quad	.L.str82
	.long	8                       # 0x8
	.zero	4
	.quad	.L.str83
	.quad	.L.str84
	.long	4294967288              # 0xfffffff8
	.zero	4
	.quad	.L.str85
	.quad	.L.str86
	.long	16                      # 0x10
	.zero	4
	.quad	.L.str87
	.quad	.L.str88
	.long	4294967280              # 0xfffffff0
	.zero	4
	.quad	.L.str89
	.quad	.L.str90
	.long	32                      # 0x20
	.zero	4
	.quad	.L.str91
	.quad	.L.str92
	.long	4294967264              # 0xffffffe0
	.zero	4
	.quad	.L.str93
	.quad	.L.str94
	.long	64                      # 0x40
	.zero	4
	.quad	.L.str95
	.quad	.L.str96
	.long	4294967232              # 0xffffffc0
	.zero	4
	.quad	.L.str97
	.quad	.L.str98
	.long	128                     # 0x80
	.zero	4
	.quad	.L.str99
	.quad	.L.str100
	.long	4294967168              # 0xffffff80
	.zero	4
	.quad	.L.str28
	.quad	.L.str101
	.long	256                     # 0x100
	.zero	4
	.quad	.L.str102
	.quad	.L.str103
	.long	4294967040              # 0xffffff00
	.zero	4
	.quad	.L.str28
	.quad	.L.str104
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	.L.str105
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	.L.str106
	.long	4294966784              # 0xfffffe00
	.zero	4
	.quad	.L.str107
	.quad	.L.str108
	.long	512                     # 0x200
	.zero	4
	.quad	.L.str109
	.quad	.L.str110
	.long	1024                    # 0x400
	.zero	4
	.quad	.L.str111
	.quad	.L.str112
	.long	4294966272              # 0xfffffc00
	.zero	4
	.quad	.L.str113
	.quad	.L.str114
	.long	4294965248              # 0xfffff800
	.zero	4
	.quad	.L.str115
	.quad	.L.str116
	.long	2048                    # 0x800
	.zero	4
	.quad	.L.str117
	.quad	.L.str118
	.long	12288                   # 0x3000
	.zero	4
	.quad	.L.str115
	.quad	.L.str119
	.long	8192                    # 0x2000
	.zero	4
	.quad	.L.str117
	.quad	.L.str120
	.long	49152                   # 0xc000
	.zero	4
	.quad	.L.str121
	.quad	.L.str122
	.long	4294950912              # 0xffffc000
	.zero	4
	.quad	.L.str123
	.quad	.L.str122
	.long	32768                   # 0x8000
	.zero	4
	.quad	.L.str28
	.quad	.L.str124
	.long	3145728                 # 0x300000
	.zero	4
	.quad	.L.str125
	.quad	.L.str126
	.long	4293918720              # 0xfff00000
	.zero	4
	.quad	.L.str28
	.quad	.L.str126
	.long	2097152                 # 0x200000
	.zero	4
	.quad	.L.str127
	.quad	.L.str128
	.long	196608                  # 0x30000
	.zero	4
	.quad	.L.str129
	.quad	.L.str130
	.long	4294901760              # 0xffff0000
	.zero	4
	.quad	.L.str28
	.quad	.L.str130
	.long	131072                  # 0x20000
	.zero	4
	.quad	.L.str131
	.quad	.L.str132
	.long	786432                  # 0xc0000
	.zero	4
	.quad	.L.str133
	.quad	.L.str134
	.long	4294705152              # 0xfffc0000
	.zero	4
	.quad	.L.str28
	.quad	.L.str134
	.long	524288                  # 0x80000
	.zero	4
	.quad	.L.str135
	.quad	.L.str136
	.long	16777216                # 0x1000000
	.zero	4
	.quad	.L.str137
	.quad	.L.str138
	.long	4278190080              # 0xff000000
	.zero	4
	.quad	.L.str139
	.quad	.L.str140
	.long	33554432                # 0x2000000
	.zero	4
	.quad	.L.str141
	.quad	.L.str142
	.long	4261412864              # 0xfe000000
	.zero	4
	.quad	.L.str143
	.quad	.L.str144
	.long	4227858432              # 0xfc000000
	.zero	4
	.quad	.L.str145
	.quad	.L.str146
	.long	67108864                # 0x4000000
	.zero	4
	.quad	.L.str147
	.quad	.L.str28
	.long	33554481                # 0x2000031
	.zero	4
	.quad	0
	.size	target_switches, 1344

	.type	.L.str35,@object        # @.str35
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str35:
	.asciz	"-m"
	.size	.L.str35, 3

	.type	target_options,@object  # @target_options
	.section	.rodata,"a",@progbits
	.align	16
target_options:
	.quad	.L.str37
	.quad	ix86_cpu_string
	.quad	.L.str38
	.quad	.L.str39
	.quad	ix86_fpmath_string
	.quad	.L.str40
	.quad	.L.str41
	.quad	ix86_arch_string
	.quad	.L.str42
	.quad	.L.str43
	.quad	ix86_regparm_string
	.quad	.L.str44
	.quad	.L.str45
	.quad	ix86_align_loops_string
	.quad	.L.str46
	.quad	.L.str47
	.quad	ix86_align_jumps_string
	.quad	.L.str48
	.quad	.L.str49
	.quad	ix86_align_funcs_string
	.quad	.L.str50
	.quad	.L.str51
	.quad	ix86_preferred_stack_boundary_string
	.quad	.L.str52
	.quad	.L.str53
	.quad	ix86_branch_cost_string
	.quad	.L.str54
	.quad	.L.str55
	.quad	ix86_cmodel_string
	.quad	.L.str56
	.quad	.L.str57
	.quad	ix86_debug_arg_string
	.quad	.L.str28
	.quad	.L.str58
	.quad	ix86_debug_addr_string
	.quad	.L.str28
	.quad	.L.str59
	.quad	ix86_asm_string
	.quad	.L.str60
	.size	target_options, 312

	.type	.L.str36,@object        # @.str36
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str36:
	.asciz	"-m%s"
	.size	.L.str36, 5

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"cpu="
	.size	.L.str37, 5

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Schedule code for given CPU"
	.size	.L.str38, 28

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"fpmath="
	.size	.L.str39, 8

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Generate floating point mathematics using given instruction set"
	.size	.L.str40, 64

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"arch="
	.size	.L.str41, 6

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"Generate code for given CPU"
	.size	.L.str42, 28

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"regparm="
	.size	.L.str43, 9

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Number of registers used to pass integer arguments"
	.size	.L.str44, 51

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"align-loops="
	.size	.L.str45, 13

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"Loop code aligned to this power of 2"
	.size	.L.str46, 37

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"align-jumps="
	.size	.L.str47, 13

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"Jump targets are aligned to this power of 2"
	.size	.L.str48, 44

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"align-functions="
	.size	.L.str49, 17

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"Function starts are aligned to this power of 2"
	.size	.L.str50, 47

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"preferred-stack-boundary="
	.size	.L.str51, 26

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"Attempt to keep stack aligned to this power of 2"
	.size	.L.str52, 49

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"branch-cost="
	.size	.L.str53, 13

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"Branches are this expensive (1-5, arbitrary units)"
	.size	.L.str54, 51

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"cmodel="
	.size	.L.str55, 8

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"Use given x86-64 code model"
	.size	.L.str56, 28

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"debug-arg"
	.size	.L.str57, 10

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"debug-addr"
	.size	.L.str58, 11

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"asm="
	.size	.L.str59, 5

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Use given assembler dialect"
	.size	.L.str60, 28

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"80387"
	.size	.L.str61, 6

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"Use hardware fp"
	.size	.L.str62, 16

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"no-80387"
	.size	.L.str63, 9

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Do not use hardware fp"
	.size	.L.str64, 23

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"hard-float"
	.size	.L.str65, 11

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"soft-float"
	.size	.L.str66, 11

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"no-soft-float"
	.size	.L.str67, 14

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"386"
	.size	.L.str68, 4

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"486"
	.size	.L.str69, 4

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"pentium"
	.size	.L.str70, 8

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"pentiumpro"
	.size	.L.str71, 11

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"intel-syntax"
	.size	.L.str72, 13

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"no-intel-syntax"
	.size	.L.str73, 16

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"rtd"
	.size	.L.str74, 4

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"Alternate calling convention"
	.size	.L.str75, 29

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"no-rtd"
	.size	.L.str76, 7

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"Use normal calling convention"
	.size	.L.str77, 30

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"align-double"
	.size	.L.str78, 13

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"Align some doubles on dword boundary"
	.size	.L.str79, 37

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"no-align-double"
	.size	.L.str80, 16

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"Align doubles on word boundary"
	.size	.L.str81, 31

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"svr3-shlib"
	.size	.L.str82, 11

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"Uninitialized locals in .bss"
	.size	.L.str83, 29

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"no-svr3-shlib"
	.size	.L.str84, 14

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"Uninitialized locals in .data"
	.size	.L.str85, 30

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"ieee-fp"
	.size	.L.str86, 8

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"Use IEEE math for fp comparisons"
	.size	.L.str87, 33

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"no-ieee-fp"
	.size	.L.str88, 11

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"Do not use IEEE math for fp comparisons"
	.size	.L.str89, 40

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"fp-ret-in-387"
	.size	.L.str90, 14

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"Return values of functions in FPU registers"
	.size	.L.str91, 44

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"no-fp-ret-in-387"
	.size	.L.str92, 17

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"Do not return values of functions in FPU registers"
	.size	.L.str93, 51

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"no-fancy-math-387"
	.size	.L.str94, 18

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"Do not generate sin, cos, sqrt for FPU"
	.size	.L.str95, 39

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"fancy-math-387"
	.size	.L.str96, 15

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"Generate sin, cos, sqrt for FPU"
	.size	.L.str97, 32

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"omit-leaf-frame-pointer"
	.size	.L.str98, 24

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"Omit the frame pointer in leaf functions"
	.size	.L.str99, 41

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"no-omit-leaf-frame-pointer"
	.size	.L.str100, 27

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"stack-arg-probe"
	.size	.L.str101, 16

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"Enable stack probing"
	.size	.L.str102, 21

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"no-stack-arg-probe"
	.size	.L.str103, 19

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"windows"
	.size	.L.str104, 8

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"dll"
	.size	.L.str105, 4

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"align-stringops"
	.size	.L.str106, 16

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"Align destination of the string operations"
	.size	.L.str107, 43

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"no-align-stringops"
	.size	.L.str108, 19

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"Do not align destination of the string operations"
	.size	.L.str109, 50

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"inline-all-stringops"
	.size	.L.str110, 21

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"Inline all known string operations"
	.size	.L.str111, 35

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"no-inline-all-stringops"
	.size	.L.str112, 24

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"Do not inline all known string operations"
	.size	.L.str113, 42

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"push-args"
	.size	.L.str114, 10

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"Use push instructions to save outgoing arguments"
	.size	.L.str115, 49

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"no-push-args"
	.size	.L.str116, 13

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"Do not use push instructions to save outgoing arguments"
	.size	.L.str117, 56

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"accumulate-outgoing-args"
	.size	.L.str118, 25

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"no-accumulate-outgoing-args"
	.size	.L.str119, 28

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"mmx"
	.size	.L.str120, 4

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"Support MMX built-in functions"
	.size	.L.str121, 31

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"no-mmx"
	.size	.L.str122, 7

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"Do not support MMX built-in functions"
	.size	.L.str123, 38

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"3dnow"
	.size	.L.str124, 6

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"Support 3DNow! built-in functions"
	.size	.L.str125, 34

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"no-3dnow"
	.size	.L.str126, 9

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"Do not support 3DNow! built-in functions"
	.size	.L.str127, 41

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"sse"
	.size	.L.str128, 4

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"Support MMX and SSE built-in functions and code generation"
	.size	.L.str129, 59

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"no-sse"
	.size	.L.str130, 7

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"Do not support MMX and SSE built-in functions and code generation"
	.size	.L.str131, 66

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"sse2"
	.size	.L.str132, 5

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"Support MMX, SSE and SSE2 built-in functions and code generation"
	.size	.L.str133, 65

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"no-sse2"
	.size	.L.str134, 8

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"Do not support MMX, SSE and SSE2 built-in functions and code generation"
	.size	.L.str135, 72

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"128bit-long-double"
	.size	.L.str136, 19

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"sizeof(long double) is 16"
	.size	.L.str137, 26

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"96bit-long-double"
	.size	.L.str138, 18

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"sizeof(long double) is 12"
	.size	.L.str139, 26

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"64"
	.size	.L.str140, 3

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"Generate 64bit x86-64 code"
	.size	.L.str141, 27

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"32"
	.size	.L.str142, 3

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"Generate 32bit i386 code"
	.size	.L.str143, 25

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"red-zone"
	.size	.L.str144, 9

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"Use red-zone in the x86-64 code"
	.size	.L.str145, 32

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"no-red-zone"
	.size	.L.str146, 12

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"Do not use red-zone in the x86-64 code"
	.size	.L.str147, 39

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"eliminate-dwarf2-dups"
	.size	.L.str148, 22

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"Perform DWARF2 duplicate elimination"
	.size	.L.str149, 37

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"float-store"
	.size	.L.str150, 12

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"Do not store floats in registers"
	.size	.L.str151, 33

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"volatile"
	.size	.L.str152, 9

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"Consider all mem refs through pointers as volatile"
	.size	.L.str153, 51

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"volatile-global"
	.size	.L.str154, 16

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"Consider all mem refs to global data to be volatile"
	.size	.L.str155, 52

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"volatile-static"
	.size	.L.str156, 16

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"Consider all mem refs to static data to be volatile"
	.size	.L.str157, 52

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"defer-pop"
	.size	.L.str158, 10

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"Defer popping functions args from stack until later"
	.size	.L.str159, 52

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"omit-frame-pointer"
	.size	.L.str160, 19

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"When possible do not generate stack frames"
	.size	.L.str161, 43

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"optimize-sibling-calls"
	.size	.L.str162, 23

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"Optimize sibling and tail recursive calls"
	.size	.L.str163, 42

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"cse-follow-jumps"
	.size	.L.str164, 17

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"When running CSE, follow jumps to their targets"
	.size	.L.str165, 48

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"cse-skip-blocks"
	.size	.L.str166, 16

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"When running CSE, follow conditional jumps"
	.size	.L.str167, 43

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"expensive-optimizations"
	.size	.L.str168, 24

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"Perform a number of minor, expensive optimizations"
	.size	.L.str169, 51

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"thread-jumps"
	.size	.L.str170, 13

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"Perform jump threading optimizations"
	.size	.L.str171, 37

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"strength-reduce"
	.size	.L.str172, 16

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"Perform strength reduction optimizations"
	.size	.L.str173, 41

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"unroll-loops"
	.size	.L.str174, 13

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"Perform loop unrolling when iteration count is known"
	.size	.L.str175, 53

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"unroll-all-loops"
	.size	.L.str176, 17

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"Perform loop unrolling for all loops"
	.size	.L.str177, 37

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"prefetch-loop-arrays"
	.size	.L.str178, 21

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"Generate prefetch instructions, if available, for arrays in loops"
	.size	.L.str179, 66

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"move-all-movables"
	.size	.L.str180, 18

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"Force all loop invariant computations out of loops"
	.size	.L.str181, 51

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"reduce-all-givs"
	.size	.L.str182, 16

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"Strength reduce all loop general induction variables"
	.size	.L.str183, 53

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"writable-strings"
	.size	.L.str184, 17

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"Store strings in writable data section"
	.size	.L.str185, 39

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"peephole"
	.size	.L.str186, 9

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"Enable machine specific peephole optimizations"
	.size	.L.str187, 47

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"force-mem"
	.size	.L.str188, 10

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"Copy memory operands into registers before using"
	.size	.L.str189, 49

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"force-addr"
	.size	.L.str190, 11

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"Copy memory address constants into regs before using"
	.size	.L.str191, 53

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"function-cse"
	.size	.L.str192, 13

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"Allow function addresses to be held in registers"
	.size	.L.str193, 49

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"inline-functions"
	.size	.L.str194, 17

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"Integrate simple functions into their callers"
	.size	.L.str195, 46

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"keep-inline-functions"
	.size	.L.str196, 22

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"Generate code for funcs even if they are fully inlined"
	.size	.L.str197, 55

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"inline"
	.size	.L.str198, 7

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"Pay attention to the 'inline' keyword"
	.size	.L.str199, 38

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"keep-static-consts"
	.size	.L.str200, 19

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"Emit static const variables even if they are not used"
	.size	.L.str201, 54

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"syntax-only"
	.size	.L.str202, 12

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"Check for syntax errors, then stop"
	.size	.L.str203, 35

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"shared-data"
	.size	.L.str204, 12

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"Mark data as shared rather than private"
	.size	.L.str205, 40

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"caller-saves"
	.size	.L.str206, 13

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"Enable saving registers around function calls"
	.size	.L.str207, 46

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"pcc-struct-return"
	.size	.L.str208, 18

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"Return 'short' aggregates in memory, not registers"
	.size	.L.str209, 51

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"reg-struct-return"
	.size	.L.str210, 18

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"Return 'short' aggregates in registers"
	.size	.L.str211, 39

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"delayed-branch"
	.size	.L.str212, 15

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"Attempt to fill delay slots of branch instructions"
	.size	.L.str213, 51

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"gcse"
	.size	.L.str214, 5

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"Perform the global common subexpression elimination"
	.size	.L.str215, 52

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"gcse-lm"
	.size	.L.str216, 8

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"Perform enhanced load motion during global subexpression elimination"
	.size	.L.str217, 69

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"gcse-sm"
	.size	.L.str218, 8

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"Perform store motion after global subexpression elimination"
	.size	.L.str219, 60

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"rerun-cse-after-loop"
	.size	.L.str220, 21

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"Run CSE pass after loop optimizations"
	.size	.L.str221, 38

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"rerun-loop-opt"
	.size	.L.str222, 15

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"Run the loop optimizer twice"
	.size	.L.str223, 29

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"delete-null-pointer-checks"
	.size	.L.str224, 27

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"Delete useless null pointer checks"
	.size	.L.str225, 35

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"pretend-float"
	.size	.L.str226, 14

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"Pretend that host and target use the same FP format"
	.size	.L.str227, 52

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"schedule-insns"
	.size	.L.str228, 15

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"Reschedule instructions before register allocation"
	.size	.L.str229, 51

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"schedule-insns2"
	.size	.L.str230, 16

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"Reschedule instructions after register allocation"
	.size	.L.str231, 50

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"sched-interblock"
	.size	.L.str232, 17

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"Enable scheduling across basic blocks"
	.size	.L.str233, 38

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"sched-spec"
	.size	.L.str234, 11

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"Allow speculative motion of non-loads"
	.size	.L.str235, 38

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"sched-spec-load"
	.size	.L.str236, 16

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"Allow speculative motion of some loads"
	.size	.L.str237, 39

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"sched-spec-load-dangerous"
	.size	.L.str238, 26

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"Allow speculative motion of more loads"
	.size	.L.str239, 39

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"branch-count-reg"
	.size	.L.str240, 17

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"Replace add,compare,branch with branch on count reg"
	.size	.L.str241, 52

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"pic"
	.size	.L.str242, 4

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"Generate position independent code, if possible"
	.size	.L.str243, 48

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"PIC"
	.size	.L.str244, 4

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"exceptions"
	.size	.L.str245, 11

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"Enable exception handling"
	.size	.L.str246, 26

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"unwind-tables"
	.size	.L.str247, 14

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"Just generate unwind tables for exception handling"
	.size	.L.str248, 51

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"asynchronous-unwind-tables"
	.size	.L.str249, 27

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"Generate unwind tables exact at each instruction boundary"
	.size	.L.str250, 58

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"non-call-exceptions"
	.size	.L.str251, 20

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"Support synchronous non-call exceptions"
	.size	.L.str252, 40

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"profile-arcs"
	.size	.L.str253, 13

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	"Insert arc based program profiling code"
	.size	.L.str254, 40

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"test-coverage"
	.size	.L.str255, 14

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"Create data files needed by gcov"
	.size	.L.str256, 33

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"branch-probabilities"
	.size	.L.str257, 21

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"Use profiling information for branch probabilities"
	.size	.L.str258, 51

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"reorder-blocks"
	.size	.L.str259, 15

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"Reorder basic blocks to improve code placement"
	.size	.L.str260, 47

	.type	.L.str261,@object       # @.str261
.L.str261:
	.asciz	"rename-registers"
	.size	.L.str261, 17

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"Do the register renaming optimization pass"
	.size	.L.str262, 43

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"cprop-registers"
	.size	.L.str263, 16

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"Do the register copy-propagation optimization pass"
	.size	.L.str264, 51

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"common"
	.size	.L.str265, 7

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"Do not put uninitialized globals in the common section"
	.size	.L.str266, 55

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"inhibit-size-directive"
	.size	.L.str267, 23

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"Do not generate .size directives"
	.size	.L.str268, 33

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"function-sections"
	.size	.L.str269, 18

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"place each function into its own section"
	.size	.L.str270, 41

	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"data-sections"
	.size	.L.str271, 14

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"place data items into their own section"
	.size	.L.str272, 40

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"verbose-asm"
	.size	.L.str273, 12

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"Add extra commentry to assembler output"
	.size	.L.str274, 40

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"gnu-linker"
	.size	.L.str275, 11

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"Output GNU ld formatted global initializers"
	.size	.L.str276, 44

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"regmove"
	.size	.L.str277, 8

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"Enables a register move optimization"
	.size	.L.str278, 37

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"optimize-register-move"
	.size	.L.str279, 23

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"Do the full regmove optimization pass"
	.size	.L.str280, 38

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"pack-struct"
	.size	.L.str281, 12

	.type	.L.str282,@object       # @.str282
.L.str282:
	.asciz	"Pack structure members together without holes"
	.size	.L.str282, 46

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"stack-check"
	.size	.L.str283, 12

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"Insert stack checking code into the program"
	.size	.L.str284, 44

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"argument-alias"
	.size	.L.str285, 15

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"Specify that arguments may alias each other & globals"
	.size	.L.str286, 54

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"argument-noalias"
	.size	.L.str287, 17

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"Assume arguments may alias globals but not each other"
	.size	.L.str288, 54

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"argument-noalias-global"
	.size	.L.str289, 24

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"Assume arguments do not alias each other or globals"
	.size	.L.str290, 52

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"strict-aliasing"
	.size	.L.str291, 16

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"Assume strict aliasing rules apply"
	.size	.L.str292, 35

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"align-loops"
	.size	.L.str293, 12

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"Align the start of loops"
	.size	.L.str294, 25

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"align-jumps"
	.size	.L.str295, 12

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"Align labels which are only reached by jumping"
	.size	.L.str296, 47

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	"align-labels"
	.size	.L.str297, 13

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"Align all labels"
	.size	.L.str298, 17

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"align-functions"
	.size	.L.str299, 16

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"Align the start of functions"
	.size	.L.str300, 29

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"merge-constants"
	.size	.L.str301, 16

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	"Attempt to merge identical constants accross compilation units"
	.size	.L.str302, 63

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	"merge-all-constants"
	.size	.L.str303, 20

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	"Attempt to merge identical constants and constant variables"
	.size	.L.str304, 60

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"dump-unnumbered"
	.size	.L.str305, 16

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"Suppress output of instruction numbers and line number notes in debugging dumps"
	.size	.L.str306, 80

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"instrument-functions"
	.size	.L.str307, 21

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"Instrument function entry/exit with profiling calls"
	.size	.L.str308, 52

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"ssa"
	.size	.L.str309, 4

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	"Enable SSA optimizations"
	.size	.L.str310, 25

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"ssa-ccp"
	.size	.L.str311, 8

	.type	.L.str312,@object       # @.str312
.L.str312:
	.asciz	"Enable SSA conditional constant propagation"
	.size	.L.str312, 44

	.type	.L.str313,@object       # @.str313
.L.str313:
	.asciz	"ssa-dce"
	.size	.L.str313, 8

	.type	.L.str314,@object       # @.str314
.L.str314:
	.asciz	"Enable aggressive SSA dead code elimination"
	.size	.L.str314, 44

	.type	.L.str315,@object       # @.str315
.L.str315:
	.asciz	"leading-underscore"
	.size	.L.str315, 19

	.type	.L.str316,@object       # @.str316
.L.str316:
	.asciz	"External symbols have a leading underscore"
	.size	.L.str316, 43

	.type	.L.str317,@object       # @.str317
.L.str317:
	.asciz	"ident"
	.size	.L.str317, 6

	.type	.L.str318,@object       # @.str318
.L.str318:
	.asciz	"Process #ident directives"
	.size	.L.str318, 26

	.type	.L.str319,@object       # @.str319
.L.str319:
	.asciz	"peephole2"
	.size	.L.str319, 10

	.type	.L.str320,@object       # @.str320
.L.str320:
	.asciz	"Enables an rtl peephole pass run before sched2"
	.size	.L.str320, 47

	.type	.L.str321,@object       # @.str321
.L.str321:
	.asciz	"guess-branch-probability"
	.size	.L.str321, 25

	.type	.L.str322,@object       # @.str322
.L.str322:
	.asciz	"Enables guessing of branch probabilities"
	.size	.L.str322, 41

	.type	.L.str323,@object       # @.str323
.L.str323:
	.asciz	"math-errno"
	.size	.L.str323, 11

	.type	.L.str324,@object       # @.str324
.L.str324:
	.asciz	"Set errno after built-in math functions"
	.size	.L.str324, 40

	.type	.L.str325,@object       # @.str325
.L.str325:
	.asciz	"trapping-math"
	.size	.L.str325, 14

	.type	.L.str326,@object       # @.str326
.L.str326:
	.asciz	"Floating-point operations can trap"
	.size	.L.str326, 35

	.type	.L.str327,@object       # @.str327
.L.str327:
	.asciz	"unsafe-math-optimizations"
	.size	.L.str327, 26

	.type	.L.str328,@object       # @.str328
.L.str328:
	.asciz	"Allow math optimizations that may violate IEEE or ANSI standards"
	.size	.L.str328, 65

	.type	.L.str329,@object       # @.str329
.L.str329:
	.asciz	"bounded-pointers"
	.size	.L.str329, 17

	.type	.L.str330,@object       # @.str330
.L.str330:
	.asciz	"Compile pointers as triples: value, base & end"
	.size	.L.str330, 47

	.type	.L.str331,@object       # @.str331
.L.str331:
	.asciz	"bounds-check"
	.size	.L.str331, 13

	.type	.L.str332,@object       # @.str332
.L.str332:
	.asciz	"Generate code to check bounds before dereferencing pointers and arrays"
	.size	.L.str332, 71

	.type	.L.str333,@object       # @.str333
.L.str333:
	.asciz	"single-precision-constant"
	.size	.L.str333, 26

	.type	.L.str334,@object       # @.str334
.L.str334:
	.asciz	"Convert floating point constant to single precision constant"
	.size	.L.str334, 61

	.type	.L.str335,@object       # @.str335
.L.str335:
	.asciz	"time-report"
	.size	.L.str335, 12

	.type	.L.str336,@object       # @.str336
.L.str336:
	.asciz	"Report time taken by each compiler pass at end of run"
	.size	.L.str336, 54

	.type	.L.str337,@object       # @.str337
.L.str337:
	.asciz	"mem-report"
	.size	.L.str337, 11

	.type	.L.str338,@object       # @.str338
.L.str338:
	.asciz	"Report on permanent memory allocation at end of run"
	.size	.L.str338, 52

	.type	.L.str339,@object       # @.str339
.L.str339:
	.asciz	"trapv"
	.size	.L.str339, 6

	.type	.L.str340,@object       # @.str340
.L.str340:
	.asciz	"Trap for signed overflow in addition / subtraction / multiplication"
	.size	.L.str340, 68

	.type	.L.str341,@object       # @.str341
.L.str341:
	.asciz	"%s%s%s"
	.size	.L.str341, 7

	.type	.L.str342,@object       # @.str342
.L.str342:
	.asciz	"%s%s%s version %s (%s)\n%s\tcompiled by GNU C version %s.\n"
	.size	.L.str342, 57

	.type	.L.str343,@object       # @.str343
.L.str343:
	.asciz	"x86-64"
	.size	.L.str343, 7

	.type	.L.str344,@object       # @.str344
.L.str344:
	.asciz	"4.2.1 Compatible Clang 3.5 (trunk)"
	.size	.L.str344, 35

	.type	.L.str345,@object       # @.str345
.L.str345:
	.asciz	"this target machine does not have delayed branches"
	.size	.L.str345, 51

	.type	.L.str346,@object       # @.str346
.L.str346:
	.asciz	"profiling does not work without a frame pointer"
	.size	.L.str346, 48

	.type	.L.str347,@object       # @.str347
.L.str347:
	.asciz	"_"
	.size	.L.str347, 2

	.type	version_flag,@object    # @version_flag
	.local	version_flag
	.comm	version_flag,1,1
	.type	.L.str350,@object       # @.str350
.L.str350:
	.asciz	"can't open %s"
	.size	.L.str350, 14

	.type	.L.str351,@object       # @.str351
.L.str351:
	.asciz	"-ffunction-sections not supported for this target"
	.size	.L.str351, 50

	.type	.L.str352,@object       # @.str352
.L.str352:
	.asciz	"-fdata-sections not supported for this target"
	.size	.L.str352, 46

	.type	.L.str353,@object       # @.str353
.L.str353:
	.asciz	"-ffunction-sections disabled; it makes profiling impossible"
	.size	.L.str353, 60

	.type	.L.str354,@object       # @.str354
.L.str354:
	.asciz	"-fprefetch-loop-arrays not supported for this target (try -march switches)"
	.size	.L.str354, 75

	.type	.L.str355,@object       # @.str355
.L.str355:
	.asciz	"-fprefetch-loop-arrays is not supported with -Os"
	.size	.L.str355, 49

	.type	lang_independent_params,@object # @lang_independent_params
	.section	.rodata,"a",@progbits
	.align	16
lang_independent_params:
	.quad	.L.str597
	.long	5500                    # 0x157c
	.zero	4
	.quad	.L.str653
	.quad	.L.str654
	.long	100                     # 0x64
	.zero	4
	.quad	.L.str655
	.quad	.L.str656
	.long	333                     # 0x14d
	.zero	4
	.quad	.L.str657
	.quad	.L.str658
	.long	32                      # 0x20
	.zero	4
	.quad	.L.str659
	.quad	.L.str660
	.long	314572800               # 0x12c00000
	.zero	4
	.quad	.L.str661
	.quad	.L.str662
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str663
	.zero	24
	.size	lang_independent_params, 168

	.type	.L.str356,@object       # @.str356
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str356:
	.asciz	"-O"
	.size	.L.str356, 3

	.type	documented_lang_options,@object # @documented_lang_options
	.section	.rodata,"a",@progbits
	.align	16
documented_lang_options:
	.quad	.L.str362
	.quad	.L.str363
	.quad	.L.str364
	.quad	.L.str365
	.quad	.L.str366
	.quad	.L.str367
	.quad	.L.str368
	.quad	.L.str28
	.quad	.L.str369
	.quad	.L.str370
	.quad	.L.str371
	.quad	.L.str28
	.quad	.L.str372
	.quad	.L.str28
	.quad	.L.str373
	.quad	.L.str374
	.quad	.L.str375
	.quad	.L.str376
	.quad	.L.str377
	.quad	.L.str28
	.quad	.L.str378
	.quad	.L.str28
	.quad	.L.str379
	.quad	.L.str28
	.quad	.L.str380
	.quad	.L.str381
	.quad	.L.str382
	.quad	.L.str28
	.quad	.L.str383
	.quad	.L.str28
	.quad	.L.str384
	.quad	.L.str28
	.quad	.L.str385
	.quad	.L.str386
	.quad	.L.str387
	.quad	.L.str28
	.quad	.L.str388
	.quad	.L.str389
	.quad	.L.str390
	.quad	.L.str391
	.quad	.L.str392
	.quad	.L.str28
	.quad	.L.str393
	.quad	.L.str394
	.quad	.L.str395
	.quad	.L.str28
	.quad	.L.str396
	.quad	.L.str397
	.quad	.L.str398
	.quad	.L.str28
	.quad	.L.str399
	.quad	.L.str400
	.quad	.L.str401
	.quad	.L.str28
	.quad	.L.str402
	.quad	.L.str28
	.quad	.L.str403
	.quad	.L.str28
	.quad	.L.str404
	.quad	.L.str405
	.quad	.L.str406
	.quad	.L.str28
	.quad	.L.str407
	.quad	.L.str408
	.quad	.L.str409
	.quad	.L.str28
	.quad	.L.str410
	.quad	.L.str411
	.quad	.L.str412
	.quad	.L.str28
	.quad	.L.str413
	.quad	.L.str414
	.quad	.L.str415
	.quad	.L.str416
	.quad	.L.str417
	.quad	.L.str28
	.quad	.L.str418
	.quad	.L.str28
	.quad	.L.str419
	.quad	.L.str420
	.quad	.L.str421
	.quad	.L.str28
	.quad	.L.str422
	.quad	.L.str423
	.quad	.L.str424
	.quad	.L.str28
	.quad	.L.str425
	.quad	.L.str426
	.quad	.L.str427
	.quad	.L.str28
	.quad	.L.str428
	.quad	.L.str429
	.quad	.L.str430
	.quad	.L.str28
	.quad	.L.str431
	.quad	.L.str429
	.quad	.L.str432
	.quad	.L.str28
	.quad	.L.str433
	.quad	.L.str434
	.quad	.L.str435
	.quad	.L.str28
	.quad	.L.str436
	.quad	.L.str437
	.quad	.L.str438
	.quad	.L.str28
	.quad	.L.str439
	.quad	.L.str28
	.quad	.L.str440
	.quad	.L.str441
	.quad	.L.str442
	.quad	.L.str28
	.quad	.L.str443
	.quad	.L.str444
	.quad	.L.str445
	.quad	.L.str446
	.quad	.L.str447
	.quad	.L.str28
	.quad	.L.str448
	.quad	.L.str449
	.quad	.L.str450
	.quad	.L.str28
	.quad	.L.str451
	.quad	.L.str452
	.quad	.L.str453
	.quad	.L.str28
	.quad	.L.str454
	.quad	.L.str28
	.quad	.L.str455
	.quad	.L.str456
	.quad	.L.str457
	.quad	.L.str28
	.quad	.L.str458
	.quad	.L.str28
	.quad	.L.str459
	.quad	.L.str28
	.quad	.L.str460
	.quad	.L.str461
	.quad	.L.str462
	.quad	.L.str28
	.quad	.L.str463
	.quad	.L.str28
	.quad	.L.str464
	.quad	.L.str465
	.quad	.L.str466
	.quad	.L.str467
	.quad	.L.str468
	.quad	.L.str28
	.quad	.L.str469
	.quad	.L.str470
	.quad	.L.str471
	.quad	.L.str28
	.quad	.L.str472
	.quad	.L.str473
	.quad	.L.str474
	.quad	.L.str28
	.quad	.L.str475
	.quad	.L.str476
	.quad	.L.str477
	.quad	.L.str28
	.quad	.L.str478
	.quad	.L.str479
	.quad	.L.str480
	.quad	.L.str28
	.quad	.L.str481
	.quad	.L.str482
	.quad	.L.str483
	.quad	.L.str28
	.quad	.L.str484
	.quad	.L.str485
	.quad	.L.str486
	.quad	.L.str28
	.quad	.L.str487
	.quad	.L.str488
	.quad	.L.str489
	.quad	.L.str28
	.quad	.L.str490
	.quad	.L.str491
	.quad	.L.str492
	.quad	.L.str28
	.quad	.L.str493
	.quad	.L.str494
	.quad	.L.str495
	.quad	.L.str28
	.quad	.L.str496
	.quad	.L.str497
	.quad	.L.str498
	.quad	.L.str28
	.quad	.L.str499
	.quad	.L.str500
	.quad	.L.str501
	.quad	.L.str28
	.quad	.L.str502
	.quad	.L.str503
	.quad	.L.str504
	.quad	.L.str28
	.quad	.L.str505
	.quad	.L.str506
	.quad	.L.str507
	.quad	.L.str28
	.quad	.L.str508
	.quad	.L.str509
	.quad	.L.str510
	.quad	.L.str28
	.quad	.L.str511
	.quad	.L.str512
	.quad	.L.str513
	.quad	.L.str28
	.quad	.L.str514
	.quad	.L.str28
	.quad	.L.str515
	.quad	.L.str28
	.quad	.L.str516
	.quad	.L.str517
	.quad	.L.str518
	.quad	.L.str28
	.size	documented_lang_options, 1728

	.type	.L.str357,@object       # @.str357
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str357:
	.asciz	"ignoring command line option '%s'"
	.size	.L.str357, 34

	.type	.L.str359,@object       # @.str359
.L.str359:
	.asciz	"`%s': unknown or unsupported -g option"
	.size	.L.str359, 39

	.type	.L.str360,@object       # @.str360
.L.str360:
	.asciz	"unrecognized option `%s'"
	.size	.L.str360, 25

	.type	.L.str361,@object       # @.str361
.L.str361:
	.asciz	"-Wuninitialized is not supported without -O"
	.size	.L.str361, 44

	.type	.L.str362,@object       # @.str362
.L.str362:
	.asciz	"-ansi"
	.size	.L.str362, 6

	.type	.L.str363,@object       # @.str363
.L.str363:
	.asciz	"Compile just for ISO C89"
	.size	.L.str363, 25

	.type	.L.str364,@object       # @.str364
.L.str364:
	.asciz	"-fallow-single-precision"
	.size	.L.str364, 25

	.type	.L.str365,@object       # @.str365
.L.str365:
	.asciz	"Do not promote floats to double if using -traditional"
	.size	.L.str365, 54

	.type	.L.str366,@object       # @.str366
.L.str366:
	.asciz	"-std= "
	.size	.L.str366, 7

	.type	.L.str367,@object       # @.str367
.L.str367:
	.asciz	"Determine language standard"
	.size	.L.str367, 28

	.type	.L.str368,@object       # @.str368
.L.str368:
	.asciz	"-fsigned-bitfields"
	.size	.L.str368, 19

	.type	.L.str369,@object       # @.str369
.L.str369:
	.asciz	"-funsigned-bitfields"
	.size	.L.str369, 21

	.type	.L.str370,@object       # @.str370
.L.str370:
	.asciz	"Make bit-fields by unsigned by default"
	.size	.L.str370, 39

	.type	.L.str371,@object       # @.str371
.L.str371:
	.asciz	"-fno-signed-bitfields"
	.size	.L.str371, 22

	.type	.L.str372,@object       # @.str372
.L.str372:
	.asciz	"-fno-unsigned-bitfields"
	.size	.L.str372, 24

	.type	.L.str373,@object       # @.str373
.L.str373:
	.asciz	"-fsigned-char"
	.size	.L.str373, 14

	.type	.L.str374,@object       # @.str374
.L.str374:
	.asciz	"Make 'char' be signed by default"
	.size	.L.str374, 33

	.type	.L.str375,@object       # @.str375
.L.str375:
	.asciz	"-funsigned-char"
	.size	.L.str375, 16

	.type	.L.str376,@object       # @.str376
.L.str376:
	.asciz	"Make 'char' be unsigned by default"
	.size	.L.str376, 35

	.type	.L.str377,@object       # @.str377
.L.str377:
	.asciz	"-fno-signed-char"
	.size	.L.str377, 17

	.type	.L.str378,@object       # @.str378
.L.str378:
	.asciz	"-fno-unsigned-char"
	.size	.L.str378, 19

	.type	.L.str379,@object       # @.str379
.L.str379:
	.asciz	"-ftraditional"
	.size	.L.str379, 14

	.type	.L.str380,@object       # @.str380
.L.str380:
	.asciz	"-traditional"
	.size	.L.str380, 13

	.type	.L.str381,@object       # @.str381
.L.str381:
	.asciz	"Attempt to support traditional K&R style C"
	.size	.L.str381, 43

	.type	.L.str382,@object       # @.str382
.L.str382:
	.asciz	"-fnotraditional"
	.size	.L.str382, 16

	.type	.L.str383,@object       # @.str383
.L.str383:
	.asciz	"-fno-traditional"
	.size	.L.str383, 17

	.type	.L.str384,@object       # @.str384
.L.str384:
	.asciz	"-fasm"
	.size	.L.str384, 6

	.type	.L.str385,@object       # @.str385
.L.str385:
	.asciz	"-fno-asm"
	.size	.L.str385, 9

	.type	.L.str386,@object       # @.str386
.L.str386:
	.asciz	"Do not recognize the 'asm' keyword"
	.size	.L.str386, 35

	.type	.L.str387,@object       # @.str387
.L.str387:
	.asciz	"-fbuiltin"
	.size	.L.str387, 10

	.type	.L.str388,@object       # @.str388
.L.str388:
	.asciz	"-fno-builtin"
	.size	.L.str388, 13

	.type	.L.str389,@object       # @.str389
.L.str389:
	.asciz	"Do not recognize any built in functions"
	.size	.L.str389, 40

	.type	.L.str390,@object       # @.str390
.L.str390:
	.asciz	"-fhosted"
	.size	.L.str390, 9

	.type	.L.str391,@object       # @.str391
.L.str391:
	.asciz	"Assume normal C execution environment"
	.size	.L.str391, 38

	.type	.L.str392,@object       # @.str392
.L.str392:
	.asciz	"-fno-hosted"
	.size	.L.str392, 12

	.type	.L.str393,@object       # @.str393
.L.str393:
	.asciz	"-ffreestanding"
	.size	.L.str393, 15

	.type	.L.str394,@object       # @.str394
.L.str394:
	.asciz	"Assume that standard libraries & main might not exist"
	.size	.L.str394, 54

	.type	.L.str395,@object       # @.str395
.L.str395:
	.asciz	"-fno-freestanding"
	.size	.L.str395, 18

	.type	.L.str396,@object       # @.str396
.L.str396:
	.asciz	"-fcond-mismatch"
	.size	.L.str396, 16

	.type	.L.str397,@object       # @.str397
.L.str397:
	.asciz	"Allow different types as args of ? operator"
	.size	.L.str397, 44

	.type	.L.str398,@object       # @.str398
.L.str398:
	.asciz	"-fno-cond-mismatch"
	.size	.L.str398, 19

	.type	.L.str399,@object       # @.str399
.L.str399:
	.asciz	"-fdollars-in-identifiers"
	.size	.L.str399, 25

	.type	.L.str400,@object       # @.str400
.L.str400:
	.asciz	"Allow the use of $ inside identifiers"
	.size	.L.str400, 38

	.type	.L.str401,@object       # @.str401
.L.str401:
	.asciz	"-fno-dollars-in-identifiers"
	.size	.L.str401, 28

	.type	.L.str402,@object       # @.str402
.L.str402:
	.asciz	"-fpreprocessed"
	.size	.L.str402, 15

	.type	.L.str403,@object       # @.str403
.L.str403:
	.asciz	"-fno-preprocessed"
	.size	.L.str403, 18

	.type	.L.str404,@object       # @.str404
.L.str404:
	.asciz	"-fshort-double"
	.size	.L.str404, 15

	.type	.L.str405,@object       # @.str405
.L.str405:
	.asciz	"Use the same size for double as for float"
	.size	.L.str405, 42

	.type	.L.str406,@object       # @.str406
.L.str406:
	.asciz	"-fno-short-double"
	.size	.L.str406, 18

	.type	.L.str407,@object       # @.str407
.L.str407:
	.asciz	"-fshort-enums"
	.size	.L.str407, 14

	.type	.L.str408,@object       # @.str408
.L.str408:
	.asciz	"Use the smallest fitting integer to hold enums"
	.size	.L.str408, 47

	.type	.L.str409,@object       # @.str409
.L.str409:
	.asciz	"-fno-short-enums"
	.size	.L.str409, 17

	.type	.L.str410,@object       # @.str410
.L.str410:
	.asciz	"-fshort-wchar"
	.size	.L.str410, 14

	.type	.L.str411,@object       # @.str411
.L.str411:
	.asciz	"Override the underlying type for wchar_t to `unsigned short'"
	.size	.L.str411, 61

	.type	.L.str412,@object       # @.str412
.L.str412:
	.asciz	"-fno-short-wchar"
	.size	.L.str412, 17

	.type	.L.str413,@object       # @.str413
.L.str413:
	.asciz	"-Wall"
	.size	.L.str413, 6

	.type	.L.str414,@object       # @.str414
.L.str414:
	.asciz	"Enable most warning messages"
	.size	.L.str414, 29

	.type	.L.str415,@object       # @.str415
.L.str415:
	.asciz	"-Wbad-function-cast"
	.size	.L.str415, 20

	.type	.L.str416,@object       # @.str416
.L.str416:
	.asciz	"Warn about casting functions to incompatible types"
	.size	.L.str416, 51

	.type	.L.str417,@object       # @.str417
.L.str417:
	.asciz	"-Wno-bad-function-cast"
	.size	.L.str417, 23

	.type	.L.str418,@object       # @.str418
.L.str418:
	.asciz	"-Wno-missing-noreturn"
	.size	.L.str418, 22

	.type	.L.str419,@object       # @.str419
.L.str419:
	.asciz	"-Wmissing-format-attribute"
	.size	.L.str419, 27

	.type	.L.str420,@object       # @.str420
.L.str420:
	.asciz	"Warn about functions which might be candidates for format attributes"
	.size	.L.str420, 69

	.type	.L.str421,@object       # @.str421
.L.str421:
	.asciz	"-Wno-missing-format-attribute"
	.size	.L.str421, 30

	.type	.L.str422,@object       # @.str422
.L.str422:
	.asciz	"-Wcast-qual"
	.size	.L.str422, 12

	.type	.L.str423,@object       # @.str423
.L.str423:
	.asciz	"Warn about casts which discard qualifiers"
	.size	.L.str423, 42

	.type	.L.str424,@object       # @.str424
.L.str424:
	.asciz	"-Wno-cast-qual"
	.size	.L.str424, 15

	.type	.L.str425,@object       # @.str425
.L.str425:
	.asciz	"-Wchar-subscripts"
	.size	.L.str425, 18

	.type	.L.str426,@object       # @.str426
.L.str426:
	.asciz	"Warn about subscripts whose type is 'char'"
	.size	.L.str426, 43

	.type	.L.str427,@object       # @.str427
.L.str427:
	.asciz	"-Wno-char-subscripts"
	.size	.L.str427, 21

	.type	.L.str428,@object       # @.str428
.L.str428:
	.asciz	"-Wcomment"
	.size	.L.str428, 10

	.type	.L.str429,@object       # @.str429
.L.str429:
	.asciz	"Warn if nested comments are detected"
	.size	.L.str429, 37

	.type	.L.str430,@object       # @.str430
.L.str430:
	.asciz	"-Wno-comment"
	.size	.L.str430, 13

	.type	.L.str431,@object       # @.str431
.L.str431:
	.asciz	"-Wcomments"
	.size	.L.str431, 11

	.type	.L.str432,@object       # @.str432
.L.str432:
	.asciz	"-Wno-comments"
	.size	.L.str432, 14

	.type	.L.str433,@object       # @.str433
.L.str433:
	.asciz	"-Wconversion"
	.size	.L.str433, 13

	.type	.L.str434,@object       # @.str434
.L.str434:
	.asciz	"Warn about possibly confusing type conversions"
	.size	.L.str434, 47

	.type	.L.str435,@object       # @.str435
.L.str435:
	.asciz	"-Wno-conversion"
	.size	.L.str435, 16

	.type	.L.str436,@object       # @.str436
.L.str436:
	.asciz	"-Wformat"
	.size	.L.str436, 9

	.type	.L.str437,@object       # @.str437
.L.str437:
	.asciz	"Warn about printf/scanf/strftime/strfmon format anomalies"
	.size	.L.str437, 58

	.type	.L.str438,@object       # @.str438
.L.str438:
	.asciz	"-Wno-format"
	.size	.L.str438, 12

	.type	.L.str439,@object       # @.str439
.L.str439:
	.asciz	"-Wformat-y2k"
	.size	.L.str439, 13

	.type	.L.str440,@object       # @.str440
.L.str440:
	.asciz	"-Wno-format-y2k"
	.size	.L.str440, 16

	.type	.L.str441,@object       # @.str441
.L.str441:
	.asciz	"Don't warn about strftime formats yielding 2 digit years"
	.size	.L.str441, 57

	.type	.L.str442,@object       # @.str442
.L.str442:
	.asciz	"-Wformat-extra-args"
	.size	.L.str442, 20

	.type	.L.str443,@object       # @.str443
.L.str443:
	.asciz	"-Wno-format-extra-args"
	.size	.L.str443, 23

	.type	.L.str444,@object       # @.str444
.L.str444:
	.asciz	"Don't warn about too many arguments to format functions"
	.size	.L.str444, 56

	.type	.L.str445,@object       # @.str445
.L.str445:
	.asciz	"-Wformat-nonliteral"
	.size	.L.str445, 20

	.type	.L.str446,@object       # @.str446
.L.str446:
	.asciz	"Warn about non-string-literal format strings"
	.size	.L.str446, 45

	.type	.L.str447,@object       # @.str447
.L.str447:
	.asciz	"-Wno-format-nonliteral"
	.size	.L.str447, 23

	.type	.L.str448,@object       # @.str448
.L.str448:
	.asciz	"-Wformat-security"
	.size	.L.str448, 18

	.type	.L.str449,@object       # @.str449
.L.str449:
	.asciz	"Warn about possible security problems with format functions"
	.size	.L.str449, 60

	.type	.L.str450,@object       # @.str450
.L.str450:
	.asciz	"-Wno-format-security"
	.size	.L.str450, 21

	.type	.L.str451,@object       # @.str451
.L.str451:
	.asciz	"-Wimplicit-function-declaration"
	.size	.L.str451, 32

	.type	.L.str452,@object       # @.str452
.L.str452:
	.asciz	"Warn about implicit function declarations"
	.size	.L.str452, 42

	.type	.L.str453,@object       # @.str453
.L.str453:
	.asciz	"-Wno-implicit-function-declaration"
	.size	.L.str453, 35

	.type	.L.str454,@object       # @.str454
.L.str454:
	.asciz	"-Werror-implicit-function-declaration"
	.size	.L.str454, 38

	.type	.L.str455,@object       # @.str455
.L.str455:
	.asciz	"-Wimplicit-int"
	.size	.L.str455, 15

	.type	.L.str456,@object       # @.str456
.L.str456:
	.asciz	"Warn when a declaration does not specify a type"
	.size	.L.str456, 48

	.type	.L.str457,@object       # @.str457
.L.str457:
	.asciz	"-Wno-implicit-int"
	.size	.L.str457, 18

	.type	.L.str458,@object       # @.str458
.L.str458:
	.asciz	"-Wimplicit"
	.size	.L.str458, 11

	.type	.L.str459,@object       # @.str459
.L.str459:
	.asciz	"-Wno-implicit"
	.size	.L.str459, 14

	.type	.L.str460,@object       # @.str460
.L.str460:
	.asciz	"-Wimport"
	.size	.L.str460, 9

	.type	.L.str461,@object       # @.str461
.L.str461:
	.asciz	"Warn about the use of the #import directive"
	.size	.L.str461, 44

	.type	.L.str462,@object       # @.str462
.L.str462:
	.asciz	"-Wno-import"
	.size	.L.str462, 12

	.type	.L.str463,@object       # @.str463
.L.str463:
	.asciz	"-Wlong-long"
	.size	.L.str463, 12

	.type	.L.str464,@object       # @.str464
.L.str464:
	.asciz	"-Wno-long-long"
	.size	.L.str464, 15

	.type	.L.str465,@object       # @.str465
.L.str465:
	.asciz	"Do not warn about using 'long long' when -pedantic"
	.size	.L.str465, 51

	.type	.L.str466,@object       # @.str466
.L.str466:
	.asciz	"-Wmain"
	.size	.L.str466, 7

	.type	.L.str467,@object       # @.str467
.L.str467:
	.asciz	"Warn about suspicious declarations of main"
	.size	.L.str467, 43

	.type	.L.str468,@object       # @.str468
.L.str468:
	.asciz	"-Wno-main"
	.size	.L.str468, 10

	.type	.L.str469,@object       # @.str469
.L.str469:
	.asciz	"-Wmissing-braces"
	.size	.L.str469, 17

	.type	.L.str470,@object       # @.str470
.L.str470:
	.asciz	"Warn about possibly missing braces around initializers"
	.size	.L.str470, 55

	.type	.L.str471,@object       # @.str471
.L.str471:
	.asciz	"-Wno-missing-braces"
	.size	.L.str471, 20

	.type	.L.str472,@object       # @.str472
.L.str472:
	.asciz	"-Wmissing-declarations"
	.size	.L.str472, 23

	.type	.L.str473,@object       # @.str473
.L.str473:
	.asciz	"Warn about global funcs without previous declarations"
	.size	.L.str473, 54

	.type	.L.str474,@object       # @.str474
.L.str474:
	.asciz	"-Wno-missing-declarations"
	.size	.L.str474, 26

	.type	.L.str475,@object       # @.str475
.L.str475:
	.asciz	"-Wmissing-prototypes"
	.size	.L.str475, 21

	.type	.L.str476,@object       # @.str476
.L.str476:
	.asciz	"Warn about global funcs without prototypes"
	.size	.L.str476, 43

	.type	.L.str477,@object       # @.str477
.L.str477:
	.asciz	"-Wno-missing-prototypes"
	.size	.L.str477, 24

	.type	.L.str478,@object       # @.str478
.L.str478:
	.asciz	"-Wmultichar"
	.size	.L.str478, 12

	.type	.L.str479,@object       # @.str479
.L.str479:
	.asciz	"Warn about use of multicharacter literals"
	.size	.L.str479, 42

	.type	.L.str480,@object       # @.str480
.L.str480:
	.asciz	"-Wno-multichar"
	.size	.L.str480, 15

	.type	.L.str481,@object       # @.str481
.L.str481:
	.asciz	"-Wnested-externs"
	.size	.L.str481, 17

	.type	.L.str482,@object       # @.str482
.L.str482:
	.asciz	"Warn about externs not at file scope level"
	.size	.L.str482, 43

	.type	.L.str483,@object       # @.str483
.L.str483:
	.asciz	"-Wno-nested-externs"
	.size	.L.str483, 20

	.type	.L.str484,@object       # @.str484
.L.str484:
	.asciz	"-Wparentheses"
	.size	.L.str484, 14

	.type	.L.str485,@object       # @.str485
.L.str485:
	.asciz	"Warn about possible missing parentheses"
	.size	.L.str485, 40

	.type	.L.str486,@object       # @.str486
.L.str486:
	.asciz	"-Wno-parentheses"
	.size	.L.str486, 17

	.type	.L.str487,@object       # @.str487
.L.str487:
	.asciz	"-Wsequence-point"
	.size	.L.str487, 17

	.type	.L.str488,@object       # @.str488
.L.str488:
	.asciz	"Warn about possible violations of sequence point rules"
	.size	.L.str488, 55

	.type	.L.str489,@object       # @.str489
.L.str489:
	.asciz	"-Wno-sequence-point"
	.size	.L.str489, 20

	.type	.L.str490,@object       # @.str490
.L.str490:
	.asciz	"-Wpointer-arith"
	.size	.L.str490, 16

	.type	.L.str491,@object       # @.str491
.L.str491:
	.asciz	"Warn about function pointer arithmetic"
	.size	.L.str491, 39

	.type	.L.str492,@object       # @.str492
.L.str492:
	.asciz	"-Wno-pointer-arith"
	.size	.L.str492, 19

	.type	.L.str493,@object       # @.str493
.L.str493:
	.asciz	"-Wredundant-decls"
	.size	.L.str493, 18

	.type	.L.str494,@object       # @.str494
.L.str494:
	.asciz	"Warn about multiple declarations of the same object"
	.size	.L.str494, 52

	.type	.L.str495,@object       # @.str495
.L.str495:
	.asciz	"-Wno-redundant-decls"
	.size	.L.str495, 21

	.type	.L.str496,@object       # @.str496
.L.str496:
	.asciz	"-Wsign-compare"
	.size	.L.str496, 15

	.type	.L.str497,@object       # @.str497
.L.str497:
	.asciz	"Warn about signed/unsigned comparisons"
	.size	.L.str497, 39

	.type	.L.str498,@object       # @.str498
.L.str498:
	.asciz	"-Wno-sign-compare"
	.size	.L.str498, 18

	.type	.L.str499,@object       # @.str499
.L.str499:
	.asciz	"-Wfloat-equal"
	.size	.L.str499, 14

	.type	.L.str500,@object       # @.str500
.L.str500:
	.asciz	"Warn about testing equality of floating point numbers"
	.size	.L.str500, 54

	.type	.L.str501,@object       # @.str501
.L.str501:
	.asciz	"-Wno-float-equal"
	.size	.L.str501, 17

	.type	.L.str502,@object       # @.str502
.L.str502:
	.asciz	"-Wunknown-pragmas"
	.size	.L.str502, 18

	.type	.L.str503,@object       # @.str503
.L.str503:
	.asciz	"Warn about unrecognized pragmas"
	.size	.L.str503, 32

	.type	.L.str504,@object       # @.str504
.L.str504:
	.asciz	"-Wno-unknown-pragmas"
	.size	.L.str504, 21

	.type	.L.str505,@object       # @.str505
.L.str505:
	.asciz	"-Wstrict-prototypes"
	.size	.L.str505, 20

	.type	.L.str506,@object       # @.str506
.L.str506:
	.asciz	"Warn about non-prototyped function decls"
	.size	.L.str506, 41

	.type	.L.str507,@object       # @.str507
.L.str507:
	.asciz	"-Wno-strict-prototypes"
	.size	.L.str507, 23

	.type	.L.str508,@object       # @.str508
.L.str508:
	.asciz	"-Wtraditional"
	.size	.L.str508, 14

	.type	.L.str509,@object       # @.str509
.L.str509:
	.asciz	"Warn about constructs whose meaning change in ISO C"
	.size	.L.str509, 52

	.type	.L.str510,@object       # @.str510
.L.str510:
	.asciz	"-Wno-traditional"
	.size	.L.str510, 17

	.type	.L.str511,@object       # @.str511
.L.str511:
	.asciz	"-Wtrigraphs"
	.size	.L.str511, 12

	.type	.L.str512,@object       # @.str512
.L.str512:
	.asciz	"Warn when trigraphs are encountered"
	.size	.L.str512, 36

	.type	.L.str513,@object       # @.str513
.L.str513:
	.asciz	"-Wno-trigraphs"
	.size	.L.str513, 15

	.type	.L.str514,@object       # @.str514
.L.str514:
	.asciz	"-Wundef"
	.size	.L.str514, 8

	.type	.L.str515,@object       # @.str515
.L.str515:
	.asciz	"-Wno-undef"
	.size	.L.str515, 11

	.type	.L.str516,@object       # @.str516
.L.str516:
	.asciz	"-Wwrite-strings"
	.size	.L.str516, 16

	.type	.L.str517,@object       # @.str517
.L.str517:
	.asciz	"Mark strings as 'const char *'"
	.size	.L.str517, 31

	.type	.L.str518,@object       # @.str518
.L.str518:
	.asciz	"-Wno-write-strings"
	.size	.L.str518, 19

	.type	.L.str519,@object       # @.str519
.L.str519:
	.asciz	"-help"
	.size	.L.str519, 6

	.type	.L.str520,@object       # @.str520
.L.str520:
	.asciz	"-target-help"
	.size	.L.str520, 13

	.type	.L.str521,@object       # @.str521
.L.str521:
	.asciz	"-param"
	.size	.L.str521, 7

	.type	.L.str522,@object       # @.str522
.L.str522:
	.asciz	"-param option missing argument"
	.size	.L.str522, 31

	.type	.L.str523,@object       # @.str523
.L.str523:
	.asciz	"invalid --param option: %s"
	.size	.L.str523, 27

	.type	.L.str524,@object       # @.str524
.L.str524:
	.asciz	"invalid parameter value `%s'"
	.size	.L.str524, 29

	.type	.L.str525,@object       # @.str525
.L.str525:
	.asciz	"dumpbase"
	.size	.L.str525, 9

	.type	.L.str526,@object       # @.str526
.L.str526:
	.asciz	"pedantic"
	.size	.L.str526, 9

	.type	.L.str527,@object       # @.str527
.L.str527:
	.asciz	"pedantic-errors"
	.size	.L.str527, 16

	.type	.L.str528,@object       # @.str528
.L.str528:
	.asciz	"quiet"
	.size	.L.str528, 6

	.type	.L.str529,@object       # @.str529
.L.str529:
	.asciz	"version"
	.size	.L.str529, 8

	.type	.L.str530,@object       # @.str530
.L.str530:
	.asciz	"aux-info"
	.size	.L.str530, 9

	.type	W_options,@object       # @W_options
	.section	.rodata,"a",@progbits
	.align	16
W_options:
	.quad	.L.str536
	.quad	warn_unused_function
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str537
	.quad	.L.str538
	.quad	warn_unused_label
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str539
	.quad	.L.str540
	.quad	warn_unused_parameter
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str541
	.quad	.L.str542
	.quad	warn_unused_variable
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str543
	.quad	.L.str544
	.quad	warn_unused_value
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str545
	.quad	.L.str546
	.quad	warn_system_headers
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str547
	.quad	.L.str548
	.quad	warnings_are_errors
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str549
	.quad	.L.str550
	.quad	warn_shadow
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str551
	.quad	.L.str552
	.quad	warn_switch
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str553
	.quad	.L.str554
	.quad	warn_aggregate_return
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str555
	.quad	.L.str556
	.quad	warn_cast_align
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str557
	.quad	.L.str558
	.quad	warn_notreached
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str559
	.quad	.L.str560
	.quad	warn_uninitialized
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str561
	.quad	.L.str198
	.quad	warn_inline
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str562
	.quad	.L.str563
	.quad	warn_packed
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str564
	.quad	.L.str565
	.quad	warn_padded
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str566
	.quad	.L.str567
	.quad	warn_disabled_optimization
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str568
	.quad	.L.str569
	.quad	warn_deprecated_decl
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str570
	.quad	.L.str571
	.quad	warn_missing_noreturn
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str572
	.size	W_options, 608

	.type	.L.str531,@object       # @.str531
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str531:
	.asciz	"id-clash-"
	.size	.L.str531, 10

	.type	.L.str532,@object       # @.str532
.L.str532:
	.asciz	"-Wid-clash-LEN is no longer supported"
	.size	.L.str532, 38

	.type	.L.str533,@object       # @.str533
.L.str533:
	.asciz	"larger-than-"
	.size	.L.str533, 13

	.type	.L.str534,@object       # @.str534
.L.str534:
	.asciz	"unused"
	.size	.L.str534, 7

	.type	.L.str535,@object       # @.str535
.L.str535:
	.asciz	"no-unused"
	.size	.L.str535, 10

	.type	.L.str536,@object       # @.str536
.L.str536:
	.asciz	"unused-function"
	.size	.L.str536, 16

	.type	.L.str537,@object       # @.str537
.L.str537:
	.asciz	"Warn when a function is unused"
	.size	.L.str537, 31

	.type	.L.str538,@object       # @.str538
.L.str538:
	.asciz	"unused-label"
	.size	.L.str538, 13

	.type	.L.str539,@object       # @.str539
.L.str539:
	.asciz	"Warn when a label is unused"
	.size	.L.str539, 28

	.type	.L.str540,@object       # @.str540
.L.str540:
	.asciz	"unused-parameter"
	.size	.L.str540, 17

	.type	.L.str541,@object       # @.str541
.L.str541:
	.asciz	"Warn when a function parameter is unused"
	.size	.L.str541, 41

	.type	.L.str542,@object       # @.str542
.L.str542:
	.asciz	"unused-variable"
	.size	.L.str542, 16

	.type	.L.str543,@object       # @.str543
.L.str543:
	.asciz	"Warn when a variable is unused"
	.size	.L.str543, 31

	.type	.L.str544,@object       # @.str544
.L.str544:
	.asciz	"unused-value"
	.size	.L.str544, 13

	.type	.L.str545,@object       # @.str545
.L.str545:
	.asciz	"Warn when an expression value is unused"
	.size	.L.str545, 40

	.type	.L.str546,@object       # @.str546
.L.str546:
	.asciz	"system-headers"
	.size	.L.str546, 15

	.type	.L.str547,@object       # @.str547
.L.str547:
	.asciz	"Do not suppress warnings from system headers"
	.size	.L.str547, 45

	.type	.L.str548,@object       # @.str548
.L.str548:
	.asciz	"error"
	.size	.L.str548, 6

	.type	.L.str549,@object       # @.str549
.L.str549:
	.asciz	"Treat all warnings as errors"
	.size	.L.str549, 29

	.type	.L.str550,@object       # @.str550
.L.str550:
	.asciz	"shadow"
	.size	.L.str550, 7

	.type	.L.str551,@object       # @.str551
.L.str551:
	.asciz	"Warn when one local variable shadows another"
	.size	.L.str551, 45

	.type	.L.str552,@object       # @.str552
.L.str552:
	.asciz	"switch"
	.size	.L.str552, 7

	.type	.L.str553,@object       # @.str553
.L.str553:
	.asciz	"Warn about enumerated switches missing a specific case"
	.size	.L.str553, 55

	.type	.L.str554,@object       # @.str554
.L.str554:
	.asciz	"aggregate-return"
	.size	.L.str554, 17

	.type	.L.str555,@object       # @.str555
.L.str555:
	.asciz	"Warn about returning structures, unions or arrays"
	.size	.L.str555, 50

	.type	.L.str556,@object       # @.str556
.L.str556:
	.asciz	"cast-align"
	.size	.L.str556, 11

	.type	.L.str557,@object       # @.str557
.L.str557:
	.asciz	"Warn about pointer casts which increase alignment"
	.size	.L.str557, 50

	.type	.L.str558,@object       # @.str558
.L.str558:
	.asciz	"unreachable-code"
	.size	.L.str558, 17

	.type	.L.str559,@object       # @.str559
.L.str559:
	.asciz	"Warn about code that will never be executed"
	.size	.L.str559, 44

	.type	.L.str560,@object       # @.str560
.L.str560:
	.asciz	"uninitialized"
	.size	.L.str560, 14

	.type	.L.str561,@object       # @.str561
.L.str561:
	.asciz	"Warn about uninitialized automatic variables"
	.size	.L.str561, 45

	.type	.L.str562,@object       # @.str562
.L.str562:
	.asciz	"Warn when an inlined function cannot be inlined"
	.size	.L.str562, 48

	.type	.L.str563,@object       # @.str563
.L.str563:
	.asciz	"packed"
	.size	.L.str563, 7

	.type	.L.str564,@object       # @.str564
.L.str564:
	.asciz	"Warn when the packed attribute has no effect on struct layout"
	.size	.L.str564, 62

	.type	.L.str565,@object       # @.str565
.L.str565:
	.asciz	"padded"
	.size	.L.str565, 7

	.type	.L.str566,@object       # @.str566
.L.str566:
	.asciz	"Warn when padding is required to align struct members"
	.size	.L.str566, 54

	.type	.L.str567,@object       # @.str567
.L.str567:
	.asciz	"disabled-optimization"
	.size	.L.str567, 22

	.type	.L.str568,@object       # @.str568
.L.str568:
	.asciz	"Warn when an optimization pass is disabled"
	.size	.L.str568, 43

	.type	.L.str569,@object       # @.str569
.L.str569:
	.asciz	"deprecated-declarations"
	.size	.L.str569, 24

	.type	.L.str570,@object       # @.str570
.L.str570:
	.asciz	"Warn about uses of __attribute__((deprecated)) declarations"
	.size	.L.str570, 60

	.type	.L.str571,@object       # @.str571
.L.str571:
	.asciz	"missing-noreturn"
	.size	.L.str571, 17

	.type	.L.str572,@object       # @.str572
.L.str572:
	.asciz	"Warn about functions which might be candidates for attribute noreturn"
	.size	.L.str572, 70

	.type	.L.str573,@object       # @.str573
.L.str573:
	.asciz	"unrecognized gcc debugging option: %c"
	.size	.L.str573, 38

	.type	decode_g_option.level,@object # @decode_g_option.level
	.local	decode_g_option.level
	.comm	decode_g_option.level,4,4
	.type	decode_g_option.selected_debug_type,@object # @decode_g_option.selected_debug_type
	.local	decode_g_option.selected_debug_type
	.comm	decode_g_option.selected_debug_type,4,4
	.type	decode_g_option.type_explicitly_set_p,@object # @decode_g_option.type_explicitly_set_p
	.local	decode_g_option.type_explicitly_set_p
	.comm	decode_g_option.type_explicitly_set_p,4,4
	.type	decode_g_option.debug_type_names,@object # @decode_g_option.debug_type_names
	.section	.rodata,"a",@progbits
	.align	16
decode_g_option.debug_type_names:
	.quad	.L.str574
	.quad	.L.str575
	.quad	.L.str576
	.quad	.L.str577
	.quad	.L.str578
	.quad	.L.str579
	.quad	.L.str580
	.size	decode_g_option.debug_type_names, 56

	.type	.L.str574,@object       # @.str574
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str574:
	.asciz	"none"
	.size	.L.str574, 5

	.type	.L.str575,@object       # @.str575
.L.str575:
	.asciz	"stabs"
	.size	.L.str575, 6

	.type	.L.str576,@object       # @.str576
.L.str576:
	.asciz	"coff"
	.size	.L.str576, 5

	.type	.L.str577,@object       # @.str577
.L.str577:
	.asciz	"dwarf-1"
	.size	.L.str577, 8

	.type	.L.str578,@object       # @.str578
.L.str578:
	.asciz	"dwarf-2"
	.size	.L.str578, 8

	.type	.L.str579,@object       # @.str579
.L.str579:
	.asciz	"xcoff"
	.size	.L.str579, 6

	.type	.L.str580,@object       # @.str580
.L.str580:
	.asciz	"vms"
	.size	.L.str580, 4

	.type	debug_args,@object      # @debug_args
	.section	.rodata,"a",@progbits
	.align	16
debug_args:
	.quad	.L.str28
	.long	0                       # 0x0
	.long	1                       # 0x1
	.quad	.L.str587
	.quad	.L.str585
	.long	0                       # 0x0
	.long	1                       # 0x1
	.quad	.L.str588
	.quad	.L.str575
	.long	1                       # 0x1
	.long	0                       # 0x0
	.quad	.L.str589
	.quad	.L.str590
	.long	1                       # 0x1
	.long	1                       # 0x1
	.quad	.L.str591
	.quad	.L.str578
	.long	4                       # 0x4
	.long	0                       # 0x0
	.quad	.L.str592
	.zero	24
	.size	debug_args, 144

	.type	da,@object              # @da
	.local	da
	.comm	da,8,8
	.type	.L.str581,@object       # @.str581
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str581:
	.asciz	"dwarf"
	.size	.L.str581, 6

	.type	.L.str582,@object       # @.str582
.L.str582:
	.asciz	"use -gdwarf -g%d for DWARF v1, level %d"
	.size	.L.str582, 40

	.type	.L.str583,@object       # @.str583
.L.str583:
	.asciz	"use -gdwarf-2   for DWARF v2"
	.size	.L.str583, 29

	.type	.L.str584,@object       # @.str584
.L.str584:
	.asciz	"ignoring option `%s' due to invalid debug level specification"
	.size	.L.str584, 62

	.type	.L.str585,@object       # @.str585
.L.str585:
	.asciz	"gdb"
	.size	.L.str585, 4

	.type	.L.str586,@object       # @.str586
.L.str586:
	.asciz	"`%s' ignored, conflicts with `-g%s'"
	.size	.L.str586, 36

	.type	.L.str587,@object       # @.str587
.L.str587:
	.asciz	"Generate debugging info in default format"
	.size	.L.str587, 42

	.type	.L.str588,@object       # @.str588
.L.str588:
	.asciz	"Generate debugging info in default extended format"
	.size	.L.str588, 51

	.type	.L.str589,@object       # @.str589
.L.str589:
	.asciz	"Generate STABS format debug info"
	.size	.L.str589, 33

	.type	.L.str590,@object       # @.str590
.L.str590:
	.asciz	"stabs+"
	.size	.L.str590, 7

	.type	.L.str591,@object       # @.str591
.L.str591:
	.asciz	"Generate extended STABS format debug info"
	.size	.L.str591, 42

	.type	.L.str592,@object       # @.str592
.L.str592:
	.asciz	"Generate DWARF-2 debug info"
	.size	.L.str592, 28

	.type	.L.str593,@object       # @.str593
.L.str593:
	.asciz	"fast-math"
	.size	.L.str593, 10

	.type	.L.str594,@object       # @.str594
.L.str594:
	.asciz	"no-fast-math"
	.size	.L.str594, 13

	.type	.L.str595,@object       # @.str595
.L.str595:
	.asciz	"inline-limit-"
	.size	.L.str595, 14

	.type	.L.str596,@object       # @.str596
.L.str596:
	.asciz	"inline-limit="
	.size	.L.str596, 14

	.type	.L.str597,@object       # @.str597
.L.str597:
	.asciz	"max-inline-insns"
	.size	.L.str597, 17

	.type	.L.str598,@object       # @.str598
.L.str598:
	.asciz	"sched-verbose="
	.size	.L.str598, 15

	.type	.L.str599,@object       # @.str599
.L.str599:
	.asciz	"verbose"
	.size	.L.str599, 8

	.type	.L.str600,@object       # @.str600
.L.str600:
	.asciz	"fixed-"
	.size	.L.str600, 7

	.type	.L.str601,@object       # @.str601
.L.str601:
	.asciz	"call-used-"
	.size	.L.str601, 11

	.type	.L.str602,@object       # @.str602
.L.str602:
	.asciz	"call-saved-"
	.size	.L.str602, 12

	.type	.L.str603,@object       # @.str603
.L.str603:
	.asciz	"align-labels="
	.size	.L.str603, 14

	.type	.L.str604,@object       # @.str604
.L.str604:
	.asciz	"stack-limit-register="
	.size	.L.str604, 22

	.type	.L.str605,@object       # @.str605
.L.str605:
	.asciz	"unrecognized register name `%s'"
	.size	.L.str605, 32

	.type	.L.str606,@object       # @.str606
.L.str606:
	.asciz	"stack-limit-symbol="
	.size	.L.str606, 20

	.type	.L.str607,@object       # @.str607
.L.str607:
	.asciz	"message-length="
	.size	.L.str607, 16

	.type	.L.str608,@object       # @.str608
.L.str608:
	.asciz	"diagnostics-show-location="
	.size	.L.str608, 27

	.type	.L.str609,@object       # @.str609
.L.str609:
	.asciz	"once"
	.size	.L.str609, 5

	.type	.L.str610,@object       # @.str610
.L.str610:
	.asciz	"every-line"
	.size	.L.str610, 11

	.type	.L.str611,@object       # @.str611
.L.str611:
	.asciz	"no-stack-limit"
	.size	.L.str611, 15

	.type	.L.str612,@object       # @.str612
.L.str612:
	.asciz	"preprocessed"
	.size	.L.str612, 13

	.type	display_target_options.displayed,@object # @display_target_options.displayed
	.local	display_target_options.displayed
	.comm	display_target_options.displayed,1,1
	.type	.L.str614,@object       # @.str614
.L.str614:
	.asciz	"  -m%-23.23s [undocumented]\n"
	.size	.L.str614, 29

	.type	.L.str615,@object       # @.str615
.L.str615:
	.asciz	"  -m%-23.23s %s\n"
	.size	.L.str615, 17

	.type	.L.str624,@object       # @.str624
.L.str624:
	.asciz	"  -f%-21s %s\n"
	.size	.L.str624, 14

	.type	.L.str627,@object       # @.str627
.L.str627:
	.asciz	"  --param %s=<value>%.*s%s\n"
	.size	.L.str627, 28

	.type	.L.str628,@object       # @.str628
.L.str628:
	.asciz	"                     "
	.size	.L.str628, 22

	.type	.L.str633,@object       # @.str633
.L.str633:
	.asciz	"  -W%-21s %s\n"
	.size	.L.str633, 14

	.type	.L.str639,@object       # @.str639
.L.str639:
	.asciz	"  -g%-21s %s\n"
	.size	.L.str639, 14

	.type	.L.str652,@object       # @.str652
.L.str652:
	.asciz	"  %-23.23s %s\n"
	.size	.L.str652, 15

	.type	.L.str653,@object       # @.str653
.L.str653:
	.asciz	"The maximum number of instructions in a function that is eligible for inlining"
	.size	.L.str653, 79

	.type	.L.str654,@object       # @.str654
.L.str654:
	.asciz	"max-delay-slot-insn-search"
	.size	.L.str654, 27

	.type	.L.str655,@object       # @.str655
.L.str655:
	.asciz	"The maximum number of instructions to consider to fill a delay slot"
	.size	.L.str655, 68

	.type	.L.str656,@object       # @.str656
.L.str656:
	.asciz	"max-delay-slot-live-search"
	.size	.L.str656, 27

	.type	.L.str657,@object       # @.str657
.L.str657:
	.asciz	"The maximum number of instructions to consider to find accurate live register information"
	.size	.L.str657, 90

	.type	.L.str658,@object       # @.str658
.L.str658:
	.asciz	"max-pending-list-length"
	.size	.L.str658, 24

	.type	.L.str659,@object       # @.str659
.L.str659:
	.asciz	"The maximum length of scheduling's pending operations list"
	.size	.L.str659, 59

	.type	.L.str660,@object       # @.str660
.L.str660:
	.asciz	"max-gcse-memory"
	.size	.L.str660, 16

	.type	.L.str661,@object       # @.str661
.L.str661:
	.asciz	"The maximum amount of memory to be allocated by GCSE"
	.size	.L.str661, 53

	.type	.L.str662,@object       # @.str662
.L.str662:
	.asciz	"max-gcse-passes"
	.size	.L.str662, 16

	.type	.L.str663,@object       # @.str663
.L.str663:
	.asciz	"The maximum number of passes to make when doing GCSE"
	.size	.L.str663, 53

	.type	.L.str664,@object       # @.str664
.L.str664:
	.asciz	"internal error: %d"
	.size	.L.str664, 19

	.type	.L.str665,@object       # @.str665
.L.str665:
	.asciz	"rtl"
	.size	.L.str665, 4

	.type	.L.str666,@object       # @.str666
.L.str666:
	.asciz	"sibling"
	.size	.L.str666, 8

	.type	.L.str667,@object       # @.str667
.L.str667:
	.asciz	"eh"
	.size	.L.str667, 3

	.type	.L.str668,@object       # @.str668
.L.str668:
	.asciz	"jump"
	.size	.L.str668, 5

	.type	.L.str669,@object       # @.str669
.L.str669:
	.asciz	"ssaccp"
	.size	.L.str669, 7

	.type	.L.str670,@object       # @.str670
.L.str670:
	.asciz	"ssadce"
	.size	.L.str670, 7

	.type	.L.str671,@object       # @.str671
.L.str671:
	.asciz	"ussa"
	.size	.L.str671, 5

	.type	.L.str672,@object       # @.str672
.L.str672:
	.asciz	"cse"
	.size	.L.str672, 4

	.type	.L.str673,@object       # @.str673
.L.str673:
	.asciz	"addressof"
	.size	.L.str673, 10

	.type	.L.str674,@object       # @.str674
.L.str674:
	.asciz	"loop"
	.size	.L.str674, 5

	.type	.L.str675,@object       # @.str675
.L.str675:
	.asciz	"cse2"
	.size	.L.str675, 5

	.type	.L.str676,@object       # @.str676
.L.str676:
	.asciz	"cfg"
	.size	.L.str676, 4

	.type	.L.str677,@object       # @.str677
.L.str677:
	.asciz	"bp"
	.size	.L.str677, 3

	.type	.L.str678,@object       # @.str678
.L.str678:
	.asciz	"life"
	.size	.L.str678, 5

	.type	.L.str679,@object       # @.str679
.L.str679:
	.asciz	"combine"
	.size	.L.str679, 8

	.type	.L.str680,@object       # @.str680
.L.str680:
	.asciz	"ce"
	.size	.L.str680, 3

	.type	.L.str681,@object       # @.str681
.L.str681:
	.asciz	"sched"
	.size	.L.str681, 6

	.type	.L.str682,@object       # @.str682
.L.str682:
	.asciz	"lreg"
	.size	.L.str682, 5

	.type	.L.str683,@object       # @.str683
.L.str683:
	.asciz	"greg"
	.size	.L.str683, 5

	.type	.L.str684,@object       # @.str684
.L.str684:
	.asciz	"postreload"
	.size	.L.str684, 11

	.type	.L.str685,@object       # @.str685
.L.str685:
	.asciz	"flow2"
	.size	.L.str685, 6

	.type	.L.str686,@object       # @.str686
.L.str686:
	.asciz	"rnreg"
	.size	.L.str686, 6

	.type	.L.str687,@object       # @.str687
.L.str687:
	.asciz	"ce2"
	.size	.L.str687, 4

	.type	.L.str688,@object       # @.str688
.L.str688:
	.asciz	"sched2"
	.size	.L.str688, 7

	.type	.L.str689,@object       # @.str689
.L.str689:
	.asciz	"stack"
	.size	.L.str689, 6

	.type	.L.str690,@object       # @.str690
.L.str690:
	.asciz	"bbro"
	.size	.L.str690, 5

	.type	.L.str691,@object       # @.str691
.L.str691:
	.asciz	"mach"
	.size	.L.str691, 5

	.type	.L.str692,@object       # @.str692
.L.str692:
	.asciz	"dbr"
	.size	.L.str692, 4

	.type	dump_file,@object       # @dump_file
	.data
	.align	16
dump_file:
	.quad	.L.str665
	.byte	114                     # 0x72
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str666
	.byte	105                     # 0x69
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str667
	.byte	104                     # 0x68
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str668
	.byte	106                     # 0x6a
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str309
	.byte	101                     # 0x65
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str669
	.byte	87                      # 0x57
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str670
	.byte	88                      # 0x58
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str671
	.byte	101                     # 0x65
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str672
	.byte	115                     # 0x73
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str673
	.byte	70                      # 0x46
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str214
	.byte	71                      # 0x47
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str674
	.byte	76                      # 0x4c
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str675
	.byte	116                     # 0x74
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str676
	.byte	102                     # 0x66
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str677
	.byte	98                      # 0x62
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str678
	.byte	102                     # 0x66
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str679
	.byte	99                      # 0x63
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str680
	.byte	67                      # 0x43
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str277
	.byte	78                      # 0x4e
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str681
	.byte	83                      # 0x53
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str682
	.byte	108                     # 0x6c
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str683
	.byte	103                     # 0x67
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str684
	.byte	111                     # 0x6f
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str685
	.byte	119                     # 0x77
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str319
	.byte	122                     # 0x7a
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str686
	.byte	110                     # 0x6e
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str687
	.byte	69                      # 0x45
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str688
	.byte	82                      # 0x52
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str689
	.byte	107                     # 0x6b
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str690
	.byte	66                      # 0x42
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str691
	.byte	77                      # 0x4d
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	.L.str692
	.byte	100                     # 0x64
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.size	dump_file, 512

	.type	.L.str693,@object       # @.str693
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str693:
	.asciz	"a"
	.size	.L.str693, 2

	.type	.L.str694,@object       # @.str694
.L.str694:
	.asciz	"\n;; Function %s\n\n"
	.size	.L.str694, 18

	.type	float_handler_set,@object # @float_handler_set
	.local	float_handler_set
	.comm	float_handler_set,1,1
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"  -ffixed-<register>      Mark <register> as being unavailable to the compiler"
	.size	.Lstr, 79

	.type	.Lstr695,@object        # @str695
	.align	16
.Lstr695:
	.asciz	"  -fcall-used-<register>  Mark <register> as being corrupted by function calls"
	.size	.Lstr695, 79

	.type	.Lstr696,@object        # @str696
	.align	16
.Lstr696:
	.asciz	"  -fcall-saved-<register> Mark <register> as being preserved across functions"
	.size	.Lstr696, 78

	.type	.Lstr697,@object        # @str697
	.align	16
.Lstr697:
	.asciz	"  -finline-limit=<number> Limits the size of inlined functions to <number>"
	.size	.Lstr697, 75

	.type	.Lstr698,@object        # @str698
	.align	16
.Lstr698:
	.asciz	"  -fmessage-length=<number> Limits diagnostics messages lengths to <number> characters per line.  0 suppresses line-wrapping"
	.size	.Lstr698, 125

	.type	.Lstr699,@object        # @str699
	.align	16
.Lstr699:
	.asciz	"  -fdiagnostics-show-location=[once | every-line] Indicates how often source location information should be emitted, as prefix, at the beginning of diagnostics when line-wrapping"
	.size	.Lstr699, 179

	.type	.Lstr700,@object        # @str700
	.align	16
.Lstr700:
	.asciz	"  -O[number]              Set optimization level to [number]"
	.size	.Lstr700, 61

	.type	.Lstr701,@object        # @str701
	.align	16
.Lstr701:
	.asciz	"  -Os                     Optimize for space rather than speed"
	.size	.Lstr701, 63

	.type	.Lstr702,@object        # @str702
	.align	16
.Lstr702:
	.asciz	"  -pedantic               Issue warnings needed by strict compliance to ISO C"
	.size	.Lstr702, 78

	.type	.Lstr703,@object        # @str703
	.align	16
.Lstr703:
	.asciz	"  -pedantic-errors        Like -pedantic except that errors are produced"
	.size	.Lstr703, 73

	.type	.Lstr704,@object        # @str704
	.align	16
.Lstr704:
	.asciz	"  -w                      Suppress warnings"
	.size	.Lstr704, 44

	.type	.Lstr705,@object        # @str705
	.align	16
.Lstr705:
	.asciz	"  -W                      Enable extra warnings"
	.size	.Lstr705, 48

	.type	.Lstr706,@object        # @str706
	.align	16
.Lstr706:
	.asciz	"  -Wunused                Enable unused warnings"
	.size	.Lstr706, 49

	.type	.Lstr707,@object        # @str707
	.align	16
.Lstr707:
	.asciz	"  -Wlarger-than-<number>  Warn if an object is larger than <number> bytes"
	.size	.Lstr707, 74

	.type	.Lstr708,@object        # @str708
	.align	16
.Lstr708:
	.asciz	"  -p                      Enable function profiling"
	.size	.Lstr708, 52

	.type	.Lstr709,@object        # @str709
	.align	16
.Lstr709:
	.asciz	"  -o <file>               Place output into <file> "
	.size	.Lstr709, 52

	.type	.Lstr710,@object        # @str710
	.align	16
.Lstr710:
	.asciz	"  -G <number>             Put global and static data smaller than <number>\n                          bytes into a special section (on some targets)"
	.size	.Lstr710, 148

	.type	.Lstr711,@object        # @str711
	.align	16
.Lstr711:
	.asciz	"  -aux-info <file>        Emit declaration info into <file>"
	.size	.Lstr711, 60

	.type	.Lstr712,@object        # @str712
	.align	16
.Lstr712:
	.asciz	"  -quiet                  Do not display functions compiled or elapsed time"
	.size	.Lstr712, 76

	.type	.Lstr713,@object        # @str713
	.align	16
.Lstr713:
	.asciz	"  -version                Display the compiler's version"
	.size	.Lstr713, 57

	.type	.Lstr714,@object        # @str714
	.align	16
.Lstr714:
	.asciz	"  -d[letters]             Enable dumps from specific passes of the compiler"
	.size	.Lstr714, 76

	.type	.Lstr715,@object        # @str715
	.align	16
.Lstr715:
	.asciz	"  -dumpbase <file>        Base name to be used for dumps from specific passes"
	.size	.Lstr715, 78

	.type	.Lstr716,@object        # @str716
	.align	16
.Lstr716:
	.asciz	"  -fsched-verbose=<number> Set the verbosity level of the scheduler"
	.size	.Lstr716, 68

	.type	.Lstr717,@object        # @str717
	.align	16
.Lstr717:
	.asciz	"  --help                  Display this information"
	.size	.Lstr717, 51

	.type	.Lstr718,@object        # @str718
	.align	16
.Lstr718:
	.asciz	"\nLanguage specific options:"
	.size	.Lstr718, 28

	.type	.Lstr719,@object        # @str719
	.align	16
.Lstr719:
	.asciz	"\nTarget specific options:"
	.size	.Lstr719, 26

	.type	.Lstr720,@object        # @str720
	.align	16
.Lstr720:
	.asciz	"  They exist, but they are not documented."
	.size	.Lstr720, 43

	.type	.Lstr721,@object        # @str721
	.align	16
.Lstr721:
	.asciz	"\nThere are undocumented target specific options as well."
	.size	.Lstr721, 57


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
