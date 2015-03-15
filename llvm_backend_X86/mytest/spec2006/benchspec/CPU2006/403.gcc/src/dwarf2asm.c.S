	.file	"dwarf2asm.c.bc"
	.text
	.globl	dw2_assemble_integer
	.align	16, 0x90
	.type	dw2_assemble_integer,@function
dw2_assemble_integer:                   # @dw2_assemble_integer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rsi, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB0_3
# BB#1:                                 # %if.then
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%r14), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB0_2
# BB#4:                                 # %if.then2
	movq	8(%r14), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fprintf  # TAILCALL
.LBB0_3:                                # %if.else4
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	assemble_integer  # TAILCALL
.LBB0_2:                                # %if.else
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	output_addr_const  # TAILCALL
.Ltmp8:
	.size	dw2_assemble_integer, .Ltmp8-dw2_assemble_integer
	.cfi_endproc

	.globl	dw2_asm_output_data
	.align	16, 0x90
	.type	dw2_asm_output_data,@function
dw2_asm_output_data:                    # @dw2_asm_output_data
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp13:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp16:
	.cfi_def_cfa_offset 112
.Ltmp17:
	.cfi_offset %rbx, -32
.Ltmp18:
	.cfi_offset %r14, -24
.Ltmp19:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movq	$3, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$35, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$63, %rdx
	jg	.LBB1_2
# BB#1:                                 # %if.then
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rsi
.LBB1_2:                                # %if.end
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB1_6
# BB#3:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%r15), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB1_5
# BB#4:                                 # %if.then2.i
	movq	8(%r15), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB1_7
.LBB1_6:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	assemble_integer
	jmp	.LBB1_7
.LBB1_5:                                # %if.else.i
	movq	%r15, %rsi
	callq	output_addr_const
.LBB1_7:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_10
# BB#8:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB1_10
# BB#9:                                 # %if.then4
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB1_10:                               # %if.end8
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp20:
	.size	dw2_asm_output_data, .Ltmp20-dw2_asm_output_data
	.cfi_endproc

	.globl	dw2_asm_output_delta
	.align	16, 0x90
	.type	dw2_asm_output_delta,@function
dw2_asm_output_delta:                   # @dw2_asm_output_delta
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp32:
	.cfi_def_cfa_offset 128
.Ltmp33:
	.cfi_offset %rbx, -48
.Ltmp34:
	.cfi_offset %r12, -40
.Ltmp35:
	.cfi_offset %r13, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %rax
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	128(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movl	target_flags(%rip), %ebx
	movq	$48, 52(%rsp)
	movq	$32, 48(%rsp)
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movl	target_flags(%rip), %esi
	movq	%rax, %r13
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_s
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB2_4
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%rbx), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB2_3
# BB#2:                                 # %if.then2.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB2_5
.LBB2_4:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	assemble_integer
	jmp	.LBB2_5
.LBB2_3:                                # %if.else.i
	movq	%rbx, %rsi
	callq	output_addr_const
.LBB2_5:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_8
# BB#6:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB2_8
# BB#7:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB2_8:                                # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	dw2_asm_output_delta, .Ltmp38-dw2_asm_output_delta
	.cfi_endproc

	.globl	dw2_asm_output_offset
	.align	16, 0x90
	.type	dw2_asm_output_offset,@function
dw2_asm_output_offset:                  # @dw2_asm_output_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp46:
	.cfi_def_cfa_offset 112
.Ltmp47:
	.cfi_offset %rbx, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	112(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movl	target_flags(%rip), %esi
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB3_4
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%rbx), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB3_3
# BB#2:                                 # %if.then2.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB3_5
.LBB3_4:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	assemble_integer
	jmp	.LBB3_5
.LBB3_3:                                # %if.else.i
	movq	%rbx, %rsi
	callq	output_addr_const
.LBB3_5:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_8
# BB#6:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB3_8
# BB#7:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB3_8:                                # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp50:
	.size	dw2_asm_output_offset, .Ltmp50-dw2_asm_output_offset
	.cfi_endproc

	.globl	dw2_asm_output_pcrel
	.align	16, 0x90
	.type	dw2_asm_output_pcrel,@function
dw2_asm_output_pcrel:                   # @dw2_asm_output_pcrel
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp55:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp58:
	.cfi_def_cfa_offset 112
.Ltmp59:
	.cfi_offset %rbx, -32
.Ltmp60:
	.cfi_offset %r14, -24
.Ltmp61:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	112(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movl	target_flags(%rip), %ebx
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	global_rtl(%rip), %rcx
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB4_4
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%rbx), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB4_3
# BB#2:                                 # %if.then2.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB4_5
.LBB4_4:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	assemble_integer
	jmp	.LBB4_5
.LBB4_3:                                # %if.else.i
	movq	%rbx, %rsi
	callq	output_addr_const
.LBB4_5:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_8
# BB#6:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB4_8
# BB#7:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB4_8:                                # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp62:
	.size	dw2_asm_output_pcrel, .Ltmp62-dw2_asm_output_pcrel
	.cfi_endproc

	.globl	dw2_asm_output_addr
	.align	16, 0x90
	.type	dw2_asm_output_addr,@function
dw2_asm_output_addr:                    # @dw2_asm_output_addr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp70:
	.cfi_def_cfa_offset 112
.Ltmp71:
	.cfi_offset %rbx, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rax
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	112(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movl	target_flags(%rip), %esi
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB5_4
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%rbx), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB5_3
# BB#2:                                 # %if.then2.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB5_5
.LBB5_4:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	assemble_integer
	jmp	.LBB5_5
.LBB5_3:                                # %if.else.i
	movq	%rbx, %rsi
	callq	output_addr_const
.LBB5_5:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_8
# BB#6:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB5_8
# BB#7:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB5_8:                                # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	dw2_asm_output_addr, .Ltmp74-dw2_asm_output_addr
	.cfi_endproc

	.globl	dw2_asm_output_addr_rtx
	.align	16, 0x90
	.type	dw2_asm_output_addr_rtx,@function
dw2_asm_output_addr_rtx:                # @dw2_asm_output_addr_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp82:
	.cfi_def_cfa_offset 112
.Ltmp83:
	.cfi_offset %rbx, -32
.Ltmp84:
	.cfi_offset %r14, -24
.Ltmp85:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	xorq	%rsi, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB6_4
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%r15), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB6_3
# BB#2:                                 # %if.then2.i
	movq	8(%r15), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB6_5
.LBB6_4:                                # %if.else4.i
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	assemble_integer
	jmp	.LBB6_5
.LBB6_3:                                # %if.else.i
	movq	%r15, %rsi
	callq	output_addr_const
.LBB6_5:                                # %dw2_assemble_integer.exit
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_8
# BB#6:                                 # %dw2_assemble_integer.exit
	testq	%r14, %r14
	je	.LBB6_8
# BB#7:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB6_8:                                # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp86:
	.size	dw2_asm_output_addr_rtx, .Ltmp86-dw2_asm_output_addr_rtx
	.cfi_endproc

	.globl	dw2_asm_output_nstring
	.align	16, 0x90
	.type	dw2_asm_output_nstring,@function
dw2_asm_output_nstring:                 # @dw2_asm_output_nstring
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp95:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp96:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp97:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp98:
	.cfi_def_cfa_offset 144
.Ltmp99:
	.cfi_offset %rbx, -48
.Ltmp100:
	.cfi_offset %r12, -40
.Ltmp101:
	.cfi_offset %r13, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	%r9, 56(%rsp)
	movq	%r8, 48(%rsp)
	movq	%rcx, 40(%rsp)
	cmpq	$-1, %rbx
	leaq	16(%rsp), %rax
	movq	%rax, 80(%rsp)
	leaq	144(%rsp), %rax
	movq	%rax, 72(%rsp)
	movq	$48, 68(%rsp)
	movq	$24, 64(%rsp)
	movq	%rbx, %r14
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r14
.LBB7_2:                                # %if.end
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB7_14
# BB#3:                                 # %if.end
	testq	%r13, %r13
	je	.LBB7_14
# BB#4:                                 # %if.then3
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	callq	fwrite
	testq	%r14, %r14
	je	.LBB7_13
# BB#5:
	movabsq	$.L.str4, %r15
	.align	16, 0x90
.LBB7_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	movq	$56, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	cmpq	$34, %rax
	je	.LBB7_8
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB7_6 Depth=1
	cmpq	$92, %rax
	jne	.LBB7_9
.LBB7_8:                                # %if.then10
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	asm_out_file(%rip), %rsi
	movq	$92, %rdi
	callq	fputc
.LBB7_9:                                # %if.end12
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%rbx, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	movq	asm_out_file(%rip), %rsi
	testq	$16, %rax
	jne	.LBB7_10
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB7_6 Depth=1
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB7_12
	.align	16, 0x90
.LBB7_10:                               # %if.then17
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%rbx, %rdi
	callq	fputc
.LBB7_12:                               # %for.inc
                                        #   in Loop: Header=BB7_6 Depth=1
	incq	%r12
	decq	%r14
	jne	.LBB7_6
.LBB7_13:                               # %for.end
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str5, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	64(%rsp), %rdx
	movq	%r13, %rsi
	callq	vfprintf
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	jmp	.LBB7_50
.LBB7_14:                               # %if.else25
	cmpq	$-1, %rbx
	jne	.LBB7_15
# BB#16:                                # %if.else25
	movabsq	$1, %rax
	jmp	.LBB7_17
.LBB7_15:
	movabsq	$0, %rax
.LBB7_17:                               # %if.else25
	addq	%rax, %r14
	testq	%r14, %r14
	jle	.LBB7_48
# BB#18:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	addq	%r12, %r14
	xorq	%r15, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB7_19:                               # %for.body33
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_28 Depth 2
                                        #     Child Loop BB7_34 Depth 2
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$60, %rax
	jb	.LBB7_21
# BB#20:                                # %if.then36
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	callq	fwrite
	xorq	%r15, %r15
.LBB7_21:                               # %if.end38
                                        #   in Loop: Header=BB7_19 Depth=1
	cmpq	%r14, %r12
	movq	%r12, %rbx
	jae	.LBB7_22
	.align	16, 0x90
.LBB7_28:                               # %land.rhs
                                        #   Parent Loop BB7_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_29
# BB#27:                                # %for.cond39
                                        #   in Loop: Header=BB7_28 Depth=2
	incq	%rbx
	cmpq	%r14, %rbx
	jb	.LBB7_28
	jmp	.LBB7_22
	.align	16, 0x90
.LBB7_29:                               # %land.lhs.true50
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	%rbx, %rax
	subq	%r12, %rax
	cmpq	$256, %rax              # imm = 0x100
	jg	.LBB7_22
# BB#30:                                # %if.then53
                                        #   in Loop: Header=BB7_19 Depth=1
	testq	%r13, %r15
	je	.LBB7_32
# BB#31:                                # %if.then56
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	callq	fwrite
.LBB7_32:                               # %do.body59
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str7, %rsi
	movabsq	$.L.str8, %rdx
	callq	fprintf
	movzbl	(%r12), %edx
	testq	%rdx, %rdx
	je	.LBB7_41
# BB#33:                                # %for.body64.preheader
                                        #   in Loop: Header=BB7_19 Depth=1
	incq	%r12
	.align	16, 0x90
.LBB7_34:                               # %for.body64
                                        #   Parent Loop BB7_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rdx
	movsbq	.L.str9(%rdx), %r15
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB7_35
# BB#37:                                # %sw.bb69
                                        #   in Loop: Header=BB7_34 Depth=2
	movq	asm_out_file(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	callq	fprintf
	jmp	.LBB7_40
	.align	16, 0x90
.LBB7_35:                               # %for.body64
                                        #   in Loop: Header=BB7_34 Depth=2
	testq	%r13, %r15
	jne	.LBB7_38
# BB#36:                                # %sw.bb
                                        #   in Loop: Header=BB7_34 Depth=2
	movq	asm_out_file(%rip), %rsi
	movq	%rdx, %rdi
	jmp	.LBB7_39
	.align	16, 0x90
.LBB7_38:                               # %sw.default
                                        #   in Loop: Header=BB7_34 Depth=2
	movq	asm_out_file(%rip), %rsi
	movq	$92, %rdi
	callq	putc
	movq	asm_out_file(%rip), %rsi
	movq	%r15, %rdi
.LBB7_39:                               # %for.inc73
                                        #   in Loop: Header=BB7_34 Depth=2
	callq	putc
.LBB7_40:                               # %for.inc73
                                        #   in Loop: Header=BB7_34 Depth=2
	movzbl	(%r12), %edx
	incq	%r12
	testq	%rdx, %rdx
	jne	.LBB7_34
.LBB7_41:                               # %for.end75
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	callq	fwrite
	xorq	%r15, %r15
	jmp	.LBB7_45
	.align	16, 0x90
.LBB7_22:                               # %if.else77
                                        #   in Loop: Header=BB7_19 Depth=1
	testq	%r13, %r15
	jne	.LBB7_24
# BB#23:                                # %if.then82
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str7, %rsi
	movabsq	$.L.str11, %rdx
	callq	fprintf
.LBB7_24:                               # %if.end84
                                        #   in Loop: Header=BB7_19 Depth=1
	movzbl	(%r12), %edx
	movsbq	.L.str9(%rdx), %rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB7_25
# BB#42:                                # %sw.bb92
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	callq	fprintf
	addq	$4, %r15
	jmp	.LBB7_44
	.align	16, 0x90
.LBB7_25:                               # %if.end84
                                        #   in Loop: Header=BB7_19 Depth=1
	testq	%r13, %rbx
	jne	.LBB7_43
# BB#26:                                # %sw.bb89
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rsi
	movq	%rdx, %rdi
	callq	putc
	incq	%r15
	jmp	.LBB7_44
	.align	16, 0x90
.LBB7_43:                               # %sw.default95
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	asm_out_file(%rip), %rsi
	movq	$92, %rdi
	callq	putc
	movq	asm_out_file(%rip), %rsi
	movq	%rbx, %rdi
	callq	putc
	addq	$2, %r15
.LBB7_44:                               # %for.inc101
                                        #   in Loop: Header=BB7_19 Depth=1
	movq	%r12, %rbx
.LBB7_45:                               # %for.inc101
                                        #   in Loop: Header=BB7_19 Depth=1
	incq	%rbx
	cmpq	%r14, %rbx
	movq	%rbx, %r12
	jb	.LBB7_19
# BB#46:                                # %for.end103
	testq	%r13, %r15
	movq	8(%rsp), %rbx           # 8-byte Reload
	je	.LBB7_48
# BB#47:                                # %if.then106
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str6, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB7_48:                               # %do.end109
	cmpq	$-1, %rbx
	je	.LBB7_50
# BB#49:                                # %if.then112
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rsi
	movq	$8, %rdx
	movq	$1, %rcx
	callq	assemble_integer
.LBB7_50:                               # %if.end115
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	dw2_asm_output_nstring, .Ltmp104-dw2_asm_output_nstring
	.cfi_endproc

	.globl	size_of_uleb128
	.align	16, 0x90
	.type	size_of_uleb128,@function
size_of_uleb128:                        # @size_of_uleb128
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movq	$7, %rcx
	.align	16, 0x90
.LBB8_1:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	shrq	%cl, %rdi
	incq	%rax
	testq	%rdi, %rdi
	jne	.LBB8_1
# BB#2:                                 # %do.end
	retq
.Ltmp105:
	.size	size_of_uleb128, .Ltmp105-size_of_uleb128
	.cfi_endproc

	.globl	size_of_sleb128
	.align	16, 0x90
	.type	size_of_sleb128,@function
size_of_sleb128:                        # @size_of_sleb128
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movabsq	$0, %r9
	movabsq	$1, %r8
	.align	16, 0x90
.LBB9_1:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rsi
	movq	$7, %rcx
	sarq	%cl, %rdi
	andq	$64, %rsi
	movq	%r9, %rcx
	jne	.LBB9_3
# BB#2:                                 # %do.body
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r8, %rcx
.LBB9_3:                                # %do.body
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	$-1, %rdi
	movq	%r9, %rdx
	je	.LBB9_5
# BB#4:                                 # %do.body
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r8, %rdx
.LBB9_5:                                # %do.body
                                        #   in Loop: Header=BB9_1 Depth=1
	incq	%rax
	orq	%rdi, %rsi
	je	.LBB9_7
# BB#6:                                 # %do.body
                                        #   in Loop: Header=BB9_1 Depth=1
	orq	%rcx, %rdx
	jne	.LBB9_1
.LBB9_7:                                # %do.end
	retq
.Ltmp106:
	.size	size_of_sleb128, .Ltmp106-size_of_sleb128
	.cfi_endproc

	.globl	size_of_encoded_value
	.align	16, 0x90
	.type	size_of_encoded_value,@function
size_of_encoded_value:                  # @size_of_encoded_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp108:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	xorq	%rax, %rax
	cmpq	$255, %rcx
	je	.LBB10_7
# BB#1:                                 # %if.end
	andq	$7, %rdi
	cmpq	$4, %rdi
	ja	.LBB10_5
# BB#2:                                 # %if.end
	movq	$2, %rax
	jmpq	*.LJTI10_0(,%rdi,8)
.LBB10_6:                               # %sw.bb
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
.LBB10_7:                               # %return
	popq	%rdx
	retq
.LBB10_3:                               # %sw.bb3
	movq	$4, %rax
	popq	%rdx
	retq
.LBB10_4:                               # %sw.bb4
	movq	$8, %rax
	popq	%rdx
	retq
.LBB10_5:                               # %sw.epilog
	movabsq	$.L.str12, %rdi
	movabsq	$.L__FUNCTION__.size_of_encoded_value, %rdx
	movq	$340, %rsi              # imm = 0x154
	callq	fancy_abort
.Ltmp109:
	.size	size_of_encoded_value, .Ltmp109-size_of_encoded_value
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_6
	.quad	.LBB10_5
	.quad	.LBB10_7
	.quad	.LBB10_3
	.quad	.LBB10_4

	.text
	.globl	eh_data_format_name
	.align	16, 0x90
	.type	eh_data_format_name,@function
eh_data_format_name:                    # @eh_data_format_name
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp111:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$255, %rax
	ja	.LBB11_3
# BB#1:                                 # %lor.lhs.false2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	eh_data_format_name.format_names(,%rdi,8), %rax
	testq	%rax, %rax
	je	.LBB11_3
# BB#2:                                 # %if.end
	popq	%rdx
	retq
.LBB11_3:                               # %if.then
	movabsq	$.L.str12, %rax
	movabsq	$.L__FUNCTION__.eh_data_format_name, %rcx
	movq	$494, %rsi              # imm = 0x1EE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp112:
	.size	eh_data_format_name, .Ltmp112-eh_data_format_name
	.cfi_endproc

	.globl	dw2_asm_output_data_uleb128
	.align	16, 0x90
	.type	dw2_asm_output_data_uleb128,@function
dw2_asm_output_data_uleb128:            # @dw2_asm_output_data_uleb128
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp118:
	.cfi_def_cfa_offset 96
.Ltmp119:
	.cfi_offset %rbx, -24
.Ltmp120:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	asm_out_file(%rip), %rcx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movabsq	$.L.str96, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_3
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB12_3
# BB#2:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB12_3:                               # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp121:
	.size	dw2_asm_output_data_uleb128, .Ltmp121-dw2_asm_output_data_uleb128
	.cfi_endproc

	.globl	dw2_asm_output_data_sleb128
	.align	16, 0x90
	.type	dw2_asm_output_data_sleb128,@function
dw2_asm_output_data_sleb128:            # @dw2_asm_output_data_sleb128
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp125:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp127:
	.cfi_def_cfa_offset 96
.Ltmp128:
	.cfi_offset %rbx, -24
.Ltmp129:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	asm_out_file(%rip), %rcx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movabsq	$.L.str97, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str98, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_3
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB13_3
# BB#2:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB13_3:                               # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp130:
	.size	dw2_asm_output_data_sleb128, .Ltmp130-dw2_asm_output_data_sleb128
	.cfi_endproc

	.globl	dw2_asm_output_delta_uleb128
	.align	16, 0x90
	.type	dw2_asm_output_delta_uleb128,@function
dw2_asm_output_delta_uleb128:           # @dw2_asm_output_delta_uleb128
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 112
.Ltmp139:
	.cfi_offset %rbx, -32
.Ltmp140:
	.cfi_offset %r14, -24
.Ltmp141:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	asm_out_file(%rip), %rcx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movabsq	$.L.str96, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rsi
	movq	$45, %rdi
	callq	fputc
	movq	asm_out_file(%rip), %rdi
	movq	%r15, %rsi
	callq	assemble_name
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_3
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB14_3
# BB#2:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB14_3:                               # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp142:
	.size	dw2_asm_output_delta_uleb128, .Ltmp142-dw2_asm_output_delta_uleb128
	.cfi_endproc

	.globl	dw2_asm_output_delta_sleb128
	.align	16, 0x90
	.type	dw2_asm_output_delta_sleb128,@function
dw2_asm_output_delta_sleb128:           # @dw2_asm_output_delta_sleb128
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp147:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp148:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp149:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp150:
	.cfi_def_cfa_offset 112
.Ltmp151:
	.cfi_offset %rbx, -32
.Ltmp152:
	.cfi_offset %r14, -24
.Ltmp153:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	asm_out_file(%rip), %rcx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	movabsq	$.L.str97, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rsi
	movq	$45, %rdi
	callq	fputc
	movq	asm_out_file(%rip), %rdi
	movq	%r15, %rsi
	callq	assemble_name
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_3
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB15_3
# BB#2:                                 # %if.then
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	48(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB15_3:                               # %if.end
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp154:
	.size	dw2_asm_output_delta_sleb128, .Ltmp154-dw2_asm_output_delta_sleb128
	.cfi_endproc

	.globl	dw2_output_indirect_constants
	.align	16, 0x90
	.type	dw2_output_indirect_constants,@function
dw2_output_indirect_constants:          # @dw2_output_indirect_constants
	.cfi_startproc
# BB#0:                                 # %entry
	movq	indirect_pool(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB16_1
# BB#2:                                 # %if.then
	movabsq	$dw2_output_indirect_constant_1, %rsi
	xorq	%rdx, %rdx
	jmp	splay_tree_foreach  # TAILCALL
.LBB16_1:                               # %if.end
	retq
.Ltmp155:
	.size	dw2_output_indirect_constants, .Ltmp155-dw2_output_indirect_constants
	.cfi_endproc

	.align	16, 0x90
	.type	dw2_output_indirect_constant_1,@function
dw2_output_indirect_constant_1:         # @dw2_output_indirect_constant_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp160:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp161:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp162:
	.cfi_def_cfa_offset 32
.Ltmp163:
	.cfi_offset %rbx, -32
.Ltmp164:
	.cfi_offset %r14, -24
.Ltmp165:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %esi
	movq	%rdi, %r15
	movq	(%r15), %rbx
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_s
	movq	asm_out_file(%rip), %rdi
	movq	%rax, %r14
	movabsq	$.L.str103, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movq	8(%r15), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	callq	assemble_variable
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	$3, %rcx
	movq	%rdx, %rsi
	sarq	%cl, %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	assemble_integer
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp166:
	.size	dw2_output_indirect_constant_1, .Ltmp166-dw2_output_indirect_constant_1
	.cfi_endproc

	.globl	dw2_asm_output_encoded_addr_rtx
	.align	16, 0x90
	.type	dw2_asm_output_encoded_addr_rtx,@function
dw2_asm_output_encoded_addr_rtx:        # @dw2_asm_output_encoded_addr_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp175:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp176:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp177:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp178:
	.cfi_def_cfa_offset 144
.Ltmp179:
	.cfi_offset %rbx, -48
.Ltmp180:
	.cfi_offset %r12, -40
.Ltmp181:
	.cfi_offset %r13, -32
.Ltmp182:
	.cfi_offset %r14, -24
.Ltmp183:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r13
	movq	%r9, 56(%rsp)
	movq	%r8, 48(%rsp)
	movq	%rcx, 40(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r13, %rcx
	andq	%rax, %rcx
	xorq	%r15, %r15
	cmpq	$255, %rcx
	leaq	16(%rsp), %rcx
	movq	%rcx, 80(%rsp)
	leaq	144(%rsp), %rcx
	movq	%rcx, 72(%rsp)
	movq	$48, 68(%rsp)
	movq	$24, 64(%rsp)
	je	.LBB18_9
# BB#1:                                 # %if.end.i
	movq	%r13, %rcx
	andq	$7, %rcx
	cmpq	$4, %rcx
	ja	.LBB18_5
# BB#2:                                 # %if.end.i
	movq	$2, %r15
	jmpq	*.LJTI18_0(,%rcx,8)
.LBB18_6:                               # %sw.bb.i
	movl	target_flags(%rip), %r15d
	movq	$20, %rcx
	shrq	%cl, %r15
	andq	$32, %r15
	addq	$32, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	jmp	.LBB18_7
.LBB18_3:                               # %sw.bb3.i
	movq	$4, %r15
	jmp	.LBB18_7
.LBB18_4:                               # %sw.bb4.i
	movq	$8, %r15
.LBB18_7:                               # %size_of_encoded_value.exit
	movq	%r13, %rcx
	andq	%rax, %rcx
	cmpq	$80, %rcx
	jne	.LBB18_9
# BB#8:                                 # %if.then
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	callq	assemble_align
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	assemble_integer
	jmp	.LBB18_49
.LBB18_9:                               # %if.end
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%r12, %rcx
	je	.LBB18_24
# BB#10:                                # %if.end
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%r12, %rcx
	je	.LBB18_24
# BB#11:                                # %do.body.preheader
	movq	%r15, %rcx
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jne	.LBB18_14
# BB#12:                                # %do.body.preheader
	movq	%r13, %rdx
	andq	$112, %rdx
	andq	%rax, %rdx
	cmpq	$48, %rdx
	jne	.LBB18_14
# BB#13:
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rbx
	jmp	.LBB18_27
.LBB18_14:                              # %do.end.lr.ph
	cmpq	$4, %rcx
	jne	.LBB18_31
# BB#15:
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967167, %r14       # imm = 0xFFFFFF7F
	.align	16, 0x90
.LBB18_16:                              # %do.end.us
                                        # =>This Inner Loop Header: Depth=1
	testq	$128, %r13
	je	.LBB18_17
# BB#26:                                # %if.then22.us
                                        #   in Loop: Header=BB18_16 Depth=1
	movq	%r12, %rdi
	callq	dw2_force_const_mem
	movq	%rax, %r12
	movq	%r13, %rbx
	andq	%r14, %rbx
	andq	$112, %r13
	cmpq	$48, %r13
	movq	%rbx, %r13
	jne	.LBB18_16
.LBB18_27:                              # %if.then13
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str99, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	8(%r12), %rsi
	callq	assemble_name
	andq	$128, %rbx
	movq	$7, %rcx
	shrq	%cl, %rbx
	testq	%rbx, %rbx
	jne	.LBB18_28
# BB#29:                                # %if.then13
	movabsq	$.L.str101, %rdi
	jmp	.LBB18_30
.LBB18_28:
	movabsq	$.L.str100, %rdi
.LBB18_30:                              # %if.then13
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	asm_out_file(%rip), %rsi
	callq	fputs
	jmp	.LBB18_45
.LBB18_31:
	movabsq	$4294967167, %rbx       # imm = 0xFFFFFF7F
	jmp	.LBB18_32
	.align	16, 0x90
.LBB18_33:                              # %if.then22
                                        #   in Loop: Header=BB18_32 Depth=1
	movq	%r12, %rdi
	callq	dw2_force_const_mem
	movq	%rax, %r12
	andq	%rbx, %r13
.LBB18_32:                              # %do.end
                                        # =>This Inner Loop Header: Depth=1
	testq	$128, %r13
	jne	.LBB18_33
	jmp	.LBB18_18
.LBB18_17:
	movq	8(%rsp), %r14           # 8-byte Reload
.LBB18_18:                              # %if.end25
	andq	$240, %r13
	cmpq	$16, %r13
	jne	.LBB18_19
# BB#34:                                # %sw.bb27
	movzbl	(%r12), %eax
	cmpq	$68, %rax
	jne	.LBB18_35
# BB#37:                                # %if.end30
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl(%rip), %rax
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$76, %rdi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB18_41
# BB#38:                                # %if.then.i75
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%rbx), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB18_40
# BB#39:                                # %if.then2.i78
	movq	8(%rbx), %rdx
	jmp	.LBB18_23
.LBB18_19:                              # %if.end25
	testq	%r13, %r13
	jne	.LBB18_42
# BB#20:                                # %sw.bb
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	integer_asm_op
	testq	%rax, %rax
	je	.LBB18_24
# BB#21:                                # %if.then.i
	movq	asm_out_file(%rip), %rsi
	movq	%rax, %rdi
	callq	fputs
	movzbl	(%r12), %eax
	movq	asm_out_file(%rip), %rdi
	cmpq	$54, %rax
	jne	.LBB18_43
# BB#22:                                # %if.then2.i
	movq	8(%r12), %rdx
.LBB18_23:                              # %if.end35
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB18_45
.LBB18_24:                              # %if.then8
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
.LBB18_25:                              # %if.end35
	movq	%r15, %rsi
	callq	assemble_integer
.LBB18_45:                              # %if.end35
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB18_48
# BB#46:                                # %if.end35
	testq	%r14, %r14
	je	.LBB18_48
# BB#47:                                # %if.then39
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	leaq	64(%rsp), %rdx
	movq	%r14, %rsi
	callq	vfprintf
.LBB18_48:                              # %if.end43
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB18_49:                              # %return
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_5:                               # %sw.epilog.i
	movabsq	$.L.str12, %rdi
	movabsq	$.L__FUNCTION__.size_of_encoded_value, %rdx
	movq	$340, %rsi              # imm = 0x154
	callq	fancy_abort
.LBB18_41:                              # %if.else4.i81
	movq	$8, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	jmp	.LBB18_25
.LBB18_40:                              # %if.else.i79
	movq	%rbx, %rsi
	jmp	.LBB18_44
.LBB18_43:                              # %if.else.i
	movq	%r12, %rsi
.LBB18_44:                              # %if.end35
	callq	output_addr_const
	jmp	.LBB18_45
.LBB18_35:                              # %if.then29
	movabsq	$.L.str12, %rax
	movabsq	$.L__FUNCTION__.dw2_asm_output_encoded_addr_rtx, %rcx
	movq	$877, %rsi              # imm = 0x36D
	jmp	.LBB18_36
.LBB18_42:                              # %sw.default
	movabsq	$.L.str12, %rax
	movabsq	$.L__FUNCTION__.dw2_asm_output_encoded_addr_rtx, %rcx
	movq	$888, %rsi              # imm = 0x378
.LBB18_36:                              # %if.then29
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp184:
	.size	dw2_asm_output_encoded_addr_rtx, .Ltmp184-dw2_asm_output_encoded_addr_rtx
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_6
	.quad	.LBB18_5
	.quad	.LBB18_7
	.quad	.LBB18_3
	.quad	.LBB18_4

	.text
	.align	16, 0x90
	.type	dw2_force_const_mem,@function
dw2_force_const_mem:                    # @dw2_force_const_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp188:
	.cfi_def_cfa_offset 16
.Ltmp189:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp190:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
.Ltmp191:
	.cfi_offset %rbx, -32
.Ltmp192:
	.cfi_offset %r14, -24
	cmpq	$0, indirect_pool(%rip)
	movq	%rdi, %rbx
	jne	.LBB19_2
# BB#1:                                 # %if.then
	movabsq	$splay_tree_compare_pointers, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	splay_tree_new
	movq	%rax, indirect_pool(%rip)
	movabsq	$indirect_pool, %rdi
	movabsq	$mark_indirect_pool, %rcx
	movq	$1, %rsi
	movq	$8, %rdx
	callq	ggc_add_root
.LBB19_2:                               # %if.end
	movzbl	(%rbx), %eax
	cmpq	$68, %rax
	jne	.LBB19_14
# BB#3:                                 # %if.end2
	movq	8(%rbx), %r14
	movzbl	(%r14), %eax
	cmpq	$42, %rax
	jne	.LBB19_4
# BB#5:                                 # %if.end2
	movabsq	$1, %rax
	jmp	.LBB19_6
.LBB19_4:
	movabsq	$0, %rax
.LBB19_6:                               # %if.end2
	movq	indirect_pool(%rip), %rdi
	addq	%rax, %r14
	movq	%r14, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB19_8
# BB#7:                                 # %if.then11
	movq	8(%rax), %rbx
	jmp	.LBB19_11
.LBB19_8:                               # %if.else
	movq	%r14, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$23, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movabsq	$.L.str102, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	sprintf
	movq	%rbx, %rdi
	callq	get_identifier
	movq	global_trees+224(%rip), %rdx
	movq	$34, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194304, 48(%rbx)      # imm = 0x400000
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	%rbx, 104(%rbx)
	movq	%rbx, %rdi
	callq	make_decl_one_only
	movq	%r14, %rdi
	callq	maybe_get_identifier
	testq	%rax, %rax
	je	.LBB19_10
# BB#9:                                 # %if.then25
	movl	16(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 16(%rax)
.LBB19_10:                              # %if.end30
	movq	indirect_pool(%rip), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_insert
.LBB19_11:                              # %if.end32
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB19_13
# BB#12:                                # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB19_13:                              # %cond.end
	movq	8(%rax), %rax
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.LBB19_14:                              # %if.then1
	movabsq	$.L.str12, %rax
	movabsq	$.L__FUNCTION__.dw2_force_const_mem, %rcx
	movq	$740, %rsi              # imm = 0x2E4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp193:
	.size	dw2_force_const_mem, .Ltmp193-dw2_force_const_mem
	.cfi_endproc

	.align	16, 0x90
	.type	mark_indirect_pool,@function
mark_indirect_pool:                     # @mark_indirect_pool
	.cfi_startproc
# BB#0:                                 # %entry
	movq	indirect_pool(%rip), %rdi
	movabsq	$mark_indirect_pool_entry, %rsi
	xorq	%rdx, %rdx
	jmp	splay_tree_foreach  # TAILCALL
.Ltmp194:
	.size	mark_indirect_pool, .Ltmp194-mark_indirect_pool
	.cfi_endproc

	.align	16, 0x90
	.type	mark_indirect_pool_entry,@function
mark_indirect_pool_entry:               # @mark_indirect_pool_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp197:
	.cfi_def_cfa_offset 16
.Ltmp198:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rbx
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_4
# BB#1:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB21_3
# BB#2:                                 # %if.then2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB21_3:                               # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB21_4:                               # %do.end8
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp199:
	.size	mark_indirect_pool_entry, .Ltmp199-mark_indirect_pool_entry
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"0x%llx"
	.size	.L.str, 7

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\t%s "
	.size	.L.str1, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"#"
	.size	.L.str2, 2

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"\t.ascii \""
	.size	.L.str3, 10

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\\%o"
	.size	.L.str4, 4

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\\0\"\t%s "
	.size	.L.str5, 8

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\"\n"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s\""
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\t.string\t"
	.size	.L.str8, 10

	.type	.L.str9,@object         # @.str9
	.section	.rodata,"a",@progbits
.L.str9:
	.asciz	"\001\001\001\001\001\001\001\001btn\001fr\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\000\000\"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"
	.size	.L.str9, 257

	.type	.L.str10,@object        # @.str10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str10:
	.asciz	"\\%03o"
	.size	.L.str10, 6

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"\t.ascii\t"
	.size	.L.str11, 9

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"dwarf2asm.c"
	.size	.L.str12, 12

	.type	.L__FUNCTION__.size_of_encoded_value,@object # @__FUNCTION__.size_of_encoded_value
.L__FUNCTION__.size_of_encoded_value:
	.asciz	"size_of_encoded_value"
	.size	.L__FUNCTION__.size_of_encoded_value, 22

	.type	eh_data_format_name.format_names,@object # @eh_data_format_name.format_names
	.section	.rodata,"a",@progbits
	.align	16
eh_data_format_name.format_names:
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str58
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.quad	.L.str62
	.quad	.L.str63
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str64
	.quad	.L.str65
	.quad	.L.str66
	.quad	.L.str67
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str68
	.quad	.L.str69
	.quad	.L.str70
	.quad	.L.str71
	.quad	.L.str72
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str76
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.quad	.L.str80
	.quad	.L.str81
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str82
	.quad	.L.str83
	.quad	.L.str84
	.quad	.L.str85
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str86
	.quad	.L.str87
	.quad	.L.str88
	.quad	.L.str89
	.quad	.L.str90
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str91
	.quad	.L.str92
	.quad	.L.str93
	.quad	.L.str94
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	.L.str95
	.size	eh_data_format_name.format_names, 2048

	.type	.L.str13,@object        # @.str13
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str13:
	.asciz	"absolute"
	.size	.L.str13, 9

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"uleb128"
	.size	.L.str14, 8

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"udata2"
	.size	.L.str15, 7

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"udata4"
	.size	.L.str16, 7

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"udata8"
	.size	.L.str17, 7

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"sleb128"
	.size	.L.str18, 8

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"sdata2"
	.size	.L.str19, 7

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"sdata4"
	.size	.L.str20, 7

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"sdata8"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"pcrel"
	.size	.L.str22, 6

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"pcrel uleb128"
	.size	.L.str23, 14

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"pcrel udata2"
	.size	.L.str24, 13

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"pcrel udata4"
	.size	.L.str25, 13

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"pcrel udata8"
	.size	.L.str26, 13

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"pcrel sleb128"
	.size	.L.str27, 14

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"pcrel sdata2"
	.size	.L.str28, 13

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"pcrel sdata4"
	.size	.L.str29, 13

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"pcrel sdata8"
	.size	.L.str30, 13

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"textrel"
	.size	.L.str31, 8

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"textrel uleb128"
	.size	.L.str32, 16

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"textrel udata2"
	.size	.L.str33, 15

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"textrel udata4"
	.size	.L.str34, 15

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"textrel udata8"
	.size	.L.str35, 15

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"textrel sleb128"
	.size	.L.str36, 16

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"textrel sdata2"
	.size	.L.str37, 15

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"textrel sdata4"
	.size	.L.str38, 15

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"textrel sdata8"
	.size	.L.str39, 15

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"datarel"
	.size	.L.str40, 8

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"datarel uleb128"
	.size	.L.str41, 16

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"datarel udata2"
	.size	.L.str42, 15

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"datarel udata4"
	.size	.L.str43, 15

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"datarel udata8"
	.size	.L.str44, 15

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"datarel sleb128"
	.size	.L.str45, 16

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"datarel sdata2"
	.size	.L.str46, 15

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"datarel sdata4"
	.size	.L.str47, 15

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"datarel sdata8"
	.size	.L.str48, 15

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"funcrel"
	.size	.L.str49, 8

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"funcrel uleb128"
	.size	.L.str50, 16

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"funcrel udata2"
	.size	.L.str51, 15

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"funcrel udata4"
	.size	.L.str52, 15

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"funcrel udata8"
	.size	.L.str53, 15

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"funcrel sleb128"
	.size	.L.str54, 16

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"funcrel sdata2"
	.size	.L.str55, 15

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"funcrel sdata4"
	.size	.L.str56, 15

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"funcrel sdata8"
	.size	.L.str57, 15

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"aligned absolute"
	.size	.L.str58, 17

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"indirect pcrel"
	.size	.L.str59, 15

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"indirect pcrel uleb128"
	.size	.L.str60, 23

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"indirect pcrel udata2"
	.size	.L.str61, 22

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"indirect pcrel udata4"
	.size	.L.str62, 22

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"indirect pcrel udata8"
	.size	.L.str63, 22

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"indirect pcrel sleb128"
	.size	.L.str64, 23

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"indirect pcrel sdata2"
	.size	.L.str65, 22

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"indirect pcrel sdata4"
	.size	.L.str66, 22

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"indirect pcrel sdata8"
	.size	.L.str67, 22

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"indirect textrel"
	.size	.L.str68, 17

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"indirect textrel uleb128"
	.size	.L.str69, 25

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"indirect textrel udata2"
	.size	.L.str70, 24

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"indirect textrel udata4"
	.size	.L.str71, 24

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"indirect textrel udata8"
	.size	.L.str72, 24

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"indirect textrel sleb128"
	.size	.L.str73, 25

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"indirect textrel sdata2"
	.size	.L.str74, 24

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"indirect textrel sdata4"
	.size	.L.str75, 24

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"indirect textrel sdata8"
	.size	.L.str76, 24

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"indirect datarel"
	.size	.L.str77, 17

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"indirect datarel uleb128"
	.size	.L.str78, 25

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"indirect datarel udata2"
	.size	.L.str79, 24

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"indirect datarel udata4"
	.size	.L.str80, 24

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"indirect datarel udata8"
	.size	.L.str81, 24

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"indirect datarel sleb128"
	.size	.L.str82, 25

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"indirect datarel sdata2"
	.size	.L.str83, 24

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"indirect datarel sdata4"
	.size	.L.str84, 24

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"indirect datarel sdata8"
	.size	.L.str85, 24

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"indirect funcrel"
	.size	.L.str86, 17

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"indirect funcrel uleb128"
	.size	.L.str87, 25

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"indirect funcrel udata2"
	.size	.L.str88, 24

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"indirect funcrel udata4"
	.size	.L.str89, 24

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"indirect funcrel udata8"
	.size	.L.str90, 24

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"indirect funcrel sleb128"
	.size	.L.str91, 25

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"indirect funcrel sdata2"
	.size	.L.str92, 24

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"indirect funcrel sdata4"
	.size	.L.str93, 24

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"indirect funcrel sdata8"
	.size	.L.str94, 24

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"omit"
	.size	.L.str95, 5

	.type	.L__FUNCTION__.eh_data_format_name,@object # @__FUNCTION__.eh_data_format_name
.L__FUNCTION__.eh_data_format_name:
	.asciz	"eh_data_format_name"
	.size	.L__FUNCTION__.eh_data_format_name, 20

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"\t.uleb128 "
	.size	.L.str96, 11

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"\t.sleb128 "
	.size	.L.str97, 11

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"%lld"
	.size	.L.str98, 5

	.type	indirect_pool,@object   # @indirect_pool
	.local	indirect_pool
	.comm	indirect_pool,8,8
	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"\t.long\t"
	.size	.L.str99, 8

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"@GOT"
	.size	.L.str100, 5

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"@GOTOFF"
	.size	.L.str101, 8

	.type	.L__FUNCTION__.dw2_asm_output_encoded_addr_rtx,@object # @__FUNCTION__.dw2_asm_output_encoded_addr_rtx
.L__FUNCTION__.dw2_asm_output_encoded_addr_rtx:
	.asciz	"dw2_asm_output_encoded_addr_rtx"
	.size	.L__FUNCTION__.dw2_asm_output_encoded_addr_rtx, 32

	.type	.L__FUNCTION__.dw2_force_const_mem,@object # @__FUNCTION__.dw2_force_const_mem
.L__FUNCTION__.dw2_force_const_mem:
	.asciz	"dw2_force_const_mem"
	.size	.L__FUNCTION__.dw2_force_const_mem, 20

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"DW.ref.%s"
	.size	.L.str102, 10

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"\t.hidden DW.ref.%s\n"
	.size	.L.str103, 20


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
