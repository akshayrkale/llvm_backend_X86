	.file	"diagnostic.c.bc"
	.text
	.globl	default_print_error_function
	.align	16, 0x90
	.type	default_print_error_function,@function
default_print_error_function:           # @default_print_error_function
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
	subq	$88, %rsp
.Ltmp9:
	.cfi_def_cfa_offset 128
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	last_error_function(%rip), %rax
	movq	current_function_decl(%rip), %rcx
	movq	%rdi, %rbx
	cmpq	%rcx, %rax
	je	.LBB0_24
# BB#1:                                 # %if.then
	xorq	%r14, %r14
	testq	%rsi, %rsi
	je	.LBB0_3
# BB#2:                                 # %cond.true
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	build_message_string
	movq	%rax, %r14
.LBB0_3:                                # %cond.end
	leaq	8(%rsp), %rdi
	movq	$80, %rdx
	movq	%rbx, %rsi
	callq	memcpy
	movq	%r14, (%rbx)
	movslq	12(%rbx), %r15
	testq	%r15, %r15
	jle	.LBB0_9
# BB#4:                                 # %lor.lhs.false.i.i
	movl	24(%rbx), %eax
	cmpq	$1, %rax
	jbe	.LBB0_9
# BB#5:                                 # %if.else.i.i23
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB0_7
# BB#6:                                 # %cond.true.i.i
	movq	%r14, %rdi
	callq	strlen
.LBB0_7:                                # %cond.end.i.i
	movq	%r15, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$31, %rdx
	jg	.LBB0_9
# BB#8:                                 # %if.then16.i.i
	addq	$32, %r15
.LBB0_9:                                # %if.else21.i.i
	movl	%r15d, 8(%rbx)
	xorq	%rax, %rax
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	movq	current_function_decl(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_10
# BB#11:                                # %if.else
	movq	8(%rdi), %rax
	movzbl	16(%rax), %r15d
	movq	$2, %rsi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rcx
	cmpq	$16, %r15
	jne	.LBB0_13
# BB#12:                                # %if.then6
	movabsq	$.L.str11, %rsi
	jmp	.LBB0_14
.LBB0_10:                               # %if.then3
	movabsq	$.L.str10, %rsi
	movq	%rbx, %rdi
	callq	output_add_string
	jmp	.LBB0_15
.LBB0_13:                               # %if.else8
	movabsq	$.L.str12, %rsi
.LBB0_14:                               # %if.end10
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	output_printf
.LBB0_15:                               # %if.end10
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	88(%rbx), %r15
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB0_17
# BB#16:                                # %if.then.i
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB0_17:                               # %output_add_newline.exit
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%r12, %r12
	movl	%r12d, 176(%rbx)
	movq	current_function_decl(%rip), %rax
	movq	%rax, last_error_function(%rip)
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB0_19
# BB#18:                                # %if.then.i.i
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB0_19:                               # %output_finalize_message.exit.i
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movb	%r12b, (%rax)
	movq	80(%rbx), %rsi
	movq	104(%rbx), %rdi
	callq	fputs
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB0_22
# BB#20:                                # %land.lhs.true.i.i
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB0_22
# BB#21:                                # %if.then.i9.i
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB0_23
.LBB0_22:                               # %if.else.i.i
	movq	%r15, %rdi
	callq	obstack_free
.LBB0_23:                               # %output_buffer_to_stream.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	leaq	8(%rsp), %rsi
	movq	$80, %rdx
	movq	%rbx, %rdi
	callq	memcpy
	movq	%r14, %rdi
	callq	free
.LBB0_24:                               # %if.end12
	addq	$88, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	default_print_error_function, .Ltmp14-default_print_error_function
	.cfi_endproc

	.globl	error_module_changed
	.align	16, 0x90
	.type	error_module_changed,@function
error_module_changed:                   # @error_module_changed
	.cfi_startproc
# BB#0:                                 # %entry
	movl	last_error_tick(%rip), %eax
	movl	input_file_stack_tick(%rip), %ecx
	cmpq	%rcx, %rax
	je	.LBB1_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB1_1:
	movabsq	$0, %rax
	retq
.Ltmp15:
	.size	error_module_changed, .Ltmp15-error_module_changed
	.cfi_endproc

	.globl	record_last_error_module
	.align	16, 0x90
	.type	record_last_error_module,@function
record_last_error_module:               # @record_last_error_module
	.cfi_startproc
# BB#0:                                 # %entry
	movl	input_file_stack_tick(%rip), %eax
	movl	%eax, last_error_tick(%rip)
	retq
.Ltmp16:
	.size	record_last_error_module, .Ltmp16-record_last_error_module
	.cfi_endproc

	.globl	error_function_changed
	.align	16, 0x90
	.type	error_function_changed,@function
error_function_changed:                 # @error_function_changed
	.cfi_startproc
# BB#0:                                 # %entry
	movq	last_error_function(%rip), %rax
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB3_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB3_1:
	movabsq	$0, %rax
	retq
.Ltmp17:
	.size	error_function_changed, .Ltmp17-error_function_changed
	.cfi_endproc

	.globl	record_last_error_function
	.align	16, 0x90
	.type	record_last_error_function,@function
record_last_error_function:             # @record_last_error_function
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_function_decl(%rip), %rax
	movq	%rax, last_error_function(%rip)
	retq
.Ltmp18:
	.size	record_last_error_function, .Ltmp18-record_last_error_function
	.cfi_endproc

	.globl	diagnostic_initialize
	.align	16, 0x90
	.type	diagnostic_initialize,@function
diagnostic_initialize:                  # @diagnostic_initialize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp24:
	.cfi_def_cfa_offset 32
.Ltmp25:
	.cfi_offset %rbx, -24
.Ltmp26:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	xorq	%r14, %r14
                                        # kill: RDI<def> RBX<kill>
	callq	memset
	leaq	88(%rbx), %rdi
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	_obstack_begin
	movq	stderr(%rip), %rax
	movq	%rax, 80(%rbx)
	movl	%r14d, 24(%rbx)
	movq	$default_diagnostic_starter, 352(%rbx)
	movq	$default_diagnostic_finalizer, 360(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp27:
	.size	diagnostic_initialize, .Ltmp27-diagnostic_initialize
	.cfi_endproc

	.align	16, 0x90
	.type	default_diagnostic_starter,@function
default_diagnostic_starter:             # @default_diagnostic_starter
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 32
.Ltmp34:
	.cfi_offset %rbx, -24
.Ltmp35:
	.cfi_offset %r14, -16
	movq	global_dc(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	report_problematic_module
	movq	global_dc(%rip), %rdi
	movq	input_filename(%rip), %rsi
	callq	*print_error_function(%rip)
	movq	336(%rbx), %rsi
	movl	348(%rbx), %eax
	testq	%rsi, %rsi
	je	.LBB6_5
# BB#1:                                 # %if.then.i
	movl	344(%rbx), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_4
# BB#2:                                 # %if.then2.i
	movabsq	$.L.str1, %rdi
	jmp	.LBB6_3
.LBB6_5:                                # %if.else4.i
	movq	progname(%rip), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_7
# BB#6:                                 # %if.then6.i
	movabsq	$.L.str3, %rdi
	jmp	.LBB6_8
.LBB6_4:                                # %if.else.i
	movabsq	$.L.str2, %rdi
.LBB6_3:                                # %context_as_prefix.exit
	xorq	%rax, %rax
	callq	build_message_string
	jmp	.LBB6_9
.LBB6_7:                                # %if.else8.i
	movabsq	$.L.str4, %rdi
.LBB6_8:                                # %context_as_prefix.exit
	xorq	%rax, %rax
	callq	build_message_string
.LBB6_9:                                # %context_as_prefix.exit
	movq	%rax, (%r14)
	movslq	12(%r14), %rbx
	testq	%rbx, %rbx
	jle	.LBB6_15
# BB#10:                                # %lor.lhs.false.i.i
	movl	24(%r14), %ecx
	cmpq	$1, %rcx
	jbe	.LBB6_15
# BB#11:                                # %if.else.i.i
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB6_13
# BB#12:                                # %cond.true.i.i
	movq	%rax, %rdi
	callq	strlen
	movq	%rax, %rcx
.LBB6_13:                               # %cond.end.i.i
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$31, %rax
	jg	.LBB6_15
# BB#14:                                # %if.then16.i.i
	addq	$32, %rbx
.LBB6_15:                               # %if.else21.i.i
	movl	%ebx, 8(%r14)
	xorq	%rax, %rax
	movb	%al, 20(%r14)
	movl	%eax, 16(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp36:
	.size	default_diagnostic_starter, .Ltmp36-default_diagnostic_starter
	.cfi_endproc

	.align	16, 0x90
	.type	default_diagnostic_finalizer,@function
default_diagnostic_finalizer:           # @default_diagnostic_finalizer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 16
.Ltmp40:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB7_2
# BB#1:                                 # %if.then.i
	callq	free
	movq	$0, (%rbx)
.LBB7_2:                                # %output_destroy_prefix.exit
	popq	%rbx
	retq
.Ltmp41:
	.size	default_diagnostic_finalizer, .Ltmp41-default_diagnostic_finalizer
	.cfi_endproc

	.globl	output_is_line_wrapping
	.align	16, 0x90
	.type	output_is_line_wrapping,@function
output_is_line_wrapping:                # @output_is_line_wrapping
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	12(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB8_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB8_1:
	movabsq	$0, %rax
	retq
.Ltmp42:
	.size	output_is_line_wrapping, .Ltmp42-output_is_line_wrapping
	.cfi_endproc

	.globl	output_get_prefix
	.align	16, 0x90
	.type	output_get_prefix,@function
output_get_prefix:                      # @output_get_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	retq
.Ltmp43:
	.size	output_get_prefix, .Ltmp43-output_get_prefix
	.cfi_endproc

	.globl	output_set_maximum_length
	.align	16, 0x90
	.type	output_set_maximum_length,@function
output_set_maximum_length:              # @output_set_maximum_length
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp49:
	.cfi_def_cfa_offset 32
.Ltmp50:
	.cfi_offset %rbx, -24
.Ltmp51:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movl	%ebx, 12(%r14)
	jle	.LBB10_6
# BB#1:                                 # %lor.lhs.false.i
	movl	24(%r14), %eax
	cmpq	$1, %rax
	jbe	.LBB10_6
# BB#2:                                 # %if.else.i
	movq	(%r14), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB10_4
# BB#3:                                 # %cond.true.i
	callq	strlen
.LBB10_4:                               # %cond.end.i
	movq	%rbx, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$31, %rdx
	jg	.LBB10_6
# BB#5:                                 # %if.then16.i
	leaq	32(%rbx), %rax
	movl	%eax, 8(%r14)
	jmp	.LBB10_7
.LBB10_6:                               # %if.else21.i
	movl	%ebx, 8(%r14)
.LBB10_7:                               # %set_real_maximum_length.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp52:
	.size	output_set_maximum_length, .Ltmp52-output_set_maximum_length
	.cfi_endproc

	.globl	output_set_prefix
	.align	16, 0x90
	.type	output_set_prefix,@function
output_set_prefix:                      # @output_set_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp58:
	.cfi_def_cfa_offset 32
.Ltmp59:
	.cfi_offset %rbx, -24
.Ltmp60:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	%rsi, (%rbx)
	movslq	12(%rbx), %r14
	testq	%r14, %r14
	jle	.LBB11_6
# BB#1:                                 # %lor.lhs.false.i
	movl	24(%rbx), %eax
	cmpq	$1, %rax
	jbe	.LBB11_6
# BB#2:                                 # %if.else.i
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB11_4
# BB#3:                                 # %cond.true.i
	movq	%rsi, %rdi
	callq	strlen
.LBB11_4:                               # %cond.end.i
	movq	%r14, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$31, %rdx
	jg	.LBB11_6
# BB#5:                                 # %if.then16.i
	addq	$32, %r14
.LBB11_6:                               # %if.else21.i
	movl	%r14d, 8(%rbx)
	xorq	%rax, %rax
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp61:
	.size	output_set_prefix, .Ltmp61-output_set_prefix
	.cfi_endproc

	.globl	output_last_position
	.align	16, 0x90
	.type	output_last_position,@function
output_last_position:                   # @output_last_position
	.cfi_startproc
# BB#0:                                 # %entry
	movq	104(%rdi), %rdx
	movq	112(%rdi), %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	je	.LBB12_2
# BB#1:                                 # %select.mid
	decq	%rcx
	movq	%rcx, %rax
.LBB12_2:                               # %select.end
	retq
.Ltmp62:
	.size	output_last_position, .Ltmp62-output_last_position
	.cfi_endproc

	.globl	output_destroy_prefix
	.align	16, 0x90
	.type	output_destroy_prefix,@function
output_destroy_prefix:                  # @output_destroy_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 16
.Ltmp66:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB13_2
# BB#1:                                 # %if.then
	callq	free
	movq	$0, (%rbx)
.LBB13_2:                               # %if.end
	popq	%rbx
	retq
.Ltmp67:
	.size	output_destroy_prefix, .Ltmp67-output_destroy_prefix
	.cfi_endproc

	.globl	output_clear_message_text
	.align	16, 0x90
	.type	output_clear_message_text,@function
output_clear_message_text:              # @output_clear_message_text
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 16
.Ltmp71:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB14_3
# BB#1:                                 # %land.lhs.true
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB14_3
# BB#2:                                 # %if.then
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB14_4
.LBB14_3:                               # %if.else
	leaq	88(%rbx), %rdi
	callq	obstack_free
.LBB14_4:                               # %if.end
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	popq	%rbx
	retq
.Ltmp72:
	.size	output_clear_message_text, .Ltmp72-output_clear_message_text
	.cfi_endproc

	.globl	init_output_buffer
	.align	16, 0x90
	.type	init_output_buffer,@function
init_output_buffer:                     # @init_output_buffer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp82:
	.cfi_def_cfa_offset 48
.Ltmp83:
	.cfi_offset %rbx, -40
.Ltmp84:
	.cfi_offset %r12, -32
.Ltmp85:
	.cfi_offset %r14, -24
.Ltmp86:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	xorq	%rsi, %rsi
	movq	$320, %rdx              # imm = 0x140
                                        # kill: RDI<def> RBX<kill>
	callq	memset
	leaq	88(%rbx), %rdi
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	_obstack_begin
	movq	stderr(%rip), %rax
	movq	%rax, 80(%rbx)
	movl	%r14d, 12(%rbx)
	movq	global_dc(%rip), %rax
	movl	24(%rax), %eax
	testq	%r12, %r12
	movl	%eax, 24(%rbx)
	movq	%r15, (%rbx)
	jle	.LBB15_6
# BB#1:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jbe	.LBB15_6
# BB#2:                                 # %if.else.i.i
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB15_4
# BB#3:                                 # %cond.true.i.i
	movq	%r15, %rdi
	callq	strlen
.LBB15_4:                               # %cond.end.i.i
	movq	%r14, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$31, %rdx
	jg	.LBB15_6
# BB#5:                                 # %if.then16.i.i
	leaq	32(%r14), %rax
	movl	%eax, 8(%rbx)
	jmp	.LBB15_7
.LBB15_6:                               # %if.else21.i.i
	movl	%r14d, 8(%rbx)
.LBB15_7:                               # %output_set_prefix.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 32(%rbx)
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp87:
	.size	init_output_buffer, .Ltmp87-init_output_buffer
	.cfi_endproc

	.globl	output_clear
	.align	16, 0x90
	.type	output_clear,@function
output_clear:                           # @output_clear
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 16
.Ltmp91:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB16_3
# BB#1:                                 # %land.lhs.true.i
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB16_3
# BB#2:                                 # %if.then.i
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB16_4
.LBB16_3:                               # %if.else.i
	leaq	88(%rbx), %rdi
	callq	obstack_free
.LBB16_4:                               # %output_clear_message_text.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 32(%rbx)
	popq	%rbx
	retq
.Ltmp92:
	.size	output_clear, .Ltmp92-output_clear
	.cfi_endproc

	.globl	output_finalize_message
	.align	16, 0x90
	.type	output_finalize_message,@function
output_finalize_message:                # @output_finalize_message
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 16
.Ltmp96:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB17_2
# BB#1:                                 # %if.then
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB17_2:                               # %if.end
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	104(%rbx), %rax
	popq	%rbx
	retq
.Ltmp97:
	.size	output_finalize_message, .Ltmp97-output_finalize_message
	.cfi_endproc

	.globl	flush_diagnostic_buffer
	.align	16, 0x90
	.type	flush_diagnostic_buffer,@function
flush_diagnostic_buffer:                # @flush_diagnostic_buffer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp103:
	.cfi_def_cfa_offset 32
.Ltmp104:
	.cfi_offset %rbx, -24
.Ltmp105:
	.cfi_offset %r14, -16
	movq	diagnostic_buffer(%rip), %rbx
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	88(%rbx), %r14
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB18_2
# BB#1:                                 # %if.then.i.i
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB18_2:                               # %output_finalize_message.exit.i
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	80(%rbx), %rsi
	movq	104(%rbx), %rdi
	callq	fputs
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB18_5
# BB#3:                                 # %land.lhs.true.i.i
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB18_5
# BB#4:                                 # %if.then.i9.i
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB18_6
.LBB18_5:                               # %if.else.i.i
	movq	%r14, %rdi
	callq	obstack_free
.LBB18_6:                               # %output_buffer_to_stream.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	movq	diagnostic_buffer(%rip), %rax
	movq	80(%rax), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fflush  # TAILCALL
.Ltmp106:
	.size	flush_diagnostic_buffer, .Ltmp106-flush_diagnostic_buffer
	.cfi_endproc

	.globl	output_space_left
	.align	16, 0x90
	.type	output_space_left,@function
output_space_left:                      # @output_space_left
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdi), %eax
	movl	176(%rdi), %ecx
	subq	%rcx, %rax
	retq
.Ltmp107:
	.size	output_space_left, .Ltmp107-output_space_left
	.cfi_endproc

	.globl	output_emit_prefix
	.align	16, 0x90
	.type	output_emit_prefix,@function
output_emit_prefix:                     # @output_emit_prefix
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
.Ltmp119:
	.cfi_offset %rbx, -48
.Ltmp120:
	.cfi_offset %r12, -40
.Ltmp121:
	.cfi_offset %r13, -32
.Ltmp122:
	.cfi_offset %r14, -24
.Ltmp123:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %r14
	testq	%r14, %r14
	je	.LBB20_18
# BB#1:                                 # %if.then
	movl	24(%rbx), %eax
	cmpq	$2, %rax
	je	.LBB20_15
# BB#2:                                 # %if.then
	testq	%rax, %rax
	jne	.LBB20_18
# BB#3:                                 # %sw.bb
	movzbl	20(%rbx), %eax
	movl	16(%rbx), %r15d
	testq	%rax, %rax
	je	.LBB20_14
# BB#4:                                 # %if.then3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB20_18
# BB#5:                                 # %for.body.lr.ph.i
	leaq	88(%rbx), %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB20_6:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	12(%rbx), %rax
	testq	%rax, %rax
	jle	.LBB20_11
# BB#7:                                 # %land.lhs.true.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	movl	8(%rbx), %eax
	movl	176(%rbx), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB20_11
# BB#8:                                 # %if.then.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB20_10
# BB#9:                                 # %if.then.i.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB20_10:                              # %output_add_newline.exit.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
.LBB20_11:                              # %if.end.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB20_13
# BB#12:                                # %if.then3.i.i
                                        #   in Loop: Header=BB20_6 Depth=1
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB20_13:                              # %output_add_character.exit.i
                                        #   in Loop: Header=BB20_6 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$32, %rcx
	movb	%cl, (%rax)
	movl	176(%rbx), %eax
	incq	%rax
	movl	%eax, 176(%rbx)
	incq	%r12
	movq	%r12, %rax
	andq	%r13, %rax
	movq	%r15, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB20_6
	jmp	.LBB20_18
.LBB20_14:                              # %if.end
	addq	$3, %r15
	movl	%r15d, 16(%rbx)
.LBB20_15:                              # %sw.bb5
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	112(%rbx), %rdi
	movq	120(%rbx), %rax
	movq	$32, %rcx
	movq	%r15, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	leaq	(%rdi,%r12), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB20_17
# BB#16:                                # %if.then.i
	leaq	88(%rbx), %rdi
	movq	%r15, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rdi
.LBB20_17:                              # %output_append_r.exit
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	memcpy
	addq	%r12, 112(%rbx)
	movl	176(%rbx), %eax
	addq	%r15, %rax
	movl	%eax, 176(%rbx)
	movq	$1, %rax
	movb	%al, 20(%rbx)
.LBB20_18:                              # %if.end12
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp124:
	.size	output_emit_prefix, .Ltmp124-output_emit_prefix
	.cfi_endproc

	.globl	output_add_newline
	.align	16, 0x90
	.type	output_add_newline,@function
output_add_newline:                     # @output_add_newline
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp127:
	.cfi_def_cfa_offset 16
.Ltmp128:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB21_2
# BB#1:                                 # %if.then
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB21_2:                               # %if.end
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	popq	%rbx
	retq
.Ltmp129:
	.size	output_add_newline, .Ltmp129-output_add_newline
	.cfi_endproc

	.globl	output_add_character
	.align	16, 0x90
	.type	output_add_character,@function
output_add_character:                   # @output_add_character
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp135:
	.cfi_def_cfa_offset 32
.Ltmp136:
	.cfi_offset %rbx, -24
.Ltmp137:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movslq	12(%rbx), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	jle	.LBB22_5
# BB#1:                                 # %land.lhs.true
	movl	8(%rbx), %eax
	movl	176(%rbx), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB22_5
# BB#2:                                 # %if.then
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB22_4
# BB#3:                                 # %if.then.i
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB22_4:                               # %output_add_newline.exit
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
.LBB22_5:                               # %if.end
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB22_7
# BB#6:                                 # %if.then3
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB22_7:                               # %if.end4
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movb	%r14b, (%rax)
	movl	176(%rbx), %eax
	incq	%rax
	movl	%eax, 176(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp138:
	.size	output_add_character, .Ltmp138-output_add_character
	.cfi_endproc

	.globl	output_add_space
	.align	16, 0x90
	.type	output_add_space,@function
output_add_space:                       # @output_add_space
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp141:
	.cfi_def_cfa_offset 16
.Ltmp142:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movslq	12(%rbx), %rax
	testq	%rax, %rax
	jle	.LBB23_5
# BB#1:                                 # %land.lhs.true
	movl	8(%rbx), %eax
	movl	176(%rbx), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB23_2
.LBB23_5:                               # %if.end
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB23_7
# BB#6:                                 # %if.then3
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB23_7:                               # %if.end4
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$32, %rcx
	movb	%cl, (%rax)
	movl	176(%rbx), %eax
	incq	%rax
	jmp	.LBB23_8
.LBB23_2:                               # %if.then
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB23_4
# BB#3:                                 # %if.then.i
	leaq	88(%rbx), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB23_4:                               # %output_add_newline.exit
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
.LBB23_8:                               # %return
	movl	%eax, 176(%rbx)
	popq	%rbx
	retq
.Ltmp143:
	.size	output_add_space, .Ltmp143-output_add_space
	.cfi_endproc

	.globl	output_decimal
	.align	16, 0x90
	.type	output_decimal,@function
output_decimal:                         # @output_decimal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp149:
	.cfi_def_cfa_offset 32
.Ltmp150:
	.cfi_offset %rbx, -24
.Ltmp151:
	.cfi_offset %r14, -16
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	leaq	180(%rbx), %r14
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	sprintf
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	output_add_string  # TAILCALL
.Ltmp152:
	.size	output_decimal, .Ltmp152-output_decimal
	.cfi_endproc

	.globl	output_add_string
	.align	16, 0x90
	.type	output_add_string,@function
output_add_string:                      # @output_add_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp158:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp159:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp160:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp162:
	.cfi_def_cfa_offset 48
.Ltmp163:
	.cfi_offset %rbx, -40
.Ltmp164:
	.cfi_offset %r12, -32
.Ltmp165:
	.cfi_offset %r14, -24
.Ltmp166:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB25_2
# BB#1:                                 # %cond.true
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %rbx
.LBB25_2:                               # %cond.end
	movslq	12(%r14), %rax
	leaq	(%r12,%rbx), %r15
	testq	%rax, %rax
	jle	.LBB25_3
# BB#12:                                # %if.then.i
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	wrap_text  # TAILCALL
.LBB25_3:                               # %if.else.i
	movl	176(%r14), %eax
	testq	%rax, %rax
	jne	.LBB25_9
# BB#4:                                 # %if.then.i.i
	movq	%r14, %rdi
	callq	output_emit_prefix
	movslq	12(%r14), %rax
	testq	%rax, %rax
	jle	.LBB25_9
# BB#5:                                 # %if.then.i.i
	testq	%rbx, %rbx
	je	.LBB25_9
	.align	16, 0x90
.LBB25_6:                               # %land.rhs.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB25_9
# BB#7:                                 # %while.cond.i.i
                                        #   in Loop: Header=BB25_6 Depth=1
	incq	%r12
	decq	%rbx
	jne	.LBB25_6
# BB#8:
	movq	%r15, %r12
.LBB25_9:                               # %if.end5.i.i
	subq	%r12, %r15
	movq	112(%r14), %rdi
	movq	120(%r14), %rax
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rdi,%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB25_11
# BB#10:                                # %if.then.i.i.i
	leaq	88(%r14), %rdi
	movq	%r15, %rsi
	callq	_obstack_newchunk
	movq	112(%r14), %rdi
.LBB25_11:                              # %maybe_wrap_text.exit
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	addq	%rbx, 112(%r14)
	movl	176(%r14), %eax
	addq	%r15, %rax
	movl	%eax, 176(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp167:
	.size	output_add_string, .Ltmp167-output_add_string
	.cfi_endproc

	.globl	output_append
	.align	16, 0x90
	.type	output_append,@function
output_append:                          # @output_append
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp177:
	.cfi_def_cfa_offset 48
.Ltmp178:
	.cfi_offset %rbx, -40
.Ltmp179:
	.cfi_offset %r12, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	176(%r14), %eax
	movq	%rdx, %r15
	movq	%rsi, %rbx
	testq	%rax, %rax
	jne	.LBB26_6
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	callq	output_emit_prefix
	movslq	12(%r14), %rax
	testq	%rax, %rax
	jle	.LBB26_6
# BB#2:                                 # %if.then
	cmpq	%r15, %rbx
	je	.LBB26_6
	.align	16, 0x90
.LBB26_3:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB26_6
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB26_3 Depth=1
	incq	%rbx
	cmpq	%rbx, %r15
	jne	.LBB26_3
# BB#5:
	movq	%r15, %rbx
.LBB26_6:                               # %if.end5
	subq	%rbx, %r15
	movq	112(%r14), %rdi
	movq	120(%r14), %rax
	movq	$32, %rcx
	movq	%r15, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	leaq	(%rdi,%r12), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB26_8
# BB#7:                                 # %if.then.i
	leaq	88(%r14), %rdi
	movq	%r15, %rsi
	callq	_obstack_newchunk
	movq	112(%r14), %rdi
.LBB26_8:                               # %output_append_r.exit
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	memcpy
	addq	%r12, 112(%r14)
	movl	176(%r14), %eax
	addq	%r15, %rax
	movl	%eax, 176(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp182:
	.size	output_append, .Ltmp182-output_append
	.cfi_endproc

	.globl	context_as_prefix
	.align	16, 0x90
	.type	context_as_prefix,@function
context_as_prefix:                      # @context_as_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rcx
	movq	%rdi, %rsi
	testq	%rsi, %rsi
	je	.LBB27_5
# BB#1:                                 # %if.then
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB27_4
# BB#2:                                 # %if.then2
	movabsq	$.L.str1, %rdi
	jmp	.LBB27_3
.LBB27_5:                               # %if.else4
	movq	progname(%rip), %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB27_7
# BB#6:                                 # %if.then6
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	jmp	build_message_string  # TAILCALL
.LBB27_4:                               # %if.else
	movabsq	$.L.str2, %rdi
.LBB27_3:                               # %if.then2
	xorq	%rax, %rax
	movq	%rcx, %rdx
	jmp	build_message_string  # TAILCALL
.LBB27_7:                               # %if.else8
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	jmp	build_message_string  # TAILCALL
.Ltmp183:
	.size	context_as_prefix, .Ltmp183-context_as_prefix
	.cfi_endproc

	.align	16, 0x90
	.type	build_message_string,@function
build_message_string:                   # @build_message_string
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$88, %rsp
.Ltmp185:
	.cfi_def_cfa_offset 96
	movq	%rdi, %rax
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	96(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	80(%rsp), %rdi
	leaq	48(%rsp), %rdx
	movq	%rax, %rsi
	callq	vasprintf
	movq	80(%rsp), %rax
	addq	$88, %rsp
	retq
.Ltmp186:
	.size	build_message_string, .Ltmp186-build_message_string
	.cfi_endproc

	.globl	file_name_as_prefix
	.align	16, 0x90
	.type	file_name_as_prefix,@function
file_name_as_prefix:                    # @file_name_as_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str4, %rcx
	xorq	%rax, %rax
	movq	%rdi, %rdx
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	jmp	build_message_string  # TAILCALL
.Ltmp187:
	.size	file_name_as_prefix, .Ltmp187-file_name_as_prefix
	.cfi_endproc

	.globl	output_printf
	.align	16, 0x90
	.type	output_printf,@function
output_printf:                          # @output_printf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp192:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp193:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp194:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp195:
	.cfi_def_cfa_offset 112
.Ltmp196:
	.cfi_offset %rbx, -32
.Ltmp197:
	.cfi_offset %r14, -24
.Ltmp198:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movq	40(%r14), %r15
	leaq	48(%rsp), %rdx
	movq	%rdx, 40(%r14)
	leaq	72(%rsp), %rdi
	callq	vasprintf
	movq	72(%rsp), %rbx
	movq	%rbx, %rdi
	callq	strlen
	addq	%rbx, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	wrap_text
	movq	%rbx, %rdi
	callq	free
	movq	%r15, 40(%r14)
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp199:
	.size	output_printf, .Ltmp199-output_printf
	.cfi_endproc

	.globl	count_error
	.align	16, 0x90
	.type	count_error,@function
count_error:                            # @count_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp201:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	je	.LBB31_8
# BB#1:                                 # %land.lhs.true
	movl	inhibit_warnings(%rip), %edx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jne	.LBB31_10
# BB#2:                                 # %land.lhs.true2
	movl	in_system_header(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB31_4
# BB#3:                                 # %land.lhs.true2
	movl	warn_system_headers(%rip), %edx
	testq	%rcx, %rdx
	je	.LBB31_10
.LBB31_4:                               # %if.end
	movl	warnings_are_errors(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_5
# BB#6:                                 # %if.else
	movzbl	count_error.warning_message(%rip), %eax
	testq	%rax, %rax
	jne	.LBB31_8
# BB#7:                                 # %if.then13
	movq	progname(%rip), %rsi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	verbatim
	movq	$1, %rax
	movb	%al, count_error.warning_message(%rip)
.LBB31_8:                               # %if.end14
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
.LBB31_9:                               # %return
	movq	$1, %rax
.LBB31_10:                              # %return
	popq	%rdx
	retq
.LBB31_5:                               # %if.then9
	movq	global_dc(%rip), %rax
	movl	64(%rax), %ecx
	incq	%rcx
	movl	%ecx, 64(%rax)
	jmp	.LBB31_9
.Ltmp202:
	.size	count_error, .Ltmp202-count_error
	.cfi_endproc

	.globl	verbatim
	.align	16, 0x90
	.type	verbatim,@function
verbatim:                               # @verbatim
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp207:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp208:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp209:
	.cfi_def_cfa_offset 32
	subq	$160, %rsp
.Ltmp210:
	.cfi_def_cfa_offset 192
.Ltmp211:
	.cfi_offset %rbx, -32
.Ltmp212:
	.cfi_offset %r14, -24
.Ltmp213:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	192(%rsp), %rax
	movq	diagnostic_buffer(%rip), %rbx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	80(%rsp), %r15
	movq	$80, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	memcpy
	movq	$0, (%rbx)
	movq	$1, %rax
	movl	%eax, 24(%rbx)
	movq	%r14, 32(%rbx)
	leaq	48(%rsp), %rax
	movq	%rax, 40(%rbx)
	movq	$0, 8(%rbx)
	movq	%rbx, %rdi
	callq	output_format
	movq	$80, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	memcpy
	movq	diagnostic_buffer(%rip), %rbx
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	88(%rbx), %r14
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB32_2
# BB#1:                                 # %if.then.i.i
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB32_2:                               # %output_finalize_message.exit.i
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	80(%rbx), %rsi
	movq	104(%rbx), %rdi
	callq	fputs
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB32_5
# BB#3:                                 # %land.lhs.true.i.i
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB32_5
# BB#4:                                 # %if.then.i9.i
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB32_6
.LBB32_5:                               # %if.else.i.i
	movq	%r14, %rdi
	callq	obstack_free
.LBB32_6:                               # %output_buffer_to_stream.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	addq	$160, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp214:
	.size	verbatim, .Ltmp214-verbatim
	.cfi_endproc

	.globl	fnotice
	.align	16, 0x90
	.type	fnotice,@function
fnotice:                                # @fnotice
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp216:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	48(%rsp), %rdx
	callq	vfprintf
	addq	$72, %rsp
	retq
.Ltmp217:
	.size	fnotice, .Ltmp217-fnotice
	.cfi_endproc

	.globl	fatal_io_error
	.align	16, 0x90
	.type	fatal_io_error,@function
fatal_io_error:                         # @fatal_io_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp223:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp224:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp225:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 40
	subq	$152, %rsp
.Ltmp227:
	.cfi_def_cfa_offset 192
.Ltmp228:
	.cfi_offset %rbx, -40
.Ltmp229:
	.cfi_offset %r12, -32
.Ltmp230:
	.cfi_offset %r14, -24
.Ltmp231:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	192(%rsp), %rax
	movq	diagnostic_buffer(%rip), %rbx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	72(%rsp), %r14
	movq	$80, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	memcpy
	movq	progname(%rip), %r12
	callq	__errno_location
	movl	(%rax), %edi
	callq	xstrerror
	movq	%rax, %rcx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	output_printf
	movq	diagnostic_buffer(%rip), %rax
	leaq	48(%rsp), %rcx
	movq	%rcx, 40(%rax)
	movq	diagnostic_buffer(%rip), %rax
	movq	%r15, 32(%rax)
	movq	diagnostic_buffer(%rip), %rdi
	callq	output_format
	movq	global_dc(%rip), %rdi
	callq	diagnostic_finish
	movq	diagnostic_buffer(%rip), %rdi
	movq	$80, %rdx
	movq	%r14, %rsi
	callq	memcpy
	movq	$1, %rdi
	callq	exit
.Ltmp232:
	.size	fatal_io_error, .Ltmp232-fatal_io_error
	.cfi_endproc

	.align	16, 0x90
	.type	output_format,@function
output_format:                          # @output_format
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp239:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp240:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp241:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp242:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp243:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp244:
	.cfi_def_cfa_offset 80
.Ltmp245:
	.cfi_offset %rbx, -48
.Ltmp246:
	.cfi_offset %r12, -40
.Ltmp247:
	.cfi_offset %r13, -32
.Ltmp248:
	.cfi_offset %r14, -24
.Ltmp249:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %rsi
	movzbl	(%rsi), %eax
	testq	%rax, %rax
	je	.LBB35_108
# BB#1:                                 # %while.cond.preheader.lr.ph
	leaq	88(%r14), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	leaq	180(%r14), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$37, %r13
	.align	16, 0x90
.LBB35_2:                               # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_3 Depth 2
                                        #     Child Loop BB35_101 Depth 2
	leaq	1(%rsi), %r15
	jmp	.LBB35_3
	.align	16, 0x90
.LBB35_5:                               # %while.body
                                        #   in Loop: Header=BB35_3 Depth=2
	movzbl	(%r15), %eax
	incq	%r15
.LBB35_3:                               # %while.cond
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	$255, %rax
	je	.LBB35_6
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB35_3 Depth=2
	andq	$255, %rax
	cmpq	$37, %rax
	jne	.LBB35_5
	.align	16, 0x90
.LBB35_6:                               # %while.end
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	-1(%r15), %rbx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	wrap_text
	movq	%rbx, 32(%r14)
	movzbl	-1(%r15), %eax
	testq	%rax, %rax
	je	.LBB35_108
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r15, 32(%r14)
	movzbl	(%r15), %edx
	xorq	%rax, %rax
	cmpq	$108, %rdx
	jne	.LBB35_9
# BB#8:                                 # %if.then19
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%r15), %rcx
	movq	%rcx, 32(%r14)
	movzbl	1(%r15), %edx
	movq	$1, %rax
	movq	%rcx, %r15
.LBB35_9:                               # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$45, %rdx
	jg	.LBB35_19
# BB#10:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$37, %rdx
	jne	.LBB35_28
# BB#11:                                # %sw.bb164
                                        #   in Loop: Header=BB35_2 Depth=1
	movslq	12(%r14), %rax
	testq	%rax, %rax
	jle	.LBB35_16
# BB#12:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	8(%r14), %eax
	movl	176(%r14), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB35_16
# BB#13:                                # %if.then.i261
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	112(%r14), %rax
	movq	120(%r14), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB35_15
# BB#14:                                # %if.then.i.i264
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$1, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r14), %rax
.LBB35_15:                              # %output_add_newline.exit.i
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r14)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%r14)
.LBB35_16:                              # %if.end.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	112(%r14), %rax
	movq	120(%r14), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB35_18
# BB#17:                                # %if.then3.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$1, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r14), %rax
.LBB35_18:                              # %output_add_character.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r14)
	movb	%r13b, (%rax)
.LBB35_53:                              # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	176(%r14), %eax
	incq	%rax
	movl	%eax, 176(%r14)
	jmp	.LBB35_107
	.align	16, 0x90
.LBB35_19:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$98, %rdx
	jg	.LBB35_24
# BB#20:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$46, %rdx
	jne	.LBB35_28
# BB#21:                                # %sw.bb165
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 32(%r14)
	movzbl	1(%r15), %eax
	cmpq	$42, %rax
	jne	.LBB35_22
# BB#90:                                # %if.else173
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	2(%r15), %rax
	movq	%rax, 32(%r14)
	movzbl	2(%r15), %eax
	cmpq	$115, %rax
	jne	.LBB35_91
# BB#92:                                # %if.end182
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rax
	movl	(%rax), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB35_94
# BB#93:                                # %vaarg.in_reg188
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rax), %rcx
	addq	%rsi, %rcx
	addq	$8, %rdx
	movl	%edx, (%rax)
	jmp	.LBB35_95
	.align	16, 0x90
.LBB35_24:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$119, %rdx
	jg	.LBB35_80
# BB#25:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$104, %rdx
	jg	.LBB35_31
# BB#26:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$99, %rdx
	jne	.LBB35_27
# BB#42:                                # %sw.bb
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rax
	movl	(%rax), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB35_44
# BB#43:                                # %vaarg.in_reg
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rax), %rcx
	addq	%rsi, %rcx
	addq	$8, %rdx
	movl	%edx, (%rax)
	jmp	.LBB35_45
.LBB35_80:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$120, %rdx
	jne	.LBB35_28
# BB#81:                                # %sw.bb133
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rcx
	movslq	(%rcx), %rdx
	testq	%rax, %rax
	jne	.LBB35_82
# BB#86:                                # %if.else149
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_88
# BB#87:                                # %vaarg.in_reg155
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_89
.LBB35_31:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$110, %rdx
	jg	.LBB35_36
# BB#32:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$105, %rdx
	jne	.LBB35_28
	jmp	.LBB35_33
.LBB35_94:                              # %vaarg.in_mem190
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rax), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 8(%rax)
.LBB35_95:                              # %vaarg.end194
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rax
	movl	(%rax), %edx
	movslq	(%rcx), %rbx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB35_97
# BB#96:                                # %vaarg.in_reg201
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rax), %rcx
	addq	%rsi, %rcx
	addq	$8, %rdx
	movl	%edx, (%rax)
	jmp	.LBB35_98
.LBB35_97:                              # %vaarg.in_mem203
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rax), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 8(%rax)
.LBB35_98:                              # %vaarg.end207
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rcx), %r13
	movl	176(%r14), %eax
	leaq	(%r13,%rbx), %r12
	testq	%rax, %rax
	jne	.LBB35_104
# BB#99:                                # %if.then.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r14, %rdi
	callq	output_emit_prefix
	movslq	12(%r14), %rax
	testq	%rax, %rax
	jle	.LBB35_104
# BB#100:                               # %if.then.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB35_104
	.align	16, 0x90
.LBB35_101:                             # %land.rhs.i
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	cmpq	$32, %rax
	jne	.LBB35_104
# BB#102:                               # %while.cond.i
                                        #   in Loop: Header=BB35_101 Depth=2
	incq	%r13
	decq	%rbx
	jne	.LBB35_101
# BB#103:                               #   in Loop: Header=BB35_2 Depth=1
	movq	%r12, %r13
.LBB35_104:                             # %if.end5.i
                                        #   in Loop: Header=BB35_2 Depth=1
	subq	%r13, %r12
	movq	112(%r14), %rdi
	movq	120(%r14), %rax
	movq	$32, %rcx
	movq	%r12, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	(%rdi,%r15), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB35_106
# BB#105:                               # %if.then.i.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	_obstack_newchunk
	movq	112(%r14), %rdi
.LBB35_106:                             # %output_append.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	memcpy
	addq	%r15, 112(%r14)
	movl	176(%r14), %eax
	addq	%r12, %rax
	movl	%eax, 176(%r14)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$37, %r13
	jmp	.LBB35_107
.LBB35_27:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$100, %rdx
	jne	.LBB35_28
.LBB35_33:                              # %sw.bb27
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rcx
	movslq	(%rcx), %rdx
	testq	%rax, %rax
	jne	.LBB35_34
# BB#57:                                # %if.else
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_59
# BB#58:                                # %vaarg.in_reg48
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_60
.LBB35_82:                              # %if.then135
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_84
# BB#83:                                # %vaarg.in_reg141
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_85
.LBB35_36:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	andq	%r12, %rdx
	cmpq	$111, %rdx
	je	.LBB35_61
# BB#37:                                # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$115, %rdx
	jne	.LBB35_38
# BB#70:                                # %sw.bb88
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rax
	movl	(%rax), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB35_72
# BB#71:                                # %vaarg.in_reg94
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rax), %rcx
	addq	%rsi, %rcx
	addq	$8, %rdx
	movl	%edx, (%rax)
	jmp	.LBB35_73
.LBB35_44:                              # %vaarg.in_mem
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rax), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 8(%rax)
.LBB35_45:                              # %vaarg.end
                                        #   in Loop: Header=BB35_2 Depth=1
	movslq	12(%r14), %rax
	movl	(%rcx), %ebx
	testq	%rax, %rax
	jle	.LBB35_50
# BB#46:                                # %land.lhs.true.i276
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	8(%r14), %eax
	movl	176(%r14), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB35_50
# BB#47:                                # %if.then.i281
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	112(%r14), %rax
	movq	120(%r14), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB35_49
# BB#48:                                # %if.then.i.i284
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$1, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r14), %rax
.LBB35_49:                              # %output_add_newline.exit.i286
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r14)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%r14)
.LBB35_50:                              # %if.end.i291
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	112(%r14), %rax
	movq	120(%r14), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB35_52
# BB#51:                                # %if.then3.i294
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$1, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r14), %rax
.LBB35_52:                              # %output_add_character.exit298
                                        #   in Loop: Header=BB35_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r14)
	movb	%bl, (%rax)
	jmp	.LBB35_53
.LBB35_88:                              # %vaarg.in_mem157
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_89:                              # %vaarg.end161
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	(%rax), %edx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str28, %rsi
	jmp	.LBB35_56
.LBB35_34:                              # %if.then29
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_54
# BB#35:                                # %vaarg.in_reg35
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_55
.LBB35_84:                              # %vaarg.in_mem143
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_85:                              # %vaarg.end147
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rax), %rdx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str29, %rsi
	jmp	.LBB35_56
.LBB35_61:                              # %sw.bb57
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rcx
	movslq	(%rcx), %rdx
	testq	%rax, %rax
	jne	.LBB35_62
# BB#66:                                # %if.else73
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_68
# BB#67:                                # %vaarg.in_reg79
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_69
.LBB35_38:                              # %if.end23
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$117, %rdx
	jne	.LBB35_28
# BB#39:                                # %sw.bb102
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	40(%r14), %rcx
	movslq	(%rcx), %rdx
	testq	%rax, %rax
	jne	.LBB35_40
# BB#76:                                # %if.else118
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_78
# BB#77:                                # %vaarg.in_reg124
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_79
	.align	16, 0x90
.LBB35_28:                              # %sw.default
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	312(%r14), %rax
	testq	%rax, %rax
	je	.LBB35_30
# BB#29:                                # %lor.lhs.false
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r14, %rdi
	callq	*%rax
	testq	%r12, %rax
	jne	.LBB35_107
	jmp	.LBB35_30
.LBB35_59:                              # %vaarg.in_mem50
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_60:                              # %vaarg.end54
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	(%rax), %edx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str, %rsi
	jmp	.LBB35_56
.LBB35_54:                              # %vaarg.in_mem37
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_55:                              # %vaarg.end41
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rax), %rdx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str34, %rsi
	jmp	.LBB35_56
.LBB35_72:                              # %vaarg.in_mem96
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rax), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 8(%rax)
.LBB35_73:                              # %vaarg.end100
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rcx), %rsi
	movq	%r14, %rdi
	callq	output_add_string
	jmp	.LBB35_107
.LBB35_62:                              # %if.then59
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_64
# BB#63:                                # %vaarg.in_reg65
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_65
.LBB35_68:                              # %vaarg.in_mem81
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_69:                              # %vaarg.end85
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	(%rax), %edx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str32, %rsi
	jmp	.LBB35_56
.LBB35_40:                              # %if.then104
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$40, %rax
	ja	.LBB35_74
# BB#41:                                # %vaarg.in_reg110
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%rcx), %rax
	addq	%rdx, %rax
	addq	$8, %rdx
	movl	%edx, (%rcx)
	jmp	.LBB35_75
.LBB35_64:                              # %vaarg.in_mem67
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_65:                              # %vaarg.end71
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rax), %rdx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str33, %rsi
	jmp	.LBB35_56
.LBB35_78:                              # %vaarg.in_mem126
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_79:                              # %vaarg.end130
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	(%rax), %edx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str30, %rsi
	jmp	.LBB35_56
.LBB35_74:                              # %vaarg.in_mem112
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rcx), %rax
	leaq	8(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB35_75:                              # %vaarg.end116
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	(%rax), %rdx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str31, %rsi
.LBB35_56:                              # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	callq	sprintf
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	output_add_string
	.align	16, 0x90
.LBB35_107:                             # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	32(%r14), %rax
	leaq	1(%rax), %rsi
	movq	%rsi, 32(%r14)
	movzbl	1(%rax), %eax
	testq	%rax, %rax
	jne	.LBB35_2
.LBB35_108:                             # %for.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB35_22:                              # %if.then172
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.output_format, %rcx
	movq	$723, %rsi              # imm = 0x2D3
	jmp	.LBB35_23
.LBB35_91:                              # %if.then180
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.output_format, %rcx
	movq	$725, %rsi              # imm = 0x2D5
	jmp	.LBB35_23
.LBB35_30:                              # %if.then212
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.output_format, %rcx
	movq	$737, %rsi              # imm = 0x2E1
.LBB35_23:                              # %if.then172
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp250:
	.size	output_format, .Ltmp250-output_format
	.cfi_endproc

	.align	16, 0x90
	.type	diagnostic_finish,@function
diagnostic_finish:                      # @diagnostic_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp255:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp256:
	.cfi_def_cfa_offset 32
.Ltmp257:
	.cfi_offset %rbx, -24
.Ltmp258:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	112(%rbx), %rax
	movq	120(%rbx), %rcx
	leaq	88(%rbx), %r14
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB36_2
# BB#1:                                 # %if.then.i.i
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	112(%rbx), %rax
.LBB36_2:                               # %output_finalize_message.exit.i
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%rbx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	80(%rbx), %rsi
	movq	104(%rbx), %rdi
	callq	fputs
	movq	96(%rbx), %rax
	movq	104(%rbx), %rsi
	cmpq	%rax, %rsi
	jbe	.LBB36_5
# BB#3:                                 # %land.lhs.true.i.i
	movq	120(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB36_5
# BB#4:                                 # %if.then.i9.i
	movq	%rsi, 104(%rbx)
	movq	%rsi, 112(%rbx)
	jmp	.LBB36_6
.LBB36_5:                               # %if.else.i.i
	movq	%r14, %rdi
	callq	obstack_free
.LBB36_6:                               # %output_buffer_to_stream.exit
	xorq	%rax, %rax
	movl	%eax, 176(%rbx)
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 32(%rbx)
	movq	80(%rbx), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	80(%rbx), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fflush  # TAILCALL
.Ltmp259:
	.size	diagnostic_finish, .Ltmp259-diagnostic_finish
	.cfi_endproc

	.globl	pedwarn
	.align	16, 0x90
	.type	pedwarn,@function
pedwarn:                                # @pedwarn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp268:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp269:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp270:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp271:
	.cfi_def_cfa_offset 496
.Ltmp272:
	.cfi_offset %rbx, -48
.Ltmp273:
	.cfi_offset %r12, -40
.Ltmp274:
	.cfi_offset %r13, -32
.Ltmp275:
	.cfi_offset %r14, -24
.Ltmp276:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	input_filename(%rip), %r13
	movl	flag_pedantic_errors(%rip), %eax
	movl	lineno(%rip), %r15d
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	testq	%rax, %rax
	jne	.LBB37_1
# BB#2:                                 # %entry
	movabsq	$1, %rbx
	jmp	.LBB37_3
.LBB37_1:
	movabsq	$0, %rbx
.LBB37_3:                               # %entry
	leaq	72(%rsp), %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	movq	%r14, %rdi
	callq	memset
	movq	%r12, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%r13, 408(%rsp)
	movq	global_dc(%rip), %rax
	movl	%r15d, 416(%rsp)
	movl	%ebx, 420(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 432(%rsp)
	movq	%rcx, 424(%rsp)
	movq	%r14, %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp277:
	.size	pedwarn, .Ltmp277-pedwarn
	.cfi_endproc

	.globl	set_diagnostic_context
	.align	16, 0x90
	.type	set_diagnostic_context,@function
set_diagnostic_context:                 # @set_diagnostic_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp284:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp285:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp286:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp287:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp288:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp289:
	.cfi_def_cfa_offset 64
.Ltmp290:
	.cfi_offset %rbx, -48
.Ltmp291:
	.cfi_offset %r12, -40
.Ltmp292:
	.cfi_offset %r13, -32
.Ltmp293:
	.cfi_offset %r14, -24
.Ltmp294:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
                                        # kill: RDI<def> R14<kill>
	callq	memset
	movq	%rbx, 320(%r14)
	movq	%r13, 328(%r14)
	movq	%r12, 336(%r14)
	movl	%r15d, 344(%r14)
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, 348(%r14)
	movq	global_dc(%rip), %rax
	movq	352(%rax), %rax
	movq	%rax, 352(%r14)
	movq	global_dc(%rip), %rax
	movq	360(%rax), %rax
	movq	%rax, 360(%r14)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp295:
	.size	set_diagnostic_context, .Ltmp295-set_diagnostic_context
	.cfi_endproc

	.globl	report_diagnostic
	.align	16, 0x90
	.type	report_diagnostic,@function
report_diagnostic:                      # @report_diagnostic
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp300:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp301:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp302:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp303:
	.cfi_def_cfa_offset 112
.Ltmp304:
	.cfi_offset %rbx, -32
.Ltmp305:
	.cfi_offset %r14, -24
.Ltmp306:
	.cfi_offset %r15, -16
	movl	diagnostic_lock(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	leaq	1(%rax), %rax
	movl	%eax, diagnostic_lock(%rip)
	jne	.LBB39_12
# BB#1:                                 # %if.end
	movl	348(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB39_9
# BB#2:                                 # %land.lhs.true.i
	movl	inhibit_warnings(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB39_11
# BB#3:                                 # %land.lhs.true2.i
	movl	in_system_header(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB39_5
# BB#4:                                 # %land.lhs.true2.i
	movl	warn_system_headers(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB39_11
.LBB39_5:                               # %if.end.i
	movl	warnings_are_errors(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_6
# BB#7:                                 # %if.else.i
	movzbl	count_error.warning_message(%rip), %eax
	testq	%rax, %rax
	jne	.LBB39_9
# BB#8:                                 # %if.then13.i
	movq	progname(%rip), %rsi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	verbatim
	movq	$1, %rax
	movb	%al, count_error.warning_message(%rip)
.LBB39_9:                               # %if.end14.i
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
.LBB39_10:                              # %if.then2
	movq	diagnostic_buffer(%rip), %r15
	leaq	(%rsp), %r14
	movq	$80, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	memcpy
	movq	320(%rbx), %rax
	movq	%rax, 32(%r15)
	movq	328(%rbx), %rax
	movq	diagnostic_buffer(%rip), %rcx
	movq	%rax, 40(%rcx)
	movq	diagnostic_buffer(%rip), %rdi
	movq	%rbx, %rsi
	callq	*352(%rbx)
	movq	diagnostic_buffer(%rip), %rdi
	callq	output_format
	movq	diagnostic_buffer(%rip), %rdi
	movq	%rbx, %rsi
	callq	*360(%rbx)
	movq	global_dc(%rip), %rdi
	callq	diagnostic_finish
	movq	diagnostic_buffer(%rip), %rdi
	movq	$80, %rdx
	movq	%r14, %rsi
	callq	memcpy
	movl	diagnostic_lock(%rip), %eax
.LBB39_11:                              # %if.end6
	decq	%rax
	movl	%eax, diagnostic_lock(%rip)
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB39_6:                               # %if.then9.i
	movq	global_dc(%rip), %rax
	movl	64(%rax), %ecx
	incq	%rcx
	movl	%ecx, 64(%rax)
	jmp	.LBB39_10
.LBB39_12:                              # %if.then
	callq	error_recursion
.Ltmp307:
	.size	report_diagnostic, .Ltmp307-report_diagnostic
	.cfi_endproc

	.globl	pedwarn_with_decl
	.align	16, 0x90
	.type	pedwarn_with_decl,@function
pedwarn_with_decl:                      # @pedwarn_with_decl
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp309:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movzbl	50(%rdi), %eax
	testq	$1, %rax
	jne	.LBB40_5
# BB#1:                                 # %if.then
	movl	flag_pedantic_errors(%rip), %eax
	testq	%rax, %rax
	jne	.LBB40_2
# BB#3:                                 # %if.then
	movabsq	$1, %rcx
	jmp	.LBB40_4
.LBB40_2:
	movabsq	$0, %rcx
.LBB40_4:                               # %if.then
	leaq	48(%rsp), %rdx
	callq	diagnostic_for_decl
.LBB40_5:                               # %if.end
	addq	$72, %rsp
	retq
.Ltmp310:
	.size	pedwarn_with_decl, .Ltmp310-pedwarn_with_decl
	.cfi_endproc

	.align	16, 0x90
	.type	diagnostic_for_decl,@function
diagnostic_for_decl:                    # @diagnostic_for_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp317:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp318:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp319:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp320:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp321:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp322:
	.cfi_def_cfa_offset 144
.Ltmp323:
	.cfi_offset %rbx, -48
.Ltmp324:
	.cfi_offset %r12, -40
.Ltmp325:
	.cfi_offset %r13, -32
.Ltmp326:
	.cfi_offset %r14, -24
.Ltmp327:
	.cfi_offset %r15, -16
	movl	diagnostic_lock(%rip), %eax
	movq	%rcx, %r14
	movq	%rdi, %r15
	testq	%rax, %rax
	leaq	1(%rax), %rax
	movl	%eax, diagnostic_lock(%rip)
	jne	.LBB41_57
# BB#1:                                 # %if.end
	movq	%rdx, %r12
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r14
	je	.LBB41_9
# BB#2:                                 # %land.lhs.true.i
	movl	inhibit_warnings(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB41_56
# BB#3:                                 # %land.lhs.true2.i
	movl	in_system_header(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB41_5
# BB#4:                                 # %land.lhs.true2.i
	movl	warn_system_headers(%rip), %ecx
	testq	%r13, %rcx
	je	.LBB41_56
.LBB41_5:                               # %if.end.i
	movl	warnings_are_errors(%rip), %eax
	testq	%rax, %rax
	je	.LBB41_6
# BB#7:                                 # %if.else.i
	movzbl	count_error.warning_message(%rip), %eax
	testq	%rax, %rax
	jne	.LBB41_9
# BB#8:                                 # %if.then13.i
	movq	progname(%rip), %rsi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	verbatim
	movq	$1, %rax
	movb	%al, count_error.warning_message(%rip)
.LBB41_9:                               # %if.end14.i
	movq	global_dc(%rip), %rbx
	movl	60(%rbx), %eax
	incq	%rax
	movl	%eax, 60(%rbx)
.LBB41_10:                              # %if.then2
	movq	diagnostic_buffer(%rip), %rsi
	leaq	16(%rsp), %rdi
	movq	$80, %rdx
	callq	memcpy
	movq	%rbx, %rdi
	callq	report_problematic_module
	movq	global_dc(%rip), %rdi
	movq	input_filename(%rip), %rsi
	callq	*print_error_function(%rip)
	movq	%r15, %rax
	movq	24(%rax), %rsi
	movq	diagnostic_buffer(%rip), %r15
	testq	%rsi, %rsi
	je	.LBB41_15
# BB#11:                                # %if.then.i
	movl	32(%rax), %edx
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%r13, %r14
	je	.LBB41_14
# BB#12:                                # %if.then2.i
	movabsq	$.L.str1, %rdi
	jmp	.LBB41_13
.LBB41_15:                              # %if.else4.i
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	progname(%rip), %rsi
	testq	%r13, %r14
	je	.LBB41_17
# BB#16:                                # %if.then6.i
	movabsq	$.L.str3, %rdi
	jmp	.LBB41_18
.LBB41_14:                              # %if.else.i19
	movabsq	$.L.str2, %rdi
.LBB41_13:                              # %context_as_prefix.exit
	xorq	%rax, %rax
	callq	build_message_string
	jmp	.LBB41_19
.LBB41_17:                              # %if.else8.i
	movabsq	$.L.str4, %rdi
.LBB41_18:                              # %context_as_prefix.exit
	xorq	%rax, %rax
	callq	build_message_string
.LBB41_19:                              # %context_as_prefix.exit
	movq	%rax, (%r15)
	movslq	12(%r15), %rbx
	testq	%rbx, %rbx
	jle	.LBB41_25
# BB#20:                                # %lor.lhs.false.i.i
	movl	24(%r15), %ecx
	cmpq	$1, %rcx
	jbe	.LBB41_25
# BB#21:                                # %if.else.i.i
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB41_23
# BB#22:                                # %cond.true.i.i
	movq	%rax, %rdi
	callq	strlen
	movq	%rax, %rcx
.LBB41_23:                              # %cond.end.i.i
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$31, %rax
	jg	.LBB41_25
# BB#24:                                # %if.then16.i.i
	addq	$32, %rbx
.LBB41_25:                              # %if.else21.i.i
	movl	%ebx, 8(%r15)
	xorq	%rax, %rax
	movb	%al, 20(%r15)
	movl	%eax, 16(%r15)
	movq	diagnostic_buffer(%rip), %rax
	movq	%r12, 40(%rax)
	movq	diagnostic_buffer(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	diagnostic_buffer(%rip), %r15
	movq	32(%r15), %r12
	movq	%r12, %rbx
	jmp	.LBB41_26
	.align	16, 0x90
.LBB41_30:                              # %for.inc.i
                                        #   in Loop: Header=BB41_26 Depth=1
	incq	%rbx
.LBB41_26:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB41_32
# BB#27:                                # %for.cond.i
                                        #   in Loop: Header=BB41_26 Depth=1
	cmpq	$37, %rax
	jne	.LBB41_30
# BB#28:                                # %if.then.i21
                                        #   in Loop: Header=BB41_26 Depth=1
	movzbl	1(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB41_31
# BB#29:                                #   in Loop: Header=BB41_26 Depth=1
	incq	%rbx
	jmp	.LBB41_30
.LBB41_31:                              # %if.then.i21
	cmpq	$115, %rax
	jne	.LBB41_58
.LBB41_32:                              # %for.end.i
	movslq	12(%r15), %rax
	testq	%rax, %rax
	jle	.LBB41_34
# BB#33:                                # %if.then.i.i24
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	wrap_text
	jmp	.LBB41_43
.LBB41_34:                              # %if.else.i.i25
	movl	176(%r15), %eax
	testq	%rax, %rax
	jne	.LBB41_40
# BB#35:                                # %if.then.i.i.i
	movq	%r15, %rdi
	callq	output_emit_prefix
	movslq	12(%r15), %rax
	testq	%rax, %rax
	jle	.LBB41_40
# BB#36:                                # %if.then.i.i.i
	cmpq	%rbx, %r12
	je	.LBB41_40
	.align	16, 0x90
.LBB41_37:                              # %land.rhs.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB41_40
# BB#38:                                # %while.cond.i.i.i
                                        #   in Loop: Header=BB41_37 Depth=1
	incq	%r12
	cmpq	%r12, %rbx
	jne	.LBB41_37
# BB#39:
	movq	%rbx, %r12
.LBB41_40:                              # %if.end5.i.i.i
	movq	%rbx, %r13
	subq	%r12, %r13
	movq	112(%r15), %rdi
	movq	120(%r15), %rax
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%rdi,%r14), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB41_42
# BB#41:                                # %if.then.i.i.i.i
	leaq	88(%r15), %rdi
	movq	%r13, %rsi
	callq	_obstack_newchunk
	movq	112(%r15), %rdi
.LBB41_42:                              # %output_append.exit.i.i
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	memcpy
	addq	%r14, 112(%r15)
	movl	176(%r15), %eax
	addq	%r13, %rax
	movl	%eax, 176(%r15)
.LBB41_43:                              # %maybe_wrap_text.exit.i
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB41_51
# BB#44:                                # %if.then19.i
	movq	(%rsp), %rdi            # 8-byte Reload
	cmpq	$0, 72(%rdi)
	je	.LBB41_45
# BB#46:                                # %cond.true.i
	movq	$2, %rsi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rsi
	jmp	.LBB41_47
.LBB41_45:
	movabsq	$.L.str27, %rsi
.LBB41_47:                              # %cond.end.i
	movq	%r15, %rdi
	callq	output_add_string
	.align	16, 0x90
.LBB41_48:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB41_53
# BB#49:                                # %while.body.i
                                        #   in Loop: Header=BB41_48 Depth=1
	movzbl	_sch_istable(%rax,%rax), %eax
	incq	%rbx
	testq	$136, %rax
	je	.LBB41_48
# BB#50:                                # %while.body.if.end32.loopexit_crit_edge.i
	movzbl	(%rbx), %eax
.LBB41_51:                              # %if.end32.i
	testq	$255, %rax
	je	.LBB41_53
# BB#52:                                # %if.then34.i
	movq	%rbx, 32(%r15)
	movq	%r15, %rdi
	callq	output_format
.LBB41_53:                              # %format_with_decl.exit
	movq	global_dc(%rip), %rdi
	callq	diagnostic_finish
	movq	diagnostic_buffer(%rip), %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB41_55
# BB#54:                                # %if.then.i28
	callq	free
	movq	$0, (%rbx)
	movq	diagnostic_buffer(%rip), %rbx
.LBB41_55:                              # %output_destroy_prefix.exit
	leaq	16(%rsp), %rsi
	movq	$80, %rdx
	movq	%rbx, %rdi
	callq	memcpy
	movl	diagnostic_lock(%rip), %eax
.LBB41_56:                              # %if.end11
	decq	%rax
	movl	%eax, diagnostic_lock(%rip)
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_6:                               # %if.then9.i
	movq	global_dc(%rip), %rbx
	movl	64(%rbx), %eax
	incq	%rax
	movl	%eax, 64(%rbx)
	jmp	.LBB41_10
.LBB41_57:                              # %if.then
	callq	error_recursion
.LBB41_58:                              # %if.then10.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.format_with_decl, %rcx
	movq	$858, %rsi              # imm = 0x35A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp328:
	.size	diagnostic_for_decl, .Ltmp328-diagnostic_for_decl
	.cfi_endproc

	.globl	pedwarn_with_file_and_line
	.align	16, 0x90
	.type	pedwarn_with_file_and_line,@function
pedwarn_with_file_and_line:             # @pedwarn_with_file_and_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp335:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp336:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp337:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp338:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp339:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp340:
	.cfi_def_cfa_offset 496
.Ltmp341:
	.cfi_offset %rbx, -48
.Ltmp342:
	.cfi_offset %r12, -40
.Ltmp343:
	.cfi_offset %r13, -32
.Ltmp344:
	.cfi_offset %r14, -24
.Ltmp345:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movl	flag_pedantic_errors(%rip), %ecx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	testq	%rcx, %rcx
	jne	.LBB42_1
# BB#2:                                 # %entry
	movabsq	$1, %rbx
	jmp	.LBB42_3
.LBB42_1:
	movabsq	$0, %rbx
.LBB42_3:                               # %entry
	leaq	72(%rsp), %r12
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	movq	%r12, %rdi
	callq	memset
	movq	%r13, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%r15, 408(%rsp)
	movq	global_dc(%rip), %rax
	movl	%r14d, 416(%rsp)
	movl	%ebx, 420(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 432(%rsp)
	movq	%rcx, 424(%rsp)
	movq	%r12, %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp346:
	.size	pedwarn_with_file_and_line, .Ltmp346-pedwarn_with_file_and_line
	.cfi_endproc

	.globl	sorry
	.align	16, 0x90
	.type	sorry,@function
sorry:                                  # @sorry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp351:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp352:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp353:
	.cfi_def_cfa_offset 32
	subq	$160, %rsp
.Ltmp354:
	.cfi_def_cfa_offset 192
.Ltmp355:
	.cfi_offset %rbx, -32
.Ltmp356:
	.cfi_offset %r14, -24
.Ltmp357:
	.cfi_offset %r15, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	192(%rsp), %rax
	movq	global_dc(%rip), %rcx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	movl	56(%rcx), %eax
	incq	%rax
	movl	%eax, 56(%rcx)
	movq	diagnostic_buffer(%rip), %rbx
	movq	%rdi, %r14
	leaq	80(%rsp), %rdi
	movq	$80, %rdx
	movq	%rbx, %rsi
	callq	memcpy
	movq	input_filename(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB43_2
# BB#1:                                 # %if.then.i
	movl	lineno(%rip), %edx
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	build_message_string
	jmp	.LBB43_3
.LBB43_2:                               # %if.else4.i
	movq	progname(%rip), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	build_message_string
.LBB43_3:                               # %context_as_prefix.exit
	movq	%rax, (%rbx)
	movslq	12(%rbx), %r15
	testq	%r15, %r15
	jle	.LBB43_9
# BB#4:                                 # %lor.lhs.false.i.i
	movl	24(%rbx), %ecx
	cmpq	$1, %rcx
	jbe	.LBB43_9
# BB#5:                                 # %if.else.i.i
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB43_7
# BB#6:                                 # %cond.true.i.i
	movq	%rax, %rdi
	callq	strlen
	movq	%rax, %rcx
.LBB43_7:                               # %cond.end.i.i
	movq	%r15, %rax
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$31, %rax
	jg	.LBB43_9
# BB#8:                                 # %if.then16.i.i
	addq	$32, %r15
.LBB43_9:                               # %if.else21.i.i
	movl	%r15d, 8(%rbx)
	xorq	%rax, %rax
	movb	%al, 20(%rbx)
	movl	%eax, 16(%rbx)
	movq	diagnostic_buffer(%rip), %rdi
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	callq	output_printf
	movq	diagnostic_buffer(%rip), %rax
	leaq	48(%rsp), %rcx
	movq	%rcx, 40(%rax)
	movq	diagnostic_buffer(%rip), %rax
	movq	%r14, 32(%rax)
	movq	diagnostic_buffer(%rip), %rdi
	callq	output_format
	movq	global_dc(%rip), %rdi
	callq	diagnostic_finish
	movq	diagnostic_buffer(%rip), %rdi
	leaq	80(%rsp), %rsi
	movq	$80, %rdx
	callq	memcpy
	addq	$160, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp358:
	.size	sorry, .Ltmp358-sorry
	.cfi_endproc

	.globl	announce_function
	.align	16, 0x90
	.type	announce_function,@function
announce_function:                      # @announce_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp360:
	.cfi_def_cfa_offset 16
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	jne	.LBB44_5
# BB#1:                                 # %if.then
	movl	rtl_dump_and_exit(%rip), %eax
	testq	%rax, %rax
	je	.LBB44_3
# BB#2:                                 # %if.then2
	movq	72(%rdi), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	jmp	.LBB44_4
.LBB44_3:                               # %if.else
	movq	$2, %rsi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rcx
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
.LBB44_4:                               # %if.end
	callq	verbatim
	movq	stderr(%rip), %rdi
	callq	fflush
	movq	diagnostic_buffer(%rip), %rax
	movq	$1, %rcx
	movb	%cl, 21(%rax)
	movq	current_function_decl(%rip), %rax
	movq	%rax, last_error_function(%rip)
.LBB44_5:                               # %if.end5
	popq	%rax
	retq
.Ltmp361:
	.size	announce_function, .Ltmp361-announce_function
	.cfi_endproc

	.globl	report_error_function
	.align	16, 0x90
	.type	report_error_function,@function
report_error_function:                  # @report_error_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp363:
	.cfi_def_cfa_offset 16
	movq	global_dc(%rip), %rdi
	callq	report_problematic_module
	movq	print_error_function(%rip), %rax
	movq	global_dc(%rip), %rdi
	movq	input_filename(%rip), %rsi
	popq	%rdx
	jmpq	*%rax  # TAILCALL
.Ltmp364:
	.size	report_error_function, .Ltmp364-report_error_function
	.cfi_endproc

	.globl	report_problematic_module
	.align	16, 0x90
	.type	report_problematic_module,@function
report_problematic_module:              # @report_problematic_module
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp370:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp371:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp372:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp373:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp374:
	.cfi_def_cfa_offset 48
.Ltmp375:
	.cfi_offset %rbx, -40
.Ltmp376:
	.cfi_offset %r12, -32
.Ltmp377:
	.cfi_offset %r14, -24
.Ltmp378:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	21(%r12), %eax
	testq	%rax, %rax
	je	.LBB46_4
# BB#1:                                 # %if.then
	movq	112(%r12), %rax
	movq	120(%r12), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB46_3
# BB#2:                                 # %if.then.i
	leaq	88(%r12), %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	112(%r12), %rax
.LBB46_3:                               # %output_add_newline.exit
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r12)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%r12)
	movb	%al, 21(%r12)
.LBB46_4:                               # %if.end
	movq	input_file_stack(%rip), %rax
	testq	%rax, %rax
	je	.LBB46_14
# BB#5:                                 # %land.lhs.true
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB46_14
# BB#6:                                 # %land.lhs.true4
	movl	last_error_tick(%rip), %ecx
	movl	input_file_stack_tick(%rip), %edx
	cmpq	%rdx, %rcx
	je	.LBB46_14
# BB#7:
	movabsq	$.L.str18, %r14
	movabsq	$.L.str19, %r15
	movq	%rax, %rbx
	jmp	.LBB46_8
	.align	16, 0x90
.LBB46_11:                              # %for.cond.backedge.for.body_crit_edge
                                        #   in Loop: Header=BB46_8 Depth=1
	movq	input_file_stack(%rip), %rax
	movq	8(%rax), %rax
.LBB46_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdx
	movl	16(%rbx), %ecx
	cmpq	%rax, %rbx
	jne	.LBB46_12
# BB#9:                                 # %if.then11
                                        #   in Loop: Header=BB46_8 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	jmp	.LBB46_10
	.align	16, 0x90
.LBB46_12:                              # %if.else
                                        #   in Loop: Header=BB46_8 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
.LBB46_10:                              # %for.cond.backedge
                                        #   in Loop: Header=BB46_8 Depth=1
	callq	output_verbatim
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB46_11
# BB#13:                                # %for.end
	movabsq	$.L.str20, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	output_verbatim
	movl	input_file_stack_tick(%rip), %eax
	movl	%eax, last_error_tick(%rip)
.LBB46_14:                              # %if.end16
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp379:
	.size	report_problematic_module, .Ltmp379-report_problematic_module
	.cfi_endproc

	.globl	error_with_file_and_line
	.align	16, 0x90
	.type	error_with_file_and_line,@function
error_with_file_and_line:               # @error_with_file_and_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp386:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp387:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp388:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp389:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp390:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp391:
	.cfi_def_cfa_offset 496
.Ltmp392:
	.cfi_offset %rbx, -48
.Ltmp393:
	.cfi_offset %r12, -40
.Ltmp394:
	.cfi_offset %r13, -32
.Ltmp395:
	.cfi_offset %r14, -24
.Ltmp396:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	leaq	72(%rsp), %r12
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	xorq	%r13, %r13
	movq	%r12, %rdi
	callq	memset
	movq	%rbx, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%r15, 408(%rsp)
	movq	global_dc(%rip), %rax
	movl	%r14d, 416(%rsp)
	movl	%r13d, 420(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 432(%rsp)
	movq	%rcx, 424(%rsp)
	movq	%r12, %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp397:
	.size	error_with_file_and_line, .Ltmp397-error_with_file_and_line
	.cfi_endproc

	.globl	error_with_decl
	.align	16, 0x90
	.type	error_with_decl,@function
error_with_decl:                        # @error_with_decl
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp399:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	48(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	diagnostic_for_decl
	addq	$72, %rsp
	retq
.Ltmp400:
	.size	error_with_decl, .Ltmp400-error_with_decl
	.cfi_endproc

	.globl	error
	.align	16, 0x90
	.type	error,@function
error:                                  # @error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp407:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp408:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp409:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp410:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp411:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp412:
	.cfi_def_cfa_offset 496
.Ltmp413:
	.cfi_offset %rbx, -48
.Ltmp414:
	.cfi_offset %r12, -40
.Ltmp415:
	.cfi_offset %r13, -32
.Ltmp416:
	.cfi_offset %r14, -24
.Ltmp417:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	input_filename(%rip), %rbx
	movl	lineno(%rip), %r12d
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	72(%rsp), %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	memset
	movq	%r15, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%rbx, 408(%rsp)
	movq	global_dc(%rip), %rax
	movl	%r12d, 416(%rsp)
	movl	%r13d, 420(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 432(%rsp)
	movq	%rcx, 424(%rsp)
	movq	%r14, %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp418:
	.size	error, .Ltmp418-error
	.cfi_endproc

	.globl	fatal_error
	.align	16, 0x90
	.type	fatal_error,@function
fatal_error:                            # @fatal_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp425:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp426:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp427:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp428:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp429:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp430:
	.cfi_def_cfa_offset 496
.Ltmp431:
	.cfi_offset %rbx, -48
.Ltmp432:
	.cfi_offset %r12, -40
.Ltmp433:
	.cfi_offset %r13, -32
.Ltmp434:
	.cfi_offset %r14, -24
.Ltmp435:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	input_filename(%rip), %rbx
	movl	lineno(%rip), %r12d
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	72(%rsp), %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	memset
	movq	%r15, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%rbx, 408(%rsp)
	movq	global_dc(%rip), %rax
	movl	%r12d, 416(%rsp)
	movl	%r13d, 420(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 432(%rsp)
	movq	%rcx, 424(%rsp)
	movq	%r14, %rdi
	callq	report_diagnostic
	movq	stderr(%rip), %rdi
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	callq	fnotice
	movq	$1, %rdi
	callq	exit
.Ltmp436:
	.size	fatal_error, .Ltmp436-fatal_error
	.cfi_endproc

	.globl	set_internal_error_function
	.align	16, 0x90
	.type	set_internal_error_function,@function
set_internal_error_function:            # @set_internal_error_function
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, internal_error_function(%rip)
	retq
.Ltmp437:
	.size	set_internal_error_function, .Ltmp437-set_internal_error_function
	.cfi_endproc

	.globl	internal_error
	.align	16, 0x90
	.type	internal_error,@function
internal_error:                         # @internal_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp444:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp445:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp446:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp447:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp448:
	.cfi_def_cfa_offset 48
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp449:
	.cfi_def_cfa_offset 496
.Ltmp450:
	.cfi_offset %rbx, -48
.Ltmp451:
	.cfi_offset %r12, -40
.Ltmp452:
	.cfi_offset %r13, -32
.Ltmp453:
	.cfi_offset %r14, -24
.Ltmp454:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movl	diagnostic_lock(%rip), %ecx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	testq	%rcx, %rcx
	jne	.LBB52_6
# BB#1:                                 # %if.end
	movq	global_dc(%rip), %rbx
	movslq	60(%rbx), %rax
	testq	%rax, %rax
	jg	.LBB52_7
# BB#2:                                 # %lor.lhs.false
	movslq	56(%rbx), %rax
	testq	%rax, %rax
	jle	.LBB52_3
.LBB52_7:                               # %if.then6
	movq	stderr(%rip), %rdi
	movq	input_filename(%rip), %rdx
	movl	lineno(%rip), %ecx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	callq	fnotice
	movq	$1, %rdi
	callq	exit
.LBB52_6:                               # %if.then
	callq	error_recursion
.LBB52_3:                               # %if.end7
	movq	internal_error_function(%rip), %rax
	testq	%rax, %rax
	je	.LBB52_5
# BB#4:                                 # %if.then9
	leaq	48(%rsp), %rsi
	movq	%r15, %rdi
	callq	*%rax
	movq	global_dc(%rip), %rbx
.LBB52_5:                               # %if.end10
	movq	input_filename(%rip), %r12
	leaq	72(%rsp), %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	memset
	movq	%r15, 392(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 400(%rsp)
	movq	%r12, 408(%rsp)
	movl	lineno(%rip), %eax
	movl	%eax, 416(%rsp)
	movl	%r13d, 420(%rsp)
	movq	352(%rbx), %rax
	movq	360(%rbx), %rcx
	movq	%rcx, 432(%rsp)
	movq	%rax, 424(%rsp)
	movq	%r14, %rdi
	callq	report_diagnostic
	movq	stderr(%rip), %rdi
	movabsq	$.L.str15, %rsi
	movabsq	$.L.str16, %rdx
	xorq	%rax, %rax
	callq	fnotice
	movq	$1, %rdi
	callq	exit
.Ltmp455:
	.size	internal_error, .Ltmp455-internal_error
	.cfi_endproc

	.align	16, 0x90
	.type	error_recursion,@function
error_recursion:                        # @error_recursion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp458:
	.cfi_def_cfa_offset 16
.Ltmp459:
	.cfi_offset %rbx, -16
	movslq	diagnostic_lock(%rip), %rax
	cmpq	$2, %rax
	jg	.LBB53_2
# BB#1:                                 # %if.then
	movq	global_dc(%rip), %rdi
	callq	diagnostic_finish
.LBB53_2:                               # %if.end
	movq	stderr(%rip), %rbx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fnotice
	movabsq	$.L.str15, %rsi
	movabsq	$.L.str16, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fnotice
	movq	$1, %rdi
	callq	exit
.Ltmp460:
	.size	error_recursion, .Ltmp460-error_recursion
	.cfi_endproc

	.globl	warning_with_file_and_line
	.align	16, 0x90
	.type	warning_with_file_and_line,@function
warning_with_file_and_line:             # @warning_with_file_and_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp466:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp467:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp468:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp469:
	.cfi_def_cfa_offset 40
	subq	$456, %rsp              # imm = 0x1C8
.Ltmp470:
	.cfi_def_cfa_offset 496
.Ltmp471:
	.cfi_offset %rbx, -40
.Ltmp472:
	.cfi_offset %r12, -32
.Ltmp473:
	.cfi_offset %r14, -24
.Ltmp474:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	leaq	80(%rsp), %r12
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	movq	%r12, %rdi
	callq	memset
	movq	%rbx, 400(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 408(%rsp)
	movq	%r15, 416(%rsp)
	movl	%r14d, 424(%rsp)
	movq	global_dc(%rip), %rax
	movq	$1, %rcx
	movl	%ecx, 428(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 440(%rsp)
	movq	%rcx, 432(%rsp)
	movq	%r12, %rdi
	callq	report_diagnostic
	addq	$456, %rsp              # imm = 0x1C8
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp475:
	.size	warning_with_file_and_line, .Ltmp475-warning_with_file_and_line
	.cfi_endproc

	.globl	warning_with_decl
	.align	16, 0x90
	.type	warning_with_decl,@function
warning_with_decl:                      # @warning_with_decl
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp477:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	48(%rsp), %rdx
	movq	$1, %rcx
	callq	diagnostic_for_decl
	addq	$72, %rsp
	retq
.Ltmp478:
	.size	warning_with_decl, .Ltmp478-warning_with_decl
	.cfi_endproc

	.globl	warning
	.align	16, 0x90
	.type	warning,@function
warning:                                # @warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp484:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp485:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp486:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp487:
	.cfi_def_cfa_offset 40
	subq	$456, %rsp              # imm = 0x1C8
.Ltmp488:
	.cfi_def_cfa_offset 496
.Ltmp489:
	.cfi_offset %rbx, -40
.Ltmp490:
	.cfi_offset %r12, -32
.Ltmp491:
	.cfi_offset %r14, -24
.Ltmp492:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	496(%rsp), %rax
	movq	input_filename(%rip), %rbx
	movl	lineno(%rip), %r12d
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	leaq	80(%rsp), %r14
	xorq	%rsi, %rsi
	movq	$376, %rdx              # imm = 0x178
	movq	%r14, %rdi
	callq	memset
	movq	%r15, 400(%rsp)
	leaq	48(%rsp), %rax
	movq	%rax, 408(%rsp)
	movq	%rbx, 416(%rsp)
	movl	%r12d, 424(%rsp)
	movq	global_dc(%rip), %rax
	movq	$1, %rcx
	movl	%ecx, 428(%rsp)
	movq	352(%rax), %rcx
	movq	360(%rax), %rax
	movq	%rax, 440(%rsp)
	movq	%rcx, 432(%rsp)
	movq	%r14, %rdi
	callq	report_diagnostic
	addq	$456, %rsp              # imm = 0x1C8
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp493:
	.size	warning, .Ltmp493-warning
	.cfi_endproc

	.globl	output_verbatim
	.align	16, 0x90
	.type	output_verbatim,@function
output_verbatim:                        # @output_verbatim
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp498:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp499:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp500:
	.cfi_def_cfa_offset 32
	subq	$160, %rsp
.Ltmp501:
	.cfi_def_cfa_offset 192
.Ltmp502:
	.cfi_offset %rbx, -32
.Ltmp503:
	.cfi_offset %r14, -24
.Ltmp504:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	192(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	80(%rsp), %r15
	movq	$80, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	memcpy
	movq	$0, (%rbx)
	movq	$1, %rax
	movl	%eax, 24(%rbx)
	movq	%r14, 32(%rbx)
	leaq	48(%rsp), %rax
	movq	%rax, 40(%rbx)
	movq	$0, 8(%rbx)
	movq	%rbx, %rdi
	callq	output_format
	movq	$80, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	memcpy
	addq	$160, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp505:
	.size	output_verbatim, .Ltmp505-output_verbatim
	.cfi_endproc

	.globl	trim_filename
	.align	16, 0x90
	.type	trim_filename,@function
trim_filename:                          # @trim_filename
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$46, %rax
	movq	%rdi, %rcx
	jne	.LBB58_5
# BB#1:
	movq	%rdi, %rcx
	.align	16, 0x90
.LBB58_2:                               # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rcx), %eax
	cmpq	$46, %rax
	jne	.LBB58_11
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB58_2 Depth=1
	movzbl	2(%rcx), %eax
	cmpq	$47, %rax
	jne	.LBB58_11
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB58_2 Depth=1
	movzbl	3(%rcx), %eax
	addq	$3, %rcx
	cmpq	$46, %rax
	je	.LBB58_2
.LBB58_5:                               # %while.cond29.preheader
	cmpq	$100, %rax
	jne	.LBB58_11
# BB#6:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB58_7:                               # %while.body43
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rcx,%rax), %esi
	movzbl	.L.str26+1(%rax), %edx
	incq	%rax
	cmpq	%rdx, %rsi
	jne	.LBB58_10
# BB#8:                                 # %while.body43
                                        #   in Loop: Header=BB58_7 Depth=1
	testq	%rsi, %rsi
	je	.LBB58_10
# BB#9:                                 # %while.body43
                                        #   in Loop: Header=BB58_7 Depth=1
	testq	%rdx, %rdx
	jne	.LBB58_7
.LBB58_10:                              # %while.cond46.preheader.loopexit
	addq	%rax, %rcx
	.align	16, 0x90
.LBB58_11:                              # %while.cond46
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	cmpq	%rdi, %rax
	jbe	.LBB58_13
# BB#12:                                # %land.rhs49
                                        #   in Loop: Header=BB58_11 Depth=1
	movzbl	-1(%rax), %edx
	leaq	-1(%rax), %rcx
	cmpq	$47, %rdx
	jne	.LBB58_11
.LBB58_13:                              # %while.end57
	retq
.Ltmp506:
	.size	trim_filename, .Ltmp506-trim_filename
	.cfi_endproc

	.globl	fancy_abort
	.align	16, 0x90
	.type	fancy_abort,@function
fancy_abort:                            # @fancy_abort
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp508:
	.cfi_def_cfa_offset 16
	movzbl	(%rdi), %ecx
	movq	%rsi, %r8
	cmpq	$46, %rcx
	movq	%rdi, %rax
	jne	.LBB59_5
# BB#1:
	movq	%rdi, %rax
	.align	16, 0x90
.LBB59_2:                               # %land.lhs.true.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rax), %ecx
	cmpq	$46, %rcx
	jne	.LBB59_11
# BB#3:                                 # %land.rhs.i
                                        #   in Loop: Header=BB59_2 Depth=1
	movzbl	2(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB59_11
# BB#4:                                 # %while.cond.i
                                        #   in Loop: Header=BB59_2 Depth=1
	movzbl	3(%rax), %ecx
	addq	$3, %rax
	cmpq	$46, %rcx
	je	.LBB59_2
.LBB59_5:                               # %while.cond29.preheader.i
	cmpq	$100, %rcx
	jne	.LBB59_11
# BB#6:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB59_7:                               # %while.body43.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rax,%rcx), %esi
	movzbl	.L.str26+1(%rcx), %r9d
	incq	%rcx
	cmpq	%r9, %rsi
	jne	.LBB59_10
# BB#8:                                 # %while.body43.i
                                        #   in Loop: Header=BB59_7 Depth=1
	testq	%rsi, %rsi
	je	.LBB59_10
# BB#9:                                 # %while.body43.i
                                        #   in Loop: Header=BB59_7 Depth=1
	testq	%r9, %r9
	jne	.LBB59_7
.LBB59_10:                              # %while.cond46.i.preheader.loopexit
	addq	%rcx, %rax
	.align	16, 0x90
.LBB59_11:                              # %while.cond46.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	cmpq	%rdi, %rcx
	jbe	.LBB59_13
# BB#12:                                # %land.rhs49.i
                                        #   in Loop: Header=BB59_11 Depth=1
	movzbl	-1(%rcx), %esi
	leaq	-1(%rcx), %rax
	cmpq	$47, %rsi
	jne	.LBB59_11
.LBB59_13:                              # %trim_filename.exit
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	internal_error
.Ltmp509:
	.size	fancy_abort, .Ltmp509-fancy_abort
	.cfi_endproc

	.globl	warn_deprecated_use
	.align	16, 0x90
	.type	warn_deprecated_use,@function
warn_deprecated_use:                    # @warn_deprecated_use
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB60_15
# BB#1:                                 # %entry
	movl	warn_deprecated_decl(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_15
# BB#2:                                 # %if.end
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB60_3
# BB#5:                                 # %if.then15
	movq	96(%rdi), %rcx
	movzbl	16(%rcx), %edx
	movq	(%rdi), %rax
	cmpq	$1, %rdx
	je	.LBB60_8
# BB#6:                                 # %if.then15
	cmpq	$33, %rdx
	jne	.LBB60_13
# BB#7:                                 # %land.lhs.true
	movq	72(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB60_13
.LBB60_8:                               # %if.end54
	movq	32(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB60_13
# BB#9:                                 # %if.then56
	testq	%rax, %rax
	je	.LBB60_12
# BB#10:                                # %if.then58
	movq	24(%rax), %rdx
	movl	32(%rax), %ecx
	jmp	.LBB60_11
.LBB60_3:                               # %if.end
	cmpq	$100, %rax
	jne	.LBB60_15
# BB#4:                                 # %if.then3
	movq	72(%rdi), %rax
	movq	24(%rdi), %rdx
	movq	32(%rax), %rsi
	movl	32(%rdi), %ecx
.LBB60_11:                              # %if.then58
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB60_15:                              # %if.end76
	retq
.LBB60_13:                              # %if.else65
	testq	%rax, %rax
	je	.LBB60_14
# BB#16:                                # %if.then67
	movq	24(%rax), %rsi
	movl	32(%rax), %edx
	movabsq	$.L.str23, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB60_14:                              # %if.else72
	movabsq	$.L.str24, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB60_12:                              # %if.else63
	movabsq	$.L.str22, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.Ltmp510:
	.size	warn_deprecated_use, .Ltmp510-warn_deprecated_use
	.cfi_endproc

	.align	16, 0x90
	.type	wrap_text,@function
wrap_text:                              # @wrap_text
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp517:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp518:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp519:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp520:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp521:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp522:
	.cfi_def_cfa_offset 80
.Ltmp523:
	.cfi_offset %rbx, -48
.Ltmp524:
	.cfi_offset %r12, -40
.Ltmp525:
	.cfi_offset %r13, -32
.Ltmp526:
	.cfi_offset %r14, -24
.Ltmp527:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rdi, %r12
	cmpq	%r15, %rbx
	je	.LBB61_42
# BB#1:                                 # %while.cond1.preheader.lr.ph
	leaq	88(%r12), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB61_2
	.align	16, 0x90
.LBB61_37:                              # %land.lhs.true25
                                        #   in Loop: Header=BB61_2 Depth=1
	movzbl	(%r14), %eax
	cmpq	$10, %rax
	jne	.LBB61_41
# BB#38:                                # %if.then29
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	112(%r12), %rax
	movq	120(%r12), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB61_40
# BB#39:                                # %if.then.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r12), %rax
.LBB61_40:                              # %output_add_newline.exit
                                        #   in Loop: Header=BB61_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r12)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%r12)
	incq	%r14
.LBB61_41:                              # %while.cond.backedge
                                        #   in Loop: Header=BB61_2 Depth=1
	cmpq	%r15, %r14
	movq	%r14, %rbx
	jne	.LBB61_2
	jmp	.LBB61_42
.LBB61_6:                               #   in Loop: Header=BB61_2 Depth=1
	movq	%rax, %r14
	jmp	.LBB61_9
	.align	16, 0x90
.LBB61_2:                               # %while.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB61_3 Depth 2
                                        #     Child Loop BB61_18 Depth 2
	cmpq	%r15, %rbx
	movq	%rbx, %rax
	movabsq	$0, %rdx
	movq	%r15, %r14
	je	.LBB61_9
.LBB61_3:                               # %land.lhs.true
                                        #   Parent Loop BB61_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	movq	$1, %rdx
	cmpq	$10, %rcx
	je	.LBB61_4
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB61_3 Depth=2
	cmpq	$32, %rcx
	je	.LBB61_6
# BB#7:                                 # %while.body8
                                        #   in Loop: Header=BB61_3 Depth=2
	incq	%rax
	cmpq	%rax, %r15
	jne	.LBB61_3
# BB#8:                                 #   in Loop: Header=BB61_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %r14
	jmp	.LBB61_9
	.align	16, 0x90
.LBB61_4:                               #   in Loop: Header=BB61_2 Depth=1
	movq	%rax, %r14
.LBB61_9:                               # %while.end
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	$32, %rcx
	movl	12(%r12), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB61_10
# BB#12:                                # %land.lhs.true9
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r14, %rdx
	movl	8(%r12), %esi
	movl	176(%r12), %eax
	subq	%rbx, %rdx
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB61_11
# BB#13:                                # %if.then
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	112(%r12), %rax
	movq	120(%r12), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB61_15
# BB#14:                                # %if.then.i74
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r12), %rax
.LBB61_15:                              # %if.end.thread
                                        #   in Loop: Header=BB61_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r12)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	movl	%eax, 176(%r12)
	jmp	.LBB61_16
	.align	16, 0x90
.LBB61_10:                              # %while.end.if.end_crit_edge
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movl	176(%r12), %eax
.LBB61_11:                              # %if.end
                                        #   in Loop: Header=BB61_2 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB61_21
.LBB61_16:                              # %if.then.i83
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	%r12, %rdi
	callq	output_emit_prefix
	movslq	12(%r12), %rax
	testq	%rax, %rax
	jle	.LBB61_21
# BB#17:                                # %if.then.i83
                                        #   in Loop: Header=BB61_2 Depth=1
	cmpq	%r14, %rbx
	je	.LBB61_21
	.align	16, 0x90
.LBB61_18:                              # %land.rhs.i
                                        #   Parent Loop BB61_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB61_21
# BB#19:                                # %while.cond.i
                                        #   in Loop: Header=BB61_18 Depth=2
	incq	%rbx
	cmpq	%rbx, %r14
	jne	.LBB61_18
# BB#20:                                #   in Loop: Header=BB61_2 Depth=1
	movq	%r14, %rbx
	.align	16, 0x90
.LBB61_21:                              # %if.end5.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	%r14, %r15
	subq	%rbx, %r15
	movq	112(%r12), %rdi
	movq	120(%r12), %rax
	movq	$32, %rcx
	movq	%r15, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%rdi,%r13), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB61_23
# BB#22:                                # %if.then.i.i91
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r15, %rsi
	callq	_obstack_newchunk
	movq	112(%r12), %rdi
.LBB61_23:                              # %output_append.exit
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	memcpy
	movq	112(%r12), %rdx
	leaq	(%rdx,%r13), %rax
	movq	%rax, 112(%r12)
	movl	176(%r12), %ecx
	addq	%r15, %rcx
	movl	%ecx, 176(%r12)
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB61_24
# BB#25:                                # %land.lhs.true16
                                        #   in Loop: Header=BB61_2 Depth=1
	movzbl	(%r14), %esi
	cmpq	$32, %rsi
	jne	.LBB61_26
# BB#27:                                # %if.then20
                                        #   in Loop: Header=BB61_2 Depth=1
	movslq	12(%r12), %rsi
	testq	%rsi, %rsi
	jle	.LBB61_32
# BB#28:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movl	8(%r12), %esi
	subq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB61_32
# BB#29:                                # %if.then.i60
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	120(%r12), %rcx
	leaq	1(%r13,%rdx), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB61_31
# BB#30:                                # %if.then.i.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r12), %rax
.LBB61_31:                              # %output_add_newline.exit.i
                                        #   in Loop: Header=BB61_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r12)
	movq	$10, %rcx
	movb	%cl, (%rax)
	xorq	%rax, %rax
	jmp	.LBB61_35
	.align	16, 0x90
.LBB61_24:                              #   in Loop: Header=BB61_2 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
	jmp	.LBB61_36
	.align	16, 0x90
.LBB61_26:                              #   in Loop: Header=BB61_2 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
	jmp	.LBB61_36
.LBB61_32:                              # %if.end.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	120(%r12), %rcx
	leaq	1(%r13,%rdx), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB61_34
# BB#33:                                # %if.then3.i
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	_obstack_newchunk
	movq	112(%r12), %rax
.LBB61_34:                              # %if.end4.i
                                        #   in Loop: Header=BB61_2 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 112(%r12)
	movq	$32, %rcx
	movb	%cl, (%rax)
	movl	176(%r12), %eax
	incq	%rax
.LBB61_35:                              # %output_add_space.exit
                                        #   in Loop: Header=BB61_2 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
	movl	%eax, 176(%r12)
	incq	%r14
.LBB61_36:                              # %if.end22
                                        #   in Loop: Header=BB61_2 Depth=1
	cmpq	%r15, %r14
	jne	.LBB61_37
.LBB61_42:                              # %while.end32
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp528:
	.size	wrap_text, .Ltmp528-wrap_text
	.cfi_endproc

	.type	global_diagnostic_context,@object # @global_diagnostic_context
	.local	global_diagnostic_context
	.comm	global_diagnostic_context,376,8
	.type	global_dc,@object       # @global_dc
	.data
	.globl	global_dc
	.align	8
global_dc:
	.quad	global_diagnostic_context
	.size	global_dc, 8

	.type	diagnostic_buffer,@object # @diagnostic_buffer
	.globl	diagnostic_buffer
	.align	8
diagnostic_buffer:
	.quad	global_diagnostic_context
	.size	diagnostic_buffer, 8

	.type	print_error_function,@object # @print_error_function
	.globl	print_error_function
	.align	8
print_error_function:
	.quad	default_print_error_function
	.size	print_error_function, 8

	.type	last_error_tick,@object # @last_error_tick
	.local	last_error_tick
	.comm	last_error_tick,4,4
	.type	last_error_function,@object # @last_error_function
	.local	last_error_function
	.comm	last_error_function,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d"
	.size	.L.str, 3

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%s:%d: warning: "
	.size	.L.str1, 17

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%s:%d: "
	.size	.L.str2, 8

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%s: warning: "
	.size	.L.str3, 14

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%s: "
	.size	.L.str4, 5

	.type	count_error.warning_message,@object # @count_error.warning_message
	.local	count_error.warning_message
	.comm	count_error.warning_message,1,1
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%s: warnings being treated as errors\n"
	.size	.L.str5, 38

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"%s: %s: "
	.size	.L.str6, 9

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"sorry, not implemented: "
	.size	.L.str7, 25

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%s "
	.size	.L.str8, 4

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	" %s"
	.size	.L.str9, 4

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"At top level:"
	.size	.L.str10, 14

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"In member function `%s':"
	.size	.L.str11, 25

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"In function `%s':"
	.size	.L.str12, 18

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"compilation terminated.\n"
	.size	.L.str13, 25

	.type	internal_error_function,@object # @internal_error_function
	.local	internal_error_function
	.comm	internal_error_function,8,8
	.type	diagnostic_lock,@object # @diagnostic_lock
	.local	diagnostic_lock
	.comm	diagnostic_lock,4,4
	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"%s:%d: confused by earlier errors, bailing out\n"
	.size	.L.str14, 48

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"It is possible that you may be trying to use SPEC's version of gcc \nwithout first defining the appropriate flags.  Please check the flags \nthat are in the config files from recently-published results on your \nplatform, and check that you are using an up-to-date compiler.  If\nyou still need help, please contact SPEC, reporting your hw/os \nplatform, your compiler version, and your compilation flags.\nContact SPEC at %s\n"
	.size	.L.str15, 421

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"<URL:http://www.spec.org/>"
	.size	.L.str16, 27

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Internal compiler error in %s, at %s:%d"
	.size	.L.str17, 40

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"In file included from %s:%d"
	.size	.L.str18, 28

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	",\n                 from %s:%d"
	.size	.L.str19, 30

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	":\n"
	.size	.L.str20, 3

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"`%s' is deprecated (declared at %s:%d)"
	.size	.L.str21, 39

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"`%s' is deprecated"
	.size	.L.str22, 19

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"type is deprecated (declared at %s:%d)"
	.size	.L.str23, 39

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"type is deprecated"
	.size	.L.str24, 19

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Internal compiler error: Error reporting routines re-entered.\n"
	.size	.L.str25, 63

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"diagnostic.c"
	.size	.L.str26, 13

	.type	.L__FUNCTION__.format_with_decl,@object # @__FUNCTION__.format_with_decl
.L__FUNCTION__.format_with_decl:
	.asciz	"format_with_decl"
	.size	.L__FUNCTION__.format_with_decl, 17

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"((anonymous))"
	.size	.L.str27, 14

	.type	.L__FUNCTION__.output_format,@object # @__FUNCTION__.output_format
.L__FUNCTION__.output_format:
	.asciz	"output_format"
	.size	.L__FUNCTION__.output_format, 14

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"%x"
	.size	.L.str28, 3

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"%lx"
	.size	.L.str29, 4

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"%u"
	.size	.L.str30, 3

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"%lu"
	.size	.L.str31, 4

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"%o"
	.size	.L.str32, 3

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"%lo"
	.size	.L.str33, 4

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"%ld"
	.size	.L.str34, 4


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
