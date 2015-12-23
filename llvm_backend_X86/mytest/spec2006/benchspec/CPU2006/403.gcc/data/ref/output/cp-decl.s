	.file	"cp-decl.i"
.globl language_string
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"GNU C++"
	.data
	.align 8
	.type	language_string,@object
	.size	language_string,8
language_string:
	.quad	.LC0
.globl no_print_functions
	.align 4
	.type	no_print_functions,@object
	.size	no_print_functions,4
no_print_functions:
	.long	0
.globl no_print_builtins
	.align 4
	.type	no_print_builtins,@object
	.size	no_print_builtins,4
no_print_builtins:
	.long	0
	.align 4
	.type	anon_cnt,@object
	.size	anon_cnt,4
anon_cnt:
	.long	0
	.section	.rodata.str1.1
.LC26:
	.string	"redeclaration of `%s'"
.LC23:
	.string	"redefinition of `%s'"
	.section	.rodata.str1.32,"aMS",@progbits,1
	.align 32
.LC25:
	.string	"conflicting declarations of `%s'"
	.section	.rodata.str1.1
.LC24:
	.string	"`%s' not declared in class"
	.text
	.align 2
	.p2align 4,,15
	.type	redeclaration_error_message,@function
redeclaration_error_message:
.LFB1:
	movzbl	16(%rdi), %eax
	cmpb	$32, %al
	je	.L169
	cmpb	$29, %al
	je	.L170
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	je	.L171
	testb	$1, 53(%rdi)
	je	.L166
	testb	$1, 53(%rsi)
	jne	.L165
.L166:
	movl	$.LC26, %ecx
.L147:
	movq	%rcx, %rax
	ret
	.p2align 6,,7
.L165:
	xorl	%ecx, %ecx
	jmp	.L147
	.p2align 6,,7
.L171:
	testb	$1, 53(%rdi)
	jne	.L165
	testb	$1, 53(%rsi)
	jne	.L165
	cmpq	$0, 88(%rsi)
	je	.L162
	cmpq	$0, 88(%rdi)
	je	.L162
	movl	$.LC23, %ecx
	jmp	.L147
.L162:
	movzbl	18(%rsi), %r9d
	xorl	%ecx, %ecx
	movl	$.LC25, %eax
	movzbl	18(%rdi), %esi
	shrb	$3, %r9b
	shrb	$3, %sil
	andl	$1, %r9d
	andl	$1, %esi
	cmpl	%esi, %r9d
	.p2align 4,,7
.L167:
	cmovne	%rax, %rcx
	jmp	.L147
	.p2align 6,,7
.L170:
	movq	152(%rsi), %rax
	testq	%rax, %rax
	je	.L153
	testb	$1, 2(%rax)
	jne	.L165
.L153:
	cmpq	$0, 88(%rsi)
	je	.L165
	cmpq	$0, 88(%rdi)
	je	.L165
	movzbl	53(%rsi), %edx
	andb	$9, %dl
	cmpb	$9, %dl
	je	.L172
.L155:
	movl	$.LC24, %ecx
	cmpq	$0, 56(%rsi)
	.p2align 4,,7
.L168:
	movl	$.LC23, %eax
	jmp	.L167
.L172:
	movzbl	53(%rdi), %ecx
	andb	$9, %cl
	cmpb	$9, %cl
	jne	.L165
	jmp	.L155
	.p2align 6,,7
.L169:
	xorl	%ecx, %ecx
	movq	8(%rdi), %rdx
	cmpq	%rdx, 8(%rsi)
	jmp	.L168
.LFE1:
.Lfe1:
	.size	redeclaration_error_message,.Lfe1-redeclaration_error_message
	.section	.rodata.str1.1
.LC27:
	.string	"C++"
.LC28:
	.string	"C"
.LC29:
	.string	"this"
.LC30:
	.string	"__in$chrg"
.LC45:
	.string	"char"
.LC46:
	.string	"long int"
.LC47:
	.string	"unsigned int"
.LC48:
	.string	"long unsigned int"
.LC49:
	.string	"unsigned long"
.LC50:
	.string	"short int"
.LC51:
	.string	"long long int"
.LC52:
	.string	"short unsigned int"
.LC53:
	.string	"unsigned short"
.LC54:
	.string	"long long unsigned int"
.LC55:
	.string	"long long unsigned"
.LC56:
	.string	"signed char"
.LC57:
	.string	"unsigned char"
.LC58:
	.string	"long double"
.LC59:
	.string	"__builtin_constant_p"
.LC61:
	.string	"alloca"
.LC60:
	.string	"__builtin_alloca"
.LC62:
	.string	"__builtin_abs"
.LC63:
	.string	"__builtin_fabs"
.LC64:
	.string	"__builtin_labs"
.LC65:
	.string	"__builtin_ffs"
.LC66:
	.string	"__builtin_fsqrt"
.LC67:
	.string	"__builtin_sin"
.LC68:
	.string	"__builtin_cos"
.LC69:
	.string	"__builtin_saveregs"
.LC70:
	.string	"__builtin_classify_type"
.LC71:
	.string	"__builtin_next_arg"
.LC73:
	.string	"memcpy"
.LC72:
	.string	"__builtin_memcpy"
.LC75:
	.string	"memcmp"
.LC74:
	.string	"__builtin_memcmp"
.LC77:
	.string	"strcmp"
.LC76:
	.string	"__builtin_strcmp"
.LC79:
	.string	"strcpy"
.LC78:
	.string	"__builtin_strcpy"
.LC81:
	.string	"strlen"
.LC80:
	.string	"__builtin_strlen"
.LC82:
	.string	"unknown type"
.LC83:
	.string	"__wchar_t"
.LC88:
	.string	"$vtbl_ptr_type"
.LC85:
	.string	"delta"
.LC86:
	.string	"index"
.LC87:
	.string	"pfn"
.LC89:
	.string	"delta2"
.LC113:
	.string	"abort"
.LC114:
	.string	"__unhandled_exception"
.LC90:
	.string	"name"
.LC91:
	.string	"size"
.LC92:
	.string	"bits"
.LC93:
	.string	"points_to"
.LC94:
	.string	"ivars_count"
.LC95:
	.string	"meths_count"
.LC96:
	.string	"ivars"
.LC97:
	.string	"meths"
.LC98:
	.string	"parents"
.LC99:
	.string	"vbases"
.LC100:
	.string	"offsets"
.LC101:
	.string	"__t_desc"
.LC102:
	.string	"offset"
.LC103:
	.string	"type"
.LC104:
	.string	"__i_desc"
.LC105:
	.string	"vindex"
.LC106:
	.string	"vcontext"
.LC107:
	.string	"return_type"
.LC108:
	.string	"address"
.LC109:
	.string	"parm_count"
.LC110:
	.string	"required_parms"
.LC111:
	.string	"parm_types"
.LC112:
	.string	"__m_desc"
	.section	.rodata.str1.32
	.align 32
.LC41:
	.string	"declaration of `%s' shadows a member of `this'"
	.section	.rodata.str1.1
.LC35:
	.string	"%s::%s"
	.section	.rodata.str1.32
	.align 32
.LC44:
	.string	"too many incomplete variables at this point"
	.align 32
.LC42:
	.string	"declaration of `%s' shadows previous local"
	.align 32
.LC43:
	.string	"declaration of `%s' shadows global declaration"
	.align 32
.LC40:
	.string	"declaration of `%s' shadows a parameter"
	.align 32
.LC39:
	.string	"extern declaration of `%s' doesn't match global one"
	.align 32
.LC38:
	.string	"`%s' was previously implicitly declared to return `int'"
	.align 32
.LC34:
	.string	"`%s' was declared `extern' and later `static'"
	.align 32
.LC33:
	.string	"`%s' was declared implicitly `extern' and later `static'"
	.align 32
.LC36:
	.string	"type mismatch with previous external decl"
	.align 32
.LC37:
	.string	"previous external decl of `%s'"
	.section	.rodata.str1.1
.LC6:
	.string	"previous declaration of `%s'"
	.section	.rodata.str1.32
	.align 32
.LC32:
	.string	"parse errors have confused me too much"
	.align 32
.LC31:
	.string	"`%s' used prior to declaration"
	.section	.rodata.str1.1
.LC84:
	.string	"__gc_main"
	.text
	.align 2
	.p2align 4,,15
.globl init_decl_processing
	.type	init_decl_processing,@function
init_decl_processing:
.LFB2:
	pushq	%rbp
.LCFI0:
	movl	$.LC27, %edi
	xorl	%eax, %eax
	movq	%rsp, %rbp
.LCFI1:
	pushq	%r15
.LCFI2:
	pushq	%r14
.LCFI3:
	pushq	%r13
.LCFI4:
	pushq	%r12
.LCFI5:
	pushq	%rbx
.LCFI6:
	subq	$2776, %rsp
.LCFI7:
	call	get_identifier
	movl	$.LC28, %edi
	movq	%rax, lang_name_cplusplus(%rip)
	xorl	%eax, %eax
	call	get_identifier
	movl	$sigsegv, %esi
	movl	$11, %edi
	movq	%rax, lang_name_c(%rip)
	movq	%rax, current_lang_name(%rip)
	movq	$0, current_function_decl(%rip)
	xorl	%eax, %eax
	movq	$0, named_labels(%rip)
	movq	$0, named_label_uses(%rip)
	movq	$0, current_binding_level(%rip)
	movq	$0, free_binding_level(%rip)
	call	signal
	xorl	%eax, %eax
	movl	$decl_obstack, %edi
	call	gcc_obstack_init
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	jne	.L11251
.L174:
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	make_node
	movq	%rax, error_mark_node(%rip)
	orb	$4, 17(%rax)
	xorl	%eax, %eax
	movq	error_mark_node(%rip), %rsi
	movq	%rsi, 8(%rsi)
	movq	%rsi, %rdi
	call	build_tree_list
	movq	global_binding_level(%rip), %rdx
	movq	error_mark_node(%rip), %rcx
	cmpq	%rdx, current_binding_level(%rip)
	movq	%rax, error_mark_list(%rip)
	movq	%rcx, 8(%rax)
	je	.L11252
.L175:
	movq	free_binding_level(%rip), %rdi
	testq	%rdi, %rdi
	je	.L176
	movq	56(%rdi), %rbx
	movq	%rbx, free_binding_level(%rip)
.L177:
	movq	clear_binding_level(%rip), %rdx
	movq	class_binding_level(%rip), %rax
	movl	keep_next_level_flag(%rip), %ecx
	movq	%rdx, (%rdi)
	testq	%rax, %rax
	movq	clear_binding_level+8(%rip), %r15
	movq	%r15, 8(%rdi)
	movq	clear_binding_level+16(%rip), %r13
	movq	%r13, 16(%rdi)
	movq	clear_binding_level+24(%rip), %r12
	movq	%r12, 24(%rdi)
	movq	clear_binding_level+32(%rip), %r11
	movq	%r11, 32(%rdi)
	movq	clear_binding_level+40(%rip), %r10
	movq	%r10, 40(%rdi)
	movq	clear_binding_level+48(%rip), %r9
	movq	%r9, 48(%rdi)
	movq	clear_binding_level+56(%rip), %r8
	movq	%r8, 56(%rdi)
	movq	clear_binding_level+64(%rip), %rsi
	movq	%rsi, 64(%rdi)
	je	.L178
	movq	%rax, 56(%rdi)
	movq	$0, class_binding_level(%rip)
.L179:
.L438:
.L453:
	movzbl	66(%rdi), %esi
	movl	%ecx, %r8d
	movq	%rdi, current_binding_level(%rip)
	andb	$7, %r8b
	orb	$5, 67(%rdi)
	xorl	%eax, %eax
	salb	$4, %r8b
	andb	$15, %sil
	orb	%r8b, %sil
	movb	%sil, 66(%rdi)
	call	GNU_xref_start_scope
	movq	current_binding_level(%rip), %rdi
	xorl	%eax, %eax
	movl	$0, keep_next_level_flag(%rip)
	movq	%rdi, global_binding_level(%rip)
	movl	$.LC29, %edi
	call	get_identifier
	movl	$.LC30, %edi
	movq	%rax, this_identifier(%rip)
	xorl	%eax, %eax
	call	get_identifier
	movl	$32, %edi
	movq	%rax, in_charge_identifier(%rip)
	xorl	%eax, %eax
	call	make_signed_type
	movq	ridpointers+8(%rip), %r12
	movq	%rax, %r13
	movq	%rax, integer_type_node(%rip)
	testq	%r12, %r12
	je	.L466
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L467
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L468
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
.L10924:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L469:
	movq	$0, 8(%r12)
	movq	%r14, 40(%r12)
.L466:
	movl	flag_dossier(%rip), %r14d
	testl	%r14d, %r14d
	je	.L481
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r9d
	cmpl	%ecx, %r9d
	jge	.L11253
.L478:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%r13, (%rcx)
	movzbl	16(%r13), %eax
	cmpb	$13, %al
	je	.L479
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rdi
	leal	1(%rbx), %r15d
	salq	$3, %rdi
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%r12
	leal	1(%r11), %r10d
	salq	$3, %r12
	movl	%r10d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
	movzbl	16(%r13), %eax
.L479:
	cmpb	$6, %al
	je	.L481
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rcx
	leal	1(%r9), %r14d
	salq	$3, %rcx
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r8
	leal	1(%rsi), %r13d
	salq	$3, %r8
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%rax, (%r8)
.L481:
	movl	flag_signed_char(%rip), %edx
	testl	%edx, %edx
	je	.L482
	movl	$8, %edi
	xorl	%eax, %eax
	call	make_signed_type
.L483:
	movl	$.LC45, %r10d
	movq	%rax, char_type_node(%rip)
	movq	%rax, -216(%rbp)
	testq	%r10, %r10
	movq	$0, -1664(%rbp)
	movq	ridpointers+16(%rip), %r15
	je	.L485
	movl	$.LC45, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1664(%rbp)
.L485:
	cmpq	$0, -1664(%rbp)
	jne	.L11254
.L486:
	testq	%r15, %r15
	je	.L768
	cmpq	$0, -1664(%rbp)
	je	.L769
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11255
.L10936:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L771:
	movq	$0, 8(%r15)
	movq	-1704(%rbp), %r11
	movq	%r11, 40(%r15)
.L768:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L783
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r15d
	cmpl	%ecx, %r15d
	jge	.L11256
.L780:
	movslq	%edx,%rcx
	movq	-216(%rbp), %rsi
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rsi, (%rcx)
	movzbl	16(%rsi), %eax
	cmpb	$13, %al
	je	.L781
	movq	%rsi, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %edi
	movq	-216(%rbp), %r14
	movslq	%edi,%r13
	leal	1(%rdi), %ebx
	salq	$3, %r13
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movq	-216(%rbp), %rsi
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r9
	salq	$3, %r9
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%rsi), %eax
.L781:
	cmpb	$6, %al
	je	.L783
	movq	-216(%rbp), %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movq	-216(%rbp), %r15
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%rcx
	leal	1(%r8), %esi
	salq	$3, %rcx
	movl	%esi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movl	$1, %esi
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%r11
	leal	1(%r12), %r10d
	salq	$3, %r11
	movl	%r10d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
.L783:
	xorl	%eax, %eax
	movl	$64, %edi
	call	make_signed_type
	movl	$.LC46, %edx
	movq	$0, -1720(%rbp)
	movq	%rax, %r14
	movq	%rax, long_integer_type_node(%rip)
	testq	%rdx, %rdx
	movq	ridpointers+112(%rip), %rax
	movq	%rax, -1712(%rbp)
	je	.L785
	movl	$.LC46, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1720(%rbp)
.L785:
	cmpq	$0, -1720(%rbp)
	jne	.L11257
.L786:
	cmpq	$0, -1712(%rbp)
	je	.L1068
	cmpq	$0, -1720(%rbp)
	je	.L1069
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1070
	movq	-1712(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L10948:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1071:
	movq	-1712(%rbp), %rdx
	movq	$0, 8(%rdx)
	movq	-1760(%rbp), %rsi
	movq	%rsi, 40(%rdx)
.L1068:
	movl	flag_dossier(%rip), %r15d
	testl	%r15d, %r15d
	je	.L1083
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r9d
	cmpl	%ecx, %r9d
	jge	.L11258
.L1080:
	movslq	%edx,%r13
	incl	%edx
	salq	$3, %r13
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%r14, (%r13)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L1081
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r8
	salq	$3, %r8
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%rax, (%r8)
	movq	56(%r14), %rdi
	leal	1(%rsi), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movl	$1, %esi
	xorl	%eax, %eax
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%rcx
	leal	1(%r12), %r10d
	salq	$3, %rcx
	movl	%r10d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movzbl	16(%r14), %eax
.L1081:
	cmpb	$6, %al
	je	.L1083
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rdi
	leal	1(%r9), %r15d
	salq	$3, %rdi
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rbx
	leal	1(%r11), %r14d
	salq	$3, %rbx
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rbx
	movq	%rax, (%rbx)
.L1083:
	xorl	%eax, %eax
	movl	$32, %edi
	call	make_unsigned_type
	movl	$.LC47, %edx
	movq	ridpointers+96(%rip), %r13
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, unsigned_type_node(%rip)
	movq	$0, -1776(%rbp)
	movq	%r13, -1768(%rbp)
	je	.L1085
	movl	$.LC47, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1776(%rbp)
.L1085:
	cmpq	$0, -1776(%rbp)
	jne	.L11259
.L1086:
	cmpq	$0, -1768(%rbp)
	je	.L1368
	cmpq	$0, -1776(%rbp)
	je	.L1369
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1370
	movq	-1768(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L10960:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1371:
	movq	-1768(%rbp), %rdi
	movq	$0, 8(%rdi)
	movq	-1816(%rbp), %rsi
	movq	%rsi, 40(%rdi)
.L1368:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L1383
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r15d
	cmpl	%ecx, %r15d
	jge	.L11260
.L1380:
	movslq	%edx,%r10
	incl	%edx
	salq	$3, %r10
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%r14, (%r10)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L1381
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%rsi
	leal	1(%rdi), %r11d
	salq	$3, %rsi
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	xorl	%eax, %eax
	movl	$1, %esi
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%rcx
	leal	1(%r12), %r13d
	salq	$3, %rcx
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movzbl	16(%r14), %eax
.L1381:
	cmpb	$6, %al
	je	.L1383
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r15d
	movslq	%r15d,%rbx
	salq	$3, %rbx
	addq	builtin_type_tdescs_arr(%rip), %rbx
	movq	%rax, (%rbx)
	movq	64(%r14), %rdi
	leal	1(%r15), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r9
	leal	1(%r8), %r14d
	salq	$3, %r9
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
.L1383:
	xorl	%eax, %eax
	movl	$64, %edi
	call	make_unsigned_type
	movl	$.LC48, %edx
	movq	$0, -1824(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, long_unsigned_type_node(%rip)
	je	.L1385
	movl	$.LC48, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1824(%rbp)
.L1385:
	cmpq	$0, -1824(%rbp)
	jne	.L11261
.L1386:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L1668
	cmpq	$0, -1824(%rbp)
	je	.L1669
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1670
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L10972:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1671:
	movq	$0, 8
	movq	-1864(%rbp), %rdi
	movq	%rdi, 40
.L1668:
	movl	flag_dossier(%rip), %r11d
	testl	%r11d, %r11d
	je	.L1683
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r9d
	cmpl	%ecx, %r9d
	jge	.L11262
.L1680:
	movslq	%edx,%rsi
	incl	%edx
	salq	$3, %rsi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%r14, (%rsi)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L1681
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%r13
	leal	1(%r10), %ebx
	salq	$3, %r13
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r15d
	movslq	%r15d,%rcx
	leal	1(%r15), %r8d
	salq	$3, %rcx
	movl	%r8d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movzbl	16(%r14), %eax
.L1681:
	cmpb	$6, %al
	je	.L1683
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rsi
	leal	1(%r9), %r11d
	salq	$3, %rsi
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	xorl	%eax, %eax
	movl	$1, %esi
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%rdi
	leal	1(%r12), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
.L1683:
	movl	$.LC49, %edx
	movq	long_unsigned_type_node(%rip), %r15
	movq	$0, -1872(%rbp)
	testq	%rdx, %rdx
	je	.L1685
	movl	$.LC49, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1872(%rbp)
.L1685:
	cmpq	$0, -1872(%rbp)
	jne	.L11263
.L1686:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L1968
	cmpq	$0, -1872(%rbp)
	je	.L1969
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1970
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L10984:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1971:
	movq	$0, 8
	movq	-1912(%rbp), %rcx
	movq	%rcx, 40
.L1968:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L1983
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %ebx
	cmpl	%ecx, %ebx
	jge	.L11264
.L1980:
	movslq	%edx,%rsi
	incl	%edx
	salq	$3, %rsi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%r15, (%rsi)
	movzbl	16(%r15), %eax
	cmpb	$13, %al
	je	.L1981
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rdi
	leal	1(%r9), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r11
	salq	$3, %r11
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r15), %eax
.L1981:
	cmpb	$6, %al
	je	.L1983
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rcx
	leal	1(%rbx), %r12d
	salq	$3, %rcx
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r10
	leal	1(%r13), %r15d
	salq	$3, %r10
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
.L1983:
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	je	.L1984
	movq	long_integer_type_node(%rip), %rax
.L10986:
	movq	%rax, sizetype(%rip)
	movl	$.LC46, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	long_integer_type_node(%rip), %r14
	movq	long_unsigned_type_node(%rip), %rdi
	movq	40(%rax), %rsi
	movq	integer_type_node(%rip), %rcx
	xorl	%eax, %eax
	movq	sizetype(%rip), %r11
	movq	char_type_node(%rip), %r12
	movq	unsigned_type_node(%rip), %r13
	movq	8(%rsi), %rdx
	movq	32(%rcx), %rbx
	movq	%rdx, ptrdiff_type_node(%rip)
	movq	%r11, 8(%rbx)
	movq	32(%r12), %r10
	movq	%r11, 8(%r10)
	movq	32(%r13), %r15
	movq	%r11, 8(%r15)
	movq	32(%rdi), %r9
	movl	$16, %edi
	movq	%r11, 8(%r9)
	movq	32(%r14), %r8
	movq	%r11, 8(%r8)
	call	make_signed_type
	movl	$.LC50, %esi
	movq	$0, -1928(%rbp)
	movq	%rax, %r14
	movq	%rax, short_integer_type_node(%rip)
	testq	%rsi, %rsi
	movq	ridpointers+104(%rip), %rax
	movq	%rax, -1920(%rbp)
	je	.L1987
	movl	$.LC50, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1928(%rbp)
.L1987:
	cmpq	$0, -1928(%rbp)
	jne	.L11265
.L1988:
	cmpq	$0, -1920(%rbp)
	je	.L2270
	cmpq	$0, -1928(%rbp)
	je	.L2271
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2272
	movq	-1920(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L10997:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2273:
	movq	-1920(%rbp), %rsi
	movq	$0, 8(%rsi)
	movq	-1968(%rbp), %rdx
	movq	%rdx, 40(%rsi)
.L2270:
	movl	flag_dossier(%rip), %r15d
	testl	%r15d, %r15d
	je	.L2285
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %ebx
	cmpl	%ecx, %ebx
	jge	.L11266
.L2282:
	movslq	%edx,%r13
	incl	%edx
	salq	$3, %r13
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%r14, (%r13)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L2283
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%rcx
	leal	1(%r8), %esi
	salq	$3, %rcx
	movl	%esi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movl	$1, %esi
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%r10
	leal	1(%r12), %r9d
	salq	$3, %r10
	movl	%r9d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	movzbl	16(%r14), %eax
.L2283:
	cmpb	$6, %al
	je	.L2285
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rdi
	salq	$3, %rdi
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	movq	64(%r14), %rdi
	leal	1(%rbx), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%r15
	leal	1(%r11), %r14d
	salq	$3, %r15
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
.L2285:
	xorl	%eax, %eax
	movl	$64, %edi
	call	make_signed_type
	movl	$.LC51, %edx
	movq	$0, -1976(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, long_long_integer_type_node(%rip)
	je	.L2287
	movl	$.LC51, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -1976(%rbp)
.L2287:
	cmpq	$0, -1976(%rbp)
	jne	.L11267
.L2288:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L2570
	cmpq	$0, -1976(%rbp)
	je	.L2571
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2572
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11009:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2573:
	movq	$0, 8
	movq	-2016(%rbp), %r11
	movq	%r11, 40
.L2570:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L2585
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %esi
	cmpl	%ecx, %esi
	jge	.L11268
.L2582:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%r14, (%rcx)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L2583
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%r13
	leal	1(%r9), %ebx
	salq	$3, %r13
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r15
	salq	$3, %r15
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r14), %eax
.L2583:
	cmpb	$6, %al
	je	.L2585
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%rdi
	leal	1(%rsi), %r11d
	movl	$1, %esi
	salq	$3, %rdi
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%r12
	leal	1(%r10), %r14d
	salq	$3, %r12
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
.L2585:
	xorl	%eax, %eax
	movl	$16, %edi
	call	make_unsigned_type
	movl	$.LC52, %edx
	movq	$0, -2024(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, short_unsigned_type_node(%rip)
	je	.L2587
	movl	$.LC52, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2024(%rbp)
.L2587:
	cmpq	$0, -2024(%rbp)
	jne	.L11269
.L2588:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L2870
	cmpq	$0, -2024(%rbp)
	je	.L2871
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2872
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11021:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2873:
	movq	$0, 8
	movq	-2064(%rbp), %rcx
	movq	%rcx, 40
.L2870:
	movl	flag_dossier(%rip), %edi
	testl	%edi, %edi
	je	.L2885
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r10d
	cmpl	%ecx, %r10d
	jge	.L11270
.L2882:
	movslq	%edx,%r8
	incl	%edx
	salq	$3, %r8
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%r14, (%r8)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L2883
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%r13
	leal	1(%r11), %ebx
	salq	$3, %r13
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r15
	salq	$3, %r15
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
	leal	1(%rsi), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r14), %eax
.L2883:
	cmpb	$6, %al
	je	.L2885
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%rcx
	leal	1(%r10), %edi
	salq	$3, %rcx
	movl	%edi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%r12
	leal	1(%r9), %r14d
	salq	$3, %r12
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
.L2885:
	movl	$.LC53, %edx
	movq	short_unsigned_type_node(%rip), %r15
	movq	$0, -2072(%rbp)
	testq	%rdx, %rdx
	je	.L2887
	movl	$.LC53, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2072(%rbp)
.L2887:
	cmpq	$0, -2072(%rbp)
	jne	.L11271
.L2888:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L3170
	cmpq	$0, -2072(%rbp)
	je	.L3171
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3172
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11033:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3173:
	movq	$0, 8
	movq	-2112(%rbp), %r9
	movq	%r9, 40
.L3170:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L3185
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %esi
	cmpl	%ecx, %esi
	jge	.L11272
.L3182:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%r15, (%rcx)
	movzbl	16(%r15), %eax
	cmpb	$13, %al
	je	.L3183
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rdi
	leal	1(%rbx), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r10
	salq	$3, %r10
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r15), %eax
.L3183:
	cmpb	$6, %al
	je	.L3185
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rsi
	leal	1(%r9), %r12d
	salq	$3, %rsi
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	xorl	%eax, %eax
	movl	$1, %esi
	movq	64(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r11
	leal	1(%r13), %r15d
	salq	$3, %r11
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
.L3185:
	xorl	%eax, %eax
	movl	$64, %edi
	call	make_unsigned_type
	movl	$.LC54, %edx
	movq	$0, -2120(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, long_long_unsigned_type_node(%rip)
	je	.L3187
	movl	$.LC54, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2120(%rbp)
.L3187:
	cmpq	$0, -2120(%rbp)
	jne	.L11273
.L3188:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L3470
	cmpq	$0, -2120(%rbp)
	je	.L3471
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3472
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11045:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3473:
	movq	$0, 8
	movq	-2160(%rbp), %r10
	movq	%r10, 40
.L3470:
	movl	flag_dossier(%rip), %r11d
	testl	%r11d, %r11d
	je	.L3485
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %esi
	cmpl	%ecx, %esi
	jge	.L11274
.L3482:
	movslq	%edx,%r8
	incl	%edx
	salq	$3, %r8
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%r14, (%r8)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L3483
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%r13
	leal	1(%rdi), %ebx
	salq	$3, %r13
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r15d
	movslq	%r15d,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	leal	1(%r15), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r14), %eax
.L3483:
	cmpb	$6, %al
	je	.L3485
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rsi
	leal	1(%r11), %r10d
	salq	$3, %rsi
	movl	%r10d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	xorl	%eax, %eax
	movl	$1, %esi
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%r12
	leal	1(%r9), %r14d
	salq	$3, %r12
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
.L3485:
	movl	$.LC55, %edx
	movq	long_long_unsigned_type_node(%rip), %r15
	movq	$0, -2168(%rbp)
	testq	%rdx, %rdx
	je	.L3487
	movl	$.LC55, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2168(%rbp)
.L3487:
	cmpq	$0, -2168(%rbp)
	jne	.L11275
.L3488:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L3770
	cmpq	$0, -2168(%rbp)
	je	.L3771
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3772
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11057:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3773:
	movq	$0, 8
	movq	-2208(%rbp), %rdi
	movq	%rdi, 40
.L3770:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L3785
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r9d
	cmpl	%ecx, %r9d
	jge	.L11276
.L3782:
	movslq	%edx,%r11
	incl	%edx
	salq	$3, %r11
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%r15, (%r11)
	movzbl	16(%r15), %eax
	cmpb	$13, %al
	je	.L3783
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rcx
	leal	1(%rbx), %r14d
	salq	$3, %rcx
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	56(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%rsi
	salq	$3, %rsi
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r15), %eax
.L3783:
	cmpb	$6, %al
	je	.L3785
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%r10
	leal	1(%r9), %edi
	salq	$3, %r10
	movl	%edi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	xorl	%eax, %eax
	movq	64(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r12
	leal	1(%r13), %r15d
	salq	$3, %r12
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
.L3785:
	xorl	%eax, %eax
	movl	$8, %edi
	call	make_signed_type
	movl	$.LC56, %edx
	movq	$0, -2216(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, signed_char_type_node(%rip)
	je	.L3787
	movl	$.LC56, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2216(%rbp)
.L3787:
	cmpq	$0, -2216(%rbp)
	jne	.L11277
.L3788:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L4070
	cmpq	$0, -2216(%rbp)
	je	.L4071
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L4072
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11069:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4073:
	movq	$0, 8
	movq	-2256(%rbp), %rdi
	movq	%rdi, 40
.L4070:
	movl	flag_dossier(%rip), %ebx
	testl	%ebx, %ebx
	je	.L4085
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r10d
	cmpl	%ecx, %r10d
	jge	.L11278
.L4082:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%r14, (%rcx)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L4083
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r9
	leal	1(%r13), %r11d
	salq	$3, %r9
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r15
	leal	1(%rsi), %r8d
	salq	$3, %r15
	movl	%r8d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
	movzbl	16(%r14), %eax
.L4083:
	cmpb	$6, %al
	je	.L4085
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%rcx
	leal	1(%r10), %ebx
	salq	$3, %rcx
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%rdi
	leal	1(%r12), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
.L4085:
	xorl	%eax, %eax
	movl	$8, %edi
	call	make_unsigned_type
	movl	$.LC57, %edx
	movq	$0, -2264(%rbp)
	testq	%rdx, %rdx
	movq	%rax, %r14
	movq	%rax, unsigned_char_type_node(%rip)
	je	.L4087
	movl	$.LC57, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2264(%rbp)
.L4087:
	cmpq	$0, -2264(%rbp)
	jne	.L11279
.L4088:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L4370
	cmpq	$0, -2264(%rbp)
	je	.L4371
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L4372
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11081:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4373:
	movq	$0, 8
	movq	-2304(%rbp), %r9
	movq	%r9, 40
.L4370:
	movl	flag_dossier(%rip), %ecx
	testl	%ecx, %ecx
	je	.L4385
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %ebx
	cmpl	%ecx, %ebx
	jge	.L11280
.L4382:
	movslq	%edx,%r8
	incl	%edx
	salq	$3, %r8
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%r14, (%r8)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L4383
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r11
	leal	1(%r13), %edi
	salq	$3, %r11
	movl	%edi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r15
	salq	$3, %r15
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
	leal	1(%rsi), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r14), %eax
.L4383:
	cmpb	$6, %al
	je	.L4385
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%rcx
	leal	1(%r10), %ebx
	salq	$3, %rcx
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%r9
	leal	1(%r12), %r14d
	salq	$3, %r9
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
.L4385:
	movl	mode_size+4(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_signed_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, %rdx
	movq	%rax, intQI_type_node(%rip)
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %rdx
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%rdx, -2336(%rbp)
	cmpq	%rax, %r13
	je	.L10549
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11281
.L4387:
	movq	%rax, 64(%r13)
.L4386:
	cmpb	$32, %dl
	je	.L11282
.L4388:
	testq	%r14, %r14
	je	.L4389
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11283
	cmpq	$0, 48(%r14)
	jne	.L4392
	movq	$0, -2312(%rbp)
.L4391:
	cmpq	$0, -2312(%rbp)
	je	.L4409
	movq	-2312(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L11284
.L4399:
	cmpq	$0, -2312(%rbp)
	je	.L10553
	movq	-2312(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11285
.L4401:
	movq	-2312(%rbp), %r15
	testq	%r15, %r15
	movq	24(%r15), %r12
	movq	%r15, %rsi
	movl	32(%r15), %ebx
	je	.L10553
	movzbl	16(%r15), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L4403
	cmpb	$32, %al
	je	.L4409
	cmpb	$32, %dl
	je	.L10911
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10553
	.p2align 4,,7
.L4408:
	movl	mode_size+8(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_signed_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, intHI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r9
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r9, -2368(%rbp)
	cmpq	%rax, %r13
	je	.L10564
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11286
.L4663:
	movq	%rax, 64(%r13)
.L4662:
	cmpb	$32, %dl
	je	.L11287
.L4664:
	testq	%r14, %r14
	je	.L4665
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11288
	cmpq	$0, 48(%r14)
	jne	.L4668
	movq	$0, -2344(%rbp)
.L4667:
	cmpq	$0, -2344(%rbp)
	je	.L4685
	movq	-2344(%rbp), %r10
	cmpq	error_mark_node(%rip), %r10
	je	.L11289
.L4675:
	cmpq	$0, -2344(%rbp)
	je	.L10568
	movq	-2344(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11290
.L4677:
	movq	-2344(%rbp), %r8
	testq	%r8, %r8
	movq	24(%r8), %r12
	movq	%r8, %rsi
	movl	32(%r8), %ebx
	je	.L10568
	movzbl	16(%r8), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L4679
	cmpb	$32, %al
	je	.L4685
	cmpb	$32, %dl
	je	.L10912
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10568
	.p2align 4,,7
.L4684:
	movl	mode_size+16(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_signed_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, intSI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r11
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r11, -2400(%rbp)
	cmpq	%rax, %r13
	je	.L10579
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11291
.L4939:
	movq	%rax, 64(%r13)
.L4938:
	cmpb	$32, %dl
	je	.L11292
.L4940:
	testq	%r14, %r14
	je	.L4941
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11293
	cmpq	$0, 48(%r14)
	jne	.L4944
	movq	$0, -2376(%rbp)
.L4943:
	cmpq	$0, -2376(%rbp)
	je	.L4961
	movq	-2376(%rbp), %r9
	cmpq	error_mark_node(%rip), %r9
	je	.L11294
.L4951:
	cmpq	$0, -2376(%rbp)
	je	.L10583
	movq	-2376(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11295
.L4953:
	movq	-2376(%rbp), %rdx
	testq	%rdx, %rdx
	movq	24(%rdx), %r12
	movq	%rdx, %rsi
	movl	32(%rdx), %ebx
	je	.L10583
	movzbl	16(%rdx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L4955
	cmpb	$32, %al
	je	.L4961
	cmpb	$32, %dl
	je	.L10913
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10583
	.p2align 4,,7
.L4960:
	movl	mode_size+24(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_signed_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, intDI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %rbx
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%rbx, -2432(%rbp)
	cmpq	%rax, %r13
	je	.L10594
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11296
.L5215:
	movq	%rax, 64(%r13)
.L5214:
	cmpb	$32, %dl
	je	.L11297
.L5216:
	testq	%r14, %r14
	je	.L5217
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11298
	cmpq	$0, 48(%r14)
	jne	.L5220
	movq	$0, -2408(%rbp)
.L5219:
	cmpq	$0, -2408(%rbp)
	je	.L5237
	movq	-2408(%rbp), %r10
	cmpq	error_mark_node(%rip), %r10
	je	.L11299
.L5227:
	cmpq	$0, -2408(%rbp)
	je	.L10598
	movq	-2408(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11300
.L5229:
	movq	-2408(%rbp), %rdx
	testq	%rdx, %rdx
	movq	24(%rdx), %r12
	movq	%rdx, %rsi
	movl	32(%rdx), %ebx
	je	.L10598
	movzbl	16(%rdx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L5231
	cmpb	$32, %al
	je	.L5237
	cmpb	$32, %dl
	je	.L10914
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10598
	.p2align 4,,7
.L5236:
	movl	mode_size+4(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_unsigned_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, unsigned_intQI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r11
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r11, -2464(%rbp)
	cmpq	%rax, %r13
	je	.L10609
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11301
.L5491:
	movq	%rax, 64(%r13)
.L5490:
	cmpb	$32, %dl
	je	.L11302
.L5492:
	testq	%r14, %r14
	je	.L5493
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11303
	cmpq	$0, 48(%r14)
	jne	.L5496
	movq	$0, -2440(%rbp)
.L5495:
	cmpq	$0, -2440(%rbp)
	je	.L5513
	movq	-2440(%rbp), %r9
	cmpq	error_mark_node(%rip), %r9
	je	.L11304
.L5503:
	cmpq	$0, -2440(%rbp)
	je	.L10613
	movq	-2440(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11305
.L5505:
	movq	-2440(%rbp), %rdx
	testq	%rdx, %rdx
	movq	24(%rdx), %r12
	movq	%rdx, %rsi
	movl	32(%rdx), %ebx
	je	.L10613
	movzbl	16(%rdx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L5507
	cmpb	$32, %al
	je	.L5513
	cmpb	$32, %dl
	je	.L10915
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10613
	.p2align 4,,7
.L5512:
	movl	mode_size+8(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_unsigned_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, unsigned_intHI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r10
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r10, -2496(%rbp)
	cmpq	%rax, %r13
	je	.L10624
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11306
.L5767:
	movq	%rax, 64(%r13)
.L5766:
	cmpb	$32, %dl
	je	.L11307
.L5768:
	testq	%r14, %r14
	je	.L5769
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11308
	cmpq	$0, 48(%r14)
	jne	.L5772
	movq	$0, -2472(%rbp)
.L5771:
	cmpq	$0, -2472(%rbp)
	je	.L5789
	movq	-2472(%rbp), %r8
	cmpq	error_mark_node(%rip), %r8
	je	.L11309
.L5779:
	cmpq	$0, -2472(%rbp)
	je	.L10628
	movq	-2472(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11310
.L5781:
	movq	-2472(%rbp), %r11
	testq	%r11, %r11
	movq	24(%r11), %r12
	movq	%r11, %rsi
	movl	32(%r11), %ebx
	je	.L10628
	movzbl	16(%r11), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L5783
	cmpb	$32, %al
	je	.L5789
	cmpb	$32, %dl
	je	.L10916
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10628
	.p2align 4,,7
.L5788:
	movl	mode_size+16(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_unsigned_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, unsigned_intSI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r9
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r9, -2528(%rbp)
	cmpq	%rax, %r13
	je	.L10639
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11311
.L6043:
	movq	%rax, 64(%r13)
.L6042:
	cmpb	$32, %dl
	je	.L11312
.L6044:
	testq	%r14, %r14
	je	.L6045
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11313
	cmpq	$0, 48(%r14)
	jne	.L6048
	movq	$0, -2504(%rbp)
.L6047:
	cmpq	$0, -2504(%rbp)
	je	.L6065
	movq	-2504(%rbp), %r10
	cmpq	error_mark_node(%rip), %r10
	je	.L11314
.L6055:
	cmpq	$0, -2504(%rbp)
	je	.L10643
	movq	-2504(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11315
.L6057:
	movq	-2504(%rbp), %rdx
	testq	%rdx, %rdx
	movq	24(%rdx), %r12
	movq	%rdx, %rsi
	movl	32(%rdx), %ebx
	je	.L10643
	movzbl	16(%rdx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L6059
	cmpb	$32, %al
	je	.L6065
	cmpb	$32, %dl
	je	.L10917
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10643
	.p2align 4,,7
.L6064:
	movl	mode_size+24(%rip), %edi
	xorl	%eax, %eax
	sall	$3, %edi
	call	make_unsigned_type
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%rax, unsigned_intDI_type_node(%rip)
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %rbx
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%rbx, -2560(%rbp)
	cmpq	%rax, %r13
	je	.L10654
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11316
.L6319:
	movq	%rax, 64(%r13)
.L6318:
	cmpb	$32, %dl
	je	.L11317
.L6320:
	testq	%r14, %r14
	je	.L6321
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11318
	cmpq	$0, 48(%r14)
	jne	.L6324
	movq	$0, -2536(%rbp)
.L6323:
	cmpq	$0, -2536(%rbp)
	je	.L6341
	movq	-2536(%rbp), %r8
	cmpq	error_mark_node(%rip), %r8
	je	.L11319
.L6331:
	cmpq	$0, -2536(%rbp)
	je	.L10658
	movq	-2536(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11320
.L6333:
	movq	-2536(%rbp), %rdx
	testq	%rdx, %rdx
	movq	24(%rdx), %r12
	movq	%rdx, %rsi
	movl	32(%rdx), %ebx
	je	.L10658
	movzbl	16(%rdx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L6335
	cmpb	$32, %al
	je	.L6341
	cmpb	$32, %dl
	je	.L10918
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10658
	.p2align 4,,7
.L6340:
.L6850:
.L6865:
	movl	$8, %edi
	xorl	%eax, %eax
	call	make_node
	movq	%rax, float_type_node(%rip)
	movb	$32, 45(%rax)
	movq	ridpointers+24(%rip), %r12
	movq	float_type_node(%rip), %r13
	testq	%r12, %r12
	je	.L6878
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L6879
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L6880
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
.L11163:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6881:
	movq	$0, 8(%r12)
	movq	-2568(%rbp), %r9
	movq	%r9, 40(%r12)
.L6878:
	movl	flag_dossier(%rip), %ebx
	testl	%ebx, %ebx
	je	.L6893
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r10d
	cmpl	%ecx, %r10d
	jge	.L11321
.L6890:
	movslq	%edx,%rsi
	incl	%edx
	salq	$3, %rsi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%r13, (%rsi)
	movzbl	16(%r13), %eax
	cmpb	$13, %al
	je	.L6891
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r14d
	movslq	%r14d,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movq	56(%r13), %rdi
	leal	1(%r14), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%r8
	leal	1(%r12), %r15d
	salq	$3, %r8
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%rax, (%r8)
	movzbl	16(%r13), %eax
.L6891:
	cmpb	$6, %al
	je	.L6893
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%r10
	leal	1(%rbx), %r9d
	salq	$3, %r10
	movl	%r9d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	xorl	%eax, %eax
	movq	64(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%r11
	leal	1(%rdi), %r13d
	salq	$3, %r11
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
.L6893:
	movq	float_type_node(%rip), %rdi
	xorl	%eax, %eax
	call	layout_type
	xorl	%eax, %eax
	movl	$8, %edi
	call	make_node
	movl	flag_short_double(%rip), %edx
	movq	%rax, double_type_node(%rip)
	testl	%edx, %edx
	je	.L6894
	movb	$32, 45(%rax)
.L6895:
.L7152:
.L7167:
	movq	double_type_node(%rip), %r13
	movq	ridpointers+32(%rip), %r12
	testq	%r12, %r12
	je	.L7180
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L7181
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L7182
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
.L11165:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7183:
	movq	$0, 8(%r12)
	movq	-2576(%rbp), %rsi
	movq	%rsi, 40(%r12)
.L7180:
	movl	flag_dossier(%rip), %r15d
	testl	%r15d, %r15d
	je	.L7195
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r12d
	cmpl	%ecx, %r12d
	jge	.L11322
.L7192:
	movslq	%edx,%r14
	incl	%edx
	salq	$3, %r14
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r14
	movq	%r13, (%r14)
	movzbl	16(%r13), %eax
	cmpb	$13, %al
	je	.L7193
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rbx
	leal	1(%r9), %r11d
	salq	$3, %rbx
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rbx
	movq	%rax, (%rbx)
	xorl	%eax, %eax
	movq	56(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	leal	1(%rdi), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r13), %eax
.L7193:
	cmpb	$6, %al
	je	.L7195
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r12d
	movslq	%r12d,%r8
	leal	1(%r12), %r15d
	salq	$3, %r8
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%rax, (%r8)
	xorl	%eax, %eax
	movq	64(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%rsi
	leal	1(%r10), %r13d
	salq	$3, %rsi
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
.L7195:
	movq	double_type_node(%rip), %rdi
	xorl	%eax, %eax
	call	layout_type
	xorl	%eax, %eax
	movl	$8, %edi
	call	make_node
	movl	$.LC58, %edx
	testq	%rdx, %rdx
	movq	%rax, long_double_type_node(%rip)
	movb	$64, 45(%rax)
	movq	$0, -2584(%rbp)
	movq	long_double_type_node(%rip), %r15
	je	.L7197
	movl	$.LC58, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2584(%rbp)
.L7197:
	cmpq	$0, -2584(%rbp)
	jne	.L11323
.L7198:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L7480
	cmpq	$0, -2584(%rbp)
	je	.L7481
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L7482
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11177:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7483:
	movq	$0, 8
	movq	-2624(%rbp), %rcx
	movq	%rcx, 40
.L7480:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L7495
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %ebx
	cmpl	%ecx, %ebx
	jge	.L11324
.L7492:
	movslq	%edx,%rsi
	incl	%edx
	salq	$3, %rsi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%r15, (%rsi)
	movzbl	16(%r15), %eax
	cmpb	$13, %al
	je	.L7493
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rdi
	leal	1(%r11), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r10
	salq	$3, %r10
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r15), %eax
.L7493:
	cmpb	$6, %al
	je	.L7495
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rcx
	leal	1(%rbx), %r12d
	salq	$3, %rcx
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%r15), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r9
	leal	1(%r13), %r15d
	salq	$3, %r9
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
.L7495:
.L7752:
.L7767:
	movq	long_double_type_node(%rip), %rdi
	xorl	%eax, %eax
	call	layout_type
	xorl	%edi, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_int_2_wide
	xorl	%esi, %esi
	movl	$1, %edi
	movq	integer_type_node(%rip), %r14
	movq	%rax, integer_zero_node(%rip)
	movq	%r14, 8(%rax)
	xorl	%eax, %eax
	call	build_int_2_wide
	xorl	%esi, %esi
	movl	$2, %edi
	movq	integer_type_node(%rip), %r10
	movq	%rax, integer_one_node(%rip)
	movq	%r10, 8(%rax)
	xorl	%eax, %eax
	call	build_int_2_wide
	xorl	%esi, %esi
	movl	$3, %edi
	movq	integer_type_node(%rip), %r8
	movq	%rax, integer_two_node(%rip)
	movq	%r8, 8(%rax)
	xorl	%eax, %eax
	call	build_int_2_wide
	xorl	%edx, %edx
	movl	$43, %edi
	movq	integer_type_node(%rip), %rsi
	movq	%rax, integer_three_node(%rip)
	movq	%rsi, 8(%rax)
	xorl	%eax, %eax
	xorl	%esi, %esi
	call	build_nt
	xorl	%edi, %edi
	movq	%rax, empty_init_node(%rip)
	xorl	%eax, %eax
	call	size_int
	movl	$1, %edi
	movq	%rax, size_zero_node(%rip)
	xorl	%eax, %eax
	call	size_int
	movl	$6, %edi
	movq	%rax, size_one_node(%rip)
	xorl	%eax, %eax
	call	make_node
	movq	ridpointers+40(%rip), %r12
	movq	%rax, %r13
	movq	%rax, void_type_node(%rip)
	testq	%r12, %r12
	je	.L7780
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L7781
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L7782
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
.L11179:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7783:
	movq	$0, 8(%r12)
	movq	-2632(%rbp), %rdx
	movq	%rdx, 40(%r12)
.L7780:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L7795
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r11d
	cmpl	%ecx, %r11d
	jge	.L11325
.L7792:
	movslq	%edx,%rdi
	incl	%edx
	salq	$3, %rdi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%r13, (%rdi)
	movzbl	16(%r13), %eax
	cmpb	$13, %al
	je	.L7793
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%rcx
	leal	1(%rsi), %ebx
	movl	$1, %esi
	salq	$3, %rcx
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	56(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%r12
	leal	1(%r9), %r15d
	salq	$3, %r12
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
	movzbl	16(%r13), %eax
.L7793:
	cmpb	$6, %al
	je	.L7795
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rdi
	leal	1(%r11), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	64(%r13), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r10
	leal	1(%r8), %r13d
	salq	$3, %r10
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
.L7795:
.L8052:
.L8067:
	movq	void_type_node(%rip), %rdi
	xorl	%eax, %eax
	call	layout_type
	movq	void_type_node(%rip), %rsi
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	build_tree_list
	xorl	%esi, %esi
	xorl	%edi, %edi
	movq	%rax, void_list_node(%rip)
	orb	$64, 17(%rax)
	xorl	%eax, %eax
	call	build_int_2_wide
	movq	void_type_node(%rip), %rdi
	movq	%rax, %r12
	movq	%rax, null_pointer_node(%rip)
	xorl	%eax, %eax
	call	build_pointer_type
	movq	null_pointer_node(%rip), %rdx
	movq	%rax, 8(%r12)
	xorl	%eax, %eax
	movq	8(%rdx), %rdi
	call	layout_type
	xorl	%esi, %esi
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	build_int_2_wide
	movq	char_type_node(%rip), %rdi
	movq	void_type_node(%rip), %r15
	movq	%rax, void_zero_node(%rip)
	movq	%r15, 8(%rax)
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movq	char_type_node(%rip), %rdi
	movq	%rax, string_type_node(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	movq	string_type_node(%rip), %rbx
	movq	%rax, const_string_type_node(%rip)
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L8095
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r9d
	cmpl	%ecx, %r9d
	jge	.L11326
.L8092:
	movslq	%edx,%rsi
	incl	%edx
	salq	$3, %rsi
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rbx, (%rsi)
	movzbl	16(%rbx), %eax
	cmpb	$13, %al
	je	.L8093
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r14d
	movslq	%r14d,%r11
	leal	1(%r14), %r10d
	salq	$3, %r11
	movl	%r10d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
	xorl	%eax, %eax
	movq	56(%rbx), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%rcx
	leal	1(%r8), %r13d
	salq	$3, %rcx
	movl	%r13d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movzbl	16(%rbx), %eax
.L8093:
	cmpb	$6, %al
	je	.L8095
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r9d
	movslq	%r9d,%rsi
	leal	1(%r9), %r12d
	salq	$3, %rsi
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rsi
	movq	%rax, (%rsi)
	xorl	%eax, %eax
	movl	$1, %esi
	movq	64(%rbx), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%r15
	leal	1(%rdi), %ebx
	salq	$3, %r15
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
.L8095:
.L8352:
.L8367:
	movq	char_type_node(%rip), %rdi
	movq	unsigned_char_type_node(%rip), %rsi
	xorl	%eax, %eax
	call	build_array_type
	movq	unsigned_char_type_node(%rip), %rsi
	movq	integer_type_node(%rip), %rdi
	movq	%rax, char_array_type_node(%rip)
	xorl	%eax, %eax
	call	build_array_type
	movl	$21, %edi
	movq	%rax, int_array_type_node(%rip)
	xorl	%eax, %eax
	call	make_lang_type
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%esi, %esi
	movq	integer_type_node(%rip), %rdi
	movq	%rax, class_star_type_node(%rip)
	xorl	%eax, %eax
	call	build_function_type
	movq	%rax, %rdi
	movq	%rax, default_function_type(%rip)
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%eax, %eax
	movq	void_type_node(%rip), %rdi
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movq	void_type_node(%rip), %rdi
	movq	%rax, ptr_type_node(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	movq	ptr_type_node(%rip), %rbx
	movq	%rax, const_ptr_type_node(%rip)
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L8395
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r13d
	cmpl	%ecx, %r13d
	jge	.L11327
.L8392:
	movslq	%edx,%r8
	incl	%edx
	salq	$3, %r8
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r8
	movq	%rbx, (%r8)
	movzbl	16(%rbx), %eax
	cmpb	$13, %al
	je	.L8393
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rdi
	leal	1(%r11), %r14d
	salq	$3, %rdi
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%rbx), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r10d
	movslq	%r10d,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	leal	1(%r10), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%rbx), %eax
.L8393:
	cmpb	$6, %al
	je	.L8395
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r13
	leal	1(%rsi), %r9d
	movl	$1, %esi
	salq	$3, %r13
	movl	%r9d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	64(%rbx), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r15d
	movslq	%r15d,%r12
	leal	1(%r15), %ebx
	salq	$3, %r12
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r12
	movq	%rax, (%r12)
.L8395:
	movq	void_list_node(%rip), %r13
	movq	integer_type_node(%rip), %rsi
	xorl	%edi, %edi
	xorl	%eax, %eax
	movq	%r13, %rdx
	call	tree_cons
	movq	%r13, %rdx
	movq	double_type_node(%rip), %rsi
	movq	%rax, %r14
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%r13, %rdx
	movq	ptr_type_node(%rip), %rsi
	movq	%rax, %r15
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	xorl	%eax, %eax
	movq	%r15, %rsi
	movq	double_type_node(%rip), %rdi
	call	build_function_type
	movq	%r15, %rdx
	movq	double_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, double_ftype_double(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	double_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r14, %rsi
	movq	integer_type_node(%rip), %rdi
	movq	%rax, double_ftype_double_double(%rip)
	xorl	%eax, %eax
	call	build_function_type
	movq	%r13, %rdx
	movq	long_integer_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, int_ftype_int(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	long_integer_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r14, %rdx
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, long_ftype_long(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	void_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r13, %rdx
	movq	sizetype(%rip), %rsi
	movq	%rax, void_ftype_ptr_ptr_int(%rip)
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	const_ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	const_ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	integer_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r14, %rdx
	movq	integer_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, int_ftype_cptr_cptr_sizet(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	void_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r13, %rdx
	movq	const_string_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, void_ftype_ptr_int_int(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	string_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	string_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r13, %rdx
	movq	const_string_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, string_ftype_ptr_ptr(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	const_string_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	integer_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movq	%r13, %rdx
	movq	const_string_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, int_ftype_string_string(%rip)
	xorl	%eax, %eax
	call	tree_cons
	movq	sizetype(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movl	flag_traditional(%rip), %edx
	movq	%rax, sizet_ftype_string(%rip)
	testl	%edx, %edx
	je	.L8396
	movq	string_type_node(%rip), %rbx
.L8397:
.L8654:
.L8669:
	movq	sizetype(%rip), %rsi
	xorl	%edi, %edi
	movq	%r13, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	const_ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	xorl	%esi, %esi
	movq	ptr_type_node(%rip), %rdi
	movq	%rax, %r12
	xorl	%eax, %eax
	call	build_array_type
	movq	%rax, %rdi
	movq	%rax, vtbl_type_node(%rip)
	xorl	%eax, %eax
	call	layout_type
	movq	vtbl_type_node(%rip), %rdi
	movl	$1, %esi
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	build_type_variant
	movq	%rax, %rbx
	movq	%rax, vtbl_type_node(%rip)
	movl	flag_dossier(%rip), %r8d
	testl	%r8d, %r8d
	je	.L8697
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r10d
	cmpl	%ecx, %r10d
	jge	.L11328
.L8694:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rbx, (%rcx)
	movzbl	16(%rbx), %eax
	cmpb	$13, %al
	je	.L8695
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %r14d
	movslq	%r14d,%r15
	leal	1(%r14), %esi
	salq	$3, %r15
	movl	%esi, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r15
	movl	$1, %esi
	movq	%rax, (%r15)
	xorl	%eax, %eax
	movq	56(%rbx), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%r9
	leal	1(%rdi), %r11d
	salq	$3, %r9
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
	movzbl	16(%rbx), %eax
.L8695:
	cmpb	$6, %al
	je	.L8697
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r11d
	movslq	%r11d,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	movq	64(%rbx), %rdi
	leal	1(%r11), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	xorl	%eax, %eax
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r10
	leal	1(%r8), %ebx
	salq	$3, %r10
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
.L8697:
	movq	int_ftype_int(%rip), %rsi
	xorl	%r8d, %r8d
	movl	$pushdecl, %ecx
	movl	$.LC59, %edi
	movl	$27, %edx
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	movq	%r13, %rdx
	movq	sizetype(%rip), %rsi
	xorl	%edi, %edi
	call	tree_cons
	movq	ptr_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	movl	$.LC61, %r8d
	movl	$pushdecl, %ecx
	movq	%rax, %rsi
	movl	$.LC60, %edi
	movl	$1, %edx
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	int_ftype_int(%rip), %rsi
	movl	$.LC62, %edi
	movl	$2, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	double_ftype_double(%rip), %rsi
	movl	$.LC63, %edi
	movl	$3, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	long_ftype_long(%rip), %rsi
	movl	$.LC64, %edi
	movl	$4, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	int_ftype_int(%rip), %rsi
	movl	$.LC65, %edi
	movl	$5, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	double_ftype_double(%rip), %rsi
	movl	$.LC66, %edi
	movl	$18, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	double_ftype_double(%rip), %rsi
	movl	$.LC67, %edi
	movl	$19, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	double_ftype_double(%rip), %rsi
	movl	$20, %edx
	movl	$pushdecl, %ecx
	movl	$.LC68, %edi
	call	define_function
	xorl	%eax, %eax
	xorl	%esi, %esi
	movq	ptr_type_node(%rip), %rdi
	call	build_function_type
	xorl	%r8d, %r8d
	movl	$pushdecl, %ecx
	movq	%rax, %rsi
	movl	$.LC69, %edi
	movl	$23, %edx
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	default_function_type(%rip), %rsi
	movl	$24, %edx
	movl	$pushdecl, %ecx
	movl	$.LC70, %edi
	call	define_function
	xorl	%eax, %eax
	movq	%r13, %rsi
	movq	ptr_type_node(%rip), %rdi
	call	build_function_type
	xorl	%r8d, %r8d
	movl	$pushdecl, %ecx
	movq	%rax, %rsi
	movl	$.LC71, %edi
	movl	$25, %edx
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	movl	$.LC73, %r8d
	movq	%r12, %rsi
	movl	$.LC72, %edi
	movl	$12, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	movl	$.LC75, %r8d
	movq	int_ftype_cptr_cptr_sizet(%rip), %rsi
	movl	$.LC74, %edi
	movl	$13, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	movl	$.LC77, %r8d
	movq	int_ftype_string_string(%rip), %rsi
	movl	$.LC76, %edi
	movl	$16, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	movl	$.LC79, %r8d
	movq	string_ftype_ptr_ptr(%rip), %rsi
	movl	$.LC78, %edi
	movl	$15, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	movl	$.LC81, %r8d
	movq	sizet_ftype_string(%rip), %rsi
	movl	$.LC80, %edi
	movl	$17, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	%r12, %rsi
	movl	$.LC73, %edi
	movl	$12, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	int_ftype_cptr_cptr_sizet(%rip), %rsi
	movl	$.LC75, %edi
	movl	$13, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	int_ftype_string_string(%rip), %rsi
	movl	$.LC77, %edi
	movl	$16, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	string_ftype_ptr_ptr(%rip), %rsi
	movl	$.LC79, %edi
	movl	$15, %edx
	movl	$pushdecl, %ecx
	call	define_function
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movq	sizet_ftype_string(%rip), %rsi
	movl	$17, %edx
	movl	$pushdecl, %ecx
	movl	$.LC81, %edi
	call	define_function
	xorl	%eax, %eax
	movl	$24, %edi
	call	make_node
	movl	$.LC82, %edi
	movq	%rax, unknown_type_node(%rip)
	xorl	%eax, %eax
	call	get_identifier
	movq	unknown_type_node(%rip), %rdx
	movl	$32, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_decl
	movq	current_binding_level(%rip), %r12
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r12, -2664(%rbp)
	cmpq	%rax, %r13
	je	.L10802
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11329
.L8699:
	movq	%rax, 64(%r13)
.L8698:
	cmpb	$32, %dl
	je	.L11330
.L8700:
	testq	%r14, %r14
	je	.L8701
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11331
	cmpq	$0, 48(%r14)
	jne	.L8704
	movq	$0, -2640(%rbp)
.L8703:
	cmpq	$0, -2640(%rbp)
	je	.L8721
	movq	-2640(%rbp), %rdx
	cmpq	error_mark_node(%rip), %rdx
	je	.L11332
.L8711:
	cmpq	$0, -2640(%rbp)
	je	.L10806
	movq	-2640(%rbp), %rsi
	cmpb	$34, 16(%rsi)
	je	.L11333
.L8713:
	movq	-2640(%rbp), %rax
	testq	%rax, %rax
	movq	24(%rax), %r12
	movl	32(%rax), %ebx
	je	.L10806
	movzbl	16(%rax), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L8715
	cmpb	$32, %al
	je	.L8721
	cmpb	$32, %dl
	je	.L10920
	movq	-2640(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10806
	.p2align 4,,7
.L8720:
	movq	void_type_node(%rip), %r12
	movq	unknown_type_node(%rip), %r10
	movl	$.LC52, %edi
	xorl	%eax, %eax
	movq	32(%r12), %rsi
	movl	$1, 48(%r10)
	movq	%rsi, 32(%r10)
	movzbl	44(%r12), %edx
	movb	%dl, 44(%r10)
	movq	unknown_type_node(%rip), %r8
	movq	%r8, 8(%r8)
	movq	%r8, 56(%r8)
	movq	%r8, 64(%r8)
	call	get_identifier
	movq	40(%rax), %rdi
	xorl	%eax, %eax
	movq	8(%rdi), %rbx
	movzbl	45(%rbx), %r15d
	movq	%rbx, wchar_type_node(%rip)
	movl	%r15d, %edi
	call	make_signed_type
	movl	%r15d, %edi
	movq	%rax, signed_wchar_type_node(%rip)
	xorl	%eax, %eax
	call	make_unsigned_type
	movq	wchar_type_node(%rip), %r13
	movq	%rax, unsigned_wchar_type_node(%rip)
	testb	$64, 17(%r13)
	jne	.L8975
	movq	signed_wchar_type_node(%rip), %rax
.L8975:
	movl	$.LC83, %r11d
	movq	%rax, wchar_type_node(%rip)
	movq	%rax, -1512(%rbp)
	testq	%r11, %r11
	movq	$0, -2672(%rbp)
	movq	ridpointers+192(%rip), %r15
	je	.L8977
	movl	$.LC83, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2672(%rbp)
.L8977:
	cmpq	$0, -2672(%rbp)
	jne	.L11334
.L8978:
	testq	%r15, %r15
	je	.L9260
	cmpq	$0, -2672(%rbp)
	je	.L9261
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11335
.L11201:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9263:
	movq	$0, 8(%r15)
	movq	-2712(%rbp), %rdi
	movq	%rdi, 40(%r15)
.L9260:
	movl	flag_dossier(%rip), %eax
	testl	%eax, %eax
	je	.L9275
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r15d
	cmpl	%ecx, %r15d
	jge	.L11336
.L9272:
	movslq	%edx,%rcx
	movq	-1512(%rbp), %rsi
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rsi, (%rcx)
	movzbl	16(%rsi), %eax
	cmpb	$13, %al
	je	.L9273
	movq	%rsi, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %r9d
	movq	-1512(%rbp), %r14
	movslq	%r9d,%r13
	leal	1(%r9), %r11d
	salq	$3, %r13
	movl	%r11d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r13
	movq	%rax, (%r13)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movq	-1512(%rbp), %rsi
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r10
	salq	$3, %r10
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%rsi), %eax
.L9273:
	cmpb	$6, %al
	je	.L9275
	movq	-1512(%rbp), %rdi
	xorl	%eax, %eax
	call	build_reference_type
	movq	-1512(%rbp), %rdx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%rcx
	leal	1(%rsi), %ebx
	movl	$1, %esi
	salq	$3, %rcx
	movl	%ebx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%rax, (%rcx)
	xorl	%eax, %eax
	movq	64(%rdx), %rdi
	xorl	%edx, %edx
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %edi
	movslq	%edi,%r15
	leal	1(%rdi), %r12d
	salq	$3, %r15
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r15
	movq	%rax, (%r15)
.L9275:
	movq	wchar_type_node(%rip), %rdi
	movq	unsigned_char_type_node(%rip), %rsi
	xorl	%eax, %eax
	call	build_array_type
	movq	%rax, wchar_array_type_node(%rip)
	movl	flag_gc(%rip), %eax
	testl	%eax, %eax
	jne	.L11337
.L9276:
	movl	$21, %edi
	xorl	%eax, %eax
	movl	$.LC88, %r13d
	call	make_lang_type
	movl	$.LC85, %edi
	movq	%rax, vtable_entry_type(%rip)
	xorl	%eax, %eax
	call	get_identifier
	movq	short_integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC86, %edi
	movq	%rax, -208(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	short_integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC87, %edi
	movq	%rax, -200(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	ptr_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movq	double_type_node(%rip), %r8
	movl	$2, %ecx
	movq	vtable_entry_type(%rip), %rdi
	movl	$.LC88, %esi
	leaq	-208(%rbp), %rdx
	movq	%rax, -192(%rbp)
	xorl	%eax, %eax
	call	finish_builtin_type
	movq	-192(%rbp), %rdi
	xorl	%eax, %eax
	call	copy_node
	movl	$.LC89, %edi
	movq	short_integer_type_node(%rip), %r11
	movq	%rax, -184(%rbp)
	movq	%r11, 8(%rax)
	xorl	%eax, %eax
	movq	-184(%rbp), %r14
	call	get_identifier
	movq	short_integer_type_node(%rip), %rdx
	movq	%rax, 56(%r14)
	xorl	%eax, %eax
	movq	-184(%rbp), %r12
	movzbl	44(%rdx), %r10d
	xorl	%edx, %edx
	movb	%r10b, 52(%r12)
	movq	-184(%rbp), %rsi
	movq	short_integer_type_node(%rip), %r15
	movq	32(%r15), %rcx
	movq	%rcx, 40(%rsi)
	movl	$1, %esi
	movq	-184(%rbp), %r8
	andb	$-65, 17(%r8)
	movq	-184(%rbp), %rdi
	movq	-192(%rbp), %rbx
	movq	%rdi, (%rbx)
	movq	vtable_entry_type(%rip), %rdi
	call	build_type_variant
	testq	%r13, %r13
	movq	$0, -2760(%rbp)
	movq	%rax, %r14
	movq	%rax, vtable_entry_type(%rip)
	je	.L9572
	movl	$.LC88, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, -2760(%rbp)
.L9572:
	cmpq	$0, -2760(%rbp)
	jne	.L11338
.L9573:
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.L9855
	cmpq	$0, -2760(%rbp)
	je	.L9856
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L9857
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
.L11222:
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9858:
	movq	$0, 8
	movq	-2800(%rbp), %rsi
	movq	%rsi, 40
.L9855:
	movl	flag_dossier(%rip), %edx
	testl	%edx, %edx
	je	.L9871
	movl	builtin_type_tdescs_len(%rip), %edx
	movl	builtin_type_tdescs_max(%rip), %ecx
	leal	5(%rdx), %r10d
	cmpl	%ecx, %r10d
	jge	.L11339
.L9867:
	movslq	%edx,%rcx
	incl	%edx
	salq	$3, %rcx
	movl	%edx, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	%r14, (%rcx)
	movzbl	16(%r14), %eax
	cmpb	$13, %al
	je	.L9868
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	xorl	%edx, %edx
	movl	$1, %esi
	movl	builtin_type_tdescs_len(%rip), %ebx
	movslq	%ebx,%rdi
	leal	1(%rbx), %r15d
	salq	$3, %rdi
	movl	%r15d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %rdi
	movq	%rax, (%rdi)
	xorl	%eax, %eax
	movq	56(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r8d
	movslq	%r8d,%r11
	salq	$3, %r11
	addq	builtin_type_tdescs_arr(%rip), %r11
	movq	%rax, (%r11)
	leal	1(%r8), %eax
	movl	%eax, builtin_type_tdescs_len(%rip)
	movzbl	16(%r14), %eax
.L9868:
	cmpb	$6, %al
	je	.L10871
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_reference_type
	xorl	%edx, %edx
	movl	builtin_type_tdescs_len(%rip), %esi
	movslq	%esi,%r10
	leal	1(%rsi), %r12d
	movl	$1, %esi
	salq	$3, %r10
	movl	%r12d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r10
	movq	%rax, (%r10)
	xorl	%eax, %eax
	movq	64(%r14), %rdi
	call	build_type_variant
	movl	builtin_type_tdescs_len(%rip), %r13d
	movslq	%r13d,%r9
	leal	1(%r13), %r14d
	salq	$3, %r9
	movl	%r14d, builtin_type_tdescs_len(%rip)
	addq	builtin_type_tdescs_arr(%rip), %r9
	movq	%rax, (%r9)
.L10871:
	movl	flag_dossier(%rip), %edx
	testl	%edx, %edx
	jne	.L11340
.L9871:
	movq	lang_name_cplusplus(%rip), %rdi
	testl	%edx, %edx
	movq	%rdi, current_lang_name(%rip)
	je	.L9872
	movl	builtin_type_tdescs_len(%rip), %ebx
	testl	%ebx, %ebx
	jle	.L9872
	.p2align 4,,7
.L9876:
	decl	%ebx
	xorl	%esi, %esi
	xorl	%eax, %eax
	movslq	%ebx,%rcx
	salq	$3, %rcx
	addq	builtin_type_tdescs_arr(%rip), %rcx
	movq	(%rcx), %rdi
	call	build_t_desc
	movq	32(%rax), %rdx
	orb	$-128, 17(%rax)
	orb	$8, 18(%rdx)
	testl	%ebx, %ebx
	jg	.L9876
.L9872:
	movq	void_list_node(%rip), %rdx
	movq	sizetype(%rip), %rsi
	xorl	%edi, %edi
	xorl	%eax, %eax
	movq	ansi_opname+1016(%rip), %rbx
	call	tree_cons
	movq	ptr_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	24(%rax), %rsi
	movq	%rax, %r13
	xorl	%eax, %eax
	call	build_decl_overload
	movl	$push_overloaded_decl_1, %ecx
	xorl	%edx, %edx
	movq	32(%rax), %r8
	movq	32(%rbx), %rdi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	movq	void_list_node(%rip), %rdx
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	ansi_opname+984(%rip), %r14
	call	tree_cons
	movq	void_type_node(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_function_type
	xorl	%edx, %edx
	movq	%r14, %rdi
	movq	24(%rax), %rsi
	movq	%rax, %r15
	xorl	%eax, %eax
	call	build_decl_overload
	movl	$push_overloaded_decl_1, %ecx
	xorl	%edx, %edx
	movq	32(%rax), %r8
	movq	32(%r14), %rdi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	define_function
	xorl	%eax, %eax
	movq	void_list_node(%rip), %rsi
	movq	void_type_node(%rip), %rdi
	call	build_function_type
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	movq	%rax, %rsi
	movl	$.LC113, %edi
	xorl	%eax, %eax
	call	define_function
	xorl	%esi, %esi
	movq	void_type_node(%rip), %rdi
	movq	%rax, abort_fndecl(%rip)
	xorl	%eax, %eax
	call	build_function_type
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	movq	%rax, %rsi
	movl	$.LC114, %edi
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	call	define_function
	movq	%rax, unhandled_exception_fndecl(%rip)
	xorl	%eax, %eax
	call	init_class_processing
	xorl	%eax, %eax
	call	init_init_processing
	xorl	%eax, %eax
	call	init_search_processing
	movl	flag_handle_exceptions(%rip), %eax
	testl	%eax, %eax
	je	.L9879
	cmpl	$2, %eax
	je	.L11341
.L9880:
	xorl	%eax, %eax
	call	init_exception_processing
.L9879:
	movl	flag_gc(%rip), %eax
	testl	%eax, %eax
	jne	.L11342
.L9881:
	movl	flag_no_inline(%rip), %r8d
	testl	%r8d, %r8d
	je	.L9882
	movl	$0, flag_inline_functions(%rip)
	movl	$0, flag_default_inline(%rip)
.L9882:
	movl	flag_cadillac(%rip), %eax
	testl	%eax, %eax
	jne	.L11343
.L9883:
	xorl	%eax, %eax
	call	declare_function_name
	movl	$1, warn_return_type(%rip)
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	leave
	ret
.L11343:
	xorl	%eax, %eax
	call	init_cadillac
	jmp	.L9883
.L11342:
	xorl	%eax, %eax
	call	init_gc_processing
	jmp	.L9881
.L11341:
	movl	$2, flag_this_is_variable(%rip)
	jmp	.L9880
.L11340:
	movl	$21, %edi
	xorl	%eax, %eax
	call	make_lang_type
	movl	$21, %edi
	movq	%rax, __t_desc_type_node(%rip)
	xorl	%eax, %eax
	call	make_lang_type
	movl	$21, %edi
	movq	%rax, __i_desc_type_node(%rip)
	xorl	%eax, %eax
	call	make_lang_type
	xorl	%esi, %esi
	movq	__t_desc_type_node(%rip), %r10
	movq	%rax, __m_desc_type_node(%rip)
	xorl	%eax, %eax
	movq	56(%r10), %rdi
	call	build_array_type
	movq	__i_desc_type_node(%rip), %rsi
	movq	%rax, __t_desc_array_type(%rip)
	xorl	%eax, %eax
	movq	56(%rsi), %rdi
	xorl	%esi, %esi
	call	build_array_type
	xorl	%esi, %esi
	movq	__m_desc_type_node(%rip), %r9
	movq	%rax, __i_desc_array_type(%rip)
	xorl	%eax, %eax
	movq	56(%r9), %rdi
	call	build_array_type
	movl	$.LC90, %edi
	movq	%rax, __m_desc_array_type(%rip)
	xorl	%eax, %eax
	call	get_identifier
	movq	string_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC91, %edi
	movq	%rax, -208(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	unsigned_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC92, %edi
	movq	%rax, -200(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	unsigned_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC93, %edi
	movq	%rax, -192(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_type_node(%rip), %rbx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	movq	56(%rbx), %rdx
	call	build_lang_field_decl
	movl	$.LC94, %edi
	movq	%rax, -184(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC95, %edi
	movq	%rax, -176(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC96, %edi
	movq	%rax, -168(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__i_desc_array_type(%rip), %rdi
	movq	%rax, %r13
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r13, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC97, %edi
	movq	%rax, -160(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__m_desc_array_type(%rip), %rdi
	movq	%rax, %r14
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r14, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC98, %edi
	movq	%rax, -152(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_array_type(%rip), %rdi
	movq	%rax, %r15
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r15, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC99, %edi
	movq	%rax, -144(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_array_type(%rip), %rdi
	movq	%rax, %r12
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r12, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC100, %edi
	movq	%rax, -136(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	integer_type_node(%rip), %rdi
	movq	%rax, %r13
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r13, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movq	integer_type_node(%rip), %r8
	movl	$10, %ecx
	movq	__t_desc_type_node(%rip), %rdi
	movl	$.LC101, %esi
	leaq	-208(%rbp), %rdx
	movq	%rax, -128(%rbp)
	xorl	%eax, %eax
	call	finish_builtin_type
	movl	$.LC90, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	string_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC102, %edi
	movq	%rax, -208(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC103, %edi
	movq	%rax, -200(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_type_node(%rip), %r11
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	movq	56(%r11), %rdx
	call	build_lang_field_decl
	movq	integer_type_node(%rip), %r8
	movl	$2, %ecx
	movq	__i_desc_type_node(%rip), %rdi
	movl	$.LC104, %esi
	leaq	-208(%rbp), %rdx
	movq	%rax, -192(%rbp)
	xorl	%eax, %eax
	call	finish_builtin_type
	movl	$.LC90, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	string_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC105, %edi
	movq	%rax, -208(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC106, %edi
	movq	%rax, -200(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_type_node(%rip), %r8
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	movq	56(%r8), %rdx
	call	build_lang_field_decl
	movl	$.LC107, %edi
	movq	%rax, -192(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	__t_desc_type_node(%rip), %rcx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	movq	56(%rcx), %rdx
	call	build_lang_field_decl
	movl	$.LC108, %edi
	movq	%rax, -184(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	default_function_type(%rip), %rdi
	movq	%rax, %r14
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r14, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC109, %edi
	movq	%rax, -176(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	short_integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC110, %edi
	movq	%rax, -168(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	movq	short_integer_type_node(%rip), %rdx
	movl	$36, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movl	$.LC111, %edi
	movq	%rax, -160(%rbp)
	xorl	%eax, %eax
	call	get_identifier
	xorl	%esi, %esi
	movq	__t_desc_type_node(%rip), %rdx
	movq	%rax, %r15
	xorl	%eax, %eax
	movq	56(%rdx), %rdi
	call	build_array_type
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%r15, %rsi
	movl	$36, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_lang_field_decl
	movq	integer_type_node(%rip), %r8
	movl	$7, %ecx
	movq	__m_desc_type_node(%rip), %rdi
	movq	%rax, -152(%rbp)
	leaq	-208(%rbp), %rdx
	movl	$.LC112, %esi
	xorl	%eax, %eax
	call	finish_builtin_type
	movl	flag_dossier(%rip), %edx
	jmp	.L9871
.L11339:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L9867
.L9857:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9858
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11222
.L9856:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11344
.L11223:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9863:
	movq	$0, 8
	jmp	.L9855
.L11344:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9863
	jmp	.L11223
	.p2align 6,,7
.L11338:
	movq	-2760(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_binding_level(%rip), %rax
	movq	%rax, -2792(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10852
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11345
.L9575:
	movq	%rax, 64(%r13)
.L9574:
	cmpb	$32, %dl
	je	.L11346
.L9576:
	testq	%r15, %r15
	je	.L9577
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11347
	cmpq	$0, 48(%r15)
	jne	.L9580
	movq	$0, -2800(%rbp)
.L9579:
	cmpq	$0, -2800(%rbp)
	je	.L9597
	movq	-2800(%rbp), %r9
	cmpq	error_mark_node(%rip), %r9
	je	.L11348
.L9587:
	cmpq	$0, -2800(%rbp)
	je	.L10856
	movq	-2800(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11349
.L9589:
	movq	-2800(%rbp), %rdi
	testq	%rdi, %rdi
	movq	24(%rdi), %r12
	movq	%rdi, %rsi
	movl	32(%rdi), %ebx
	je	.L10856
	movzbl	16(%rdi), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L9591
	cmpb	$32, %al
	je	.L9597
	cmpb	$32, %dl
	je	.L10923
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10856
.L10860:
	movq	global_binding_level(%rip), %rax
.L9596:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L9850
	movq	-2760(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11221:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9851:
	movq	-2760(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L9573
	movq	-2800(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L9573
.L9850:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9851
	movq	-2760(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11221
.L10856:
	movzbl	16(%r13), %edx
.L9597:
	cmpb	$32, %dl
	je	.L10923
.L9605:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L9743
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L9743
	testb	$1, 53(%rax)
	jne	.L9744
	testb	$8, 18(%rax)
	je	.L9743
.L9744:
	andb	$8, %dl
	je	.L11350
.L9743:
	movl	flag_traditional(%rip), %ecx
	testl	%ecx, %ecx
	je	.L10868
	testb	$1, 53(%r13)
	je	.L10868
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L9747
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L9748
.L9747:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9749
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10215
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9750
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11351
.L9750:
	testq	%rcx, %rcx
	jne	.L10215
.L10216:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10215
.L9749:
	movq	40(%r15), %rcx
.L9748:
	testq	%rcx, %rcx
	je	.L10282
.L10215:
	cmpb	$32, 16(%rcx)
	je	.L9752
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L9752
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L9760
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11218
	testl	%ebx, %ebx
	jle	.L11352
.L11218:
	movq	%rax, %rcx
.L9752:
	testq	%rcx, %rcx
	jne	.L10868
.L10282:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2792(%rbp)
.L9746:
	cmpq	%rax, -2792(%rbp)
	je	.L11353
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L11354
.L11219:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L9791:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L11355
.L9802:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L9804
	testq	%r12, %r12
	je	.L9805
	testb	$1, 53(%r13)
	jne	.L9805
	cmpb	$34, 16(%r12)
	je	.L11356
.L9805:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L9804
	testb	$1, 53(%r13)
	jne	.L9804
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L9804
	testq	%rax, %rax
	jne	.L9804
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L9810
	cmpb	$34, 16(%r12)
	je	.L11357
.L9810:
	cmpq	$0, 56(%r15)
	je	.L9812
	movl	$.LC41, %edi
.L9811:
	testq	%rdi, %rdi
	jne	.L11220
.L9804:
	testq	%r12, %r12
	je	.L10869
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2792(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10869:
	movzbl	16(%r13), %edx
.L9789:
	leal	-128(%rdx), %esi
	cmpb	$1, %sil
	jbe	.L9577
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L9577
	cmpb	$18, 16(%rcx)
	je	.L11358
.L9821:
	testb	$64, 46(%rcx)
	je	.L9577
.L9820:
	movq	-2792(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11359
.L10870:
	movzbl	16(%r13), %edx
.L9577:
	cmpb	$32, %dl
	je	.L11360
.L9823:
	movq	-2792(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rdi
	cmpq	%rax, %rbx
	movq	%rdi, (%r13)
	movq	%r13, (%rbx)
	je	.L11361
.L9849:
	movq	%r13, -2800(%rbp)
	jmp	.L9596
.L11361:
	testb	$4, 17(%r13)
	jne	.L9849
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L9849
.L11360:
	testq	%r15, %r15
	je	.L9823
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9824
	cmpq	class_binding_level(%rip), %rax
	je	.L9825
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L9829
	cmpb	$32, 16(%rax)
	je	.L9827
.L9829:
	cmpq	$0, current_class_type(%rip)
	je	.L9824
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L9824
	cmpb	$32, 16(%rax)
	je	.L9827
.L9824:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L9828
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9827
	cmpb	$-127, %dl
	je	.L11362
.L9828:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L9823
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11363
.L9835:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9839
	cmpq	class_binding_level(%rip), %rax
	je	.L9840
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L9844
	cmpb	$32, 16(%rax)
	je	.L9842
.L9844:
	cmpq	$0, current_class_type(%rip)
	je	.L9839
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L9839
	cmpb	$32, 16(%rax)
	je	.L9842
.L9839:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L9823
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9842
	cmpb	$-127, %dl
	jne	.L9823
	movq	$0, 8(%rbx)
	jmp	.L9823
.L9842:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L9823
.L9840:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9844
.L11363:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r11b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L9835
.L11362:
	movq	$0, 8(%r15)
	jmp	.L9828
.L9827:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r15)
	jmp	.L9828
.L9825:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9829
.L11359:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10870
.L11358:
	movq	8(%rcx), %r10
	testb	$64, 46(%r10)
	jne	.L9820
	jmp	.L9821
.L11220:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L9804
.L9812:
	testq	%r12, %r12
	je	.L9814
	movl	$.LC42, %edi
	jmp	.L9811
.L9814:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L9811
.L11357:
	movl	$.LC40, %edi
	jmp	.L9811
.L11356:
	cmpb	$34, 16(%r13)
	je	.L9805
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L9806
	movq	56(%rax), %rax
.L9806:
	movzbl	66(%rax), %r11d
	andl	$15, %r11d
	decl	%r11d
	jne	.L9804
	movl	$.LC40, %edi
	jmp	.L11220
	.p2align 6,,7
.L11355:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11364
.L9794:
	testq	%r12, %r12
	jne	.L9802
	testq	%r8, %r8
	jne	.L9802
	testb	$1, 53(%r13)
	je	.L9802
	testb	$8, 18(%r13)
	je	.L9802
	orb	$8, 18(%r15)
	jmp	.L9802
	.p2align 6,,7
.L11364:
	testq	%r8, %r8
	je	.L9794
	cmpb	$29, 16(%r13)
	jne	.L9794
	cmpb	$29, 16(%r8)
	jne	.L9794
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L11365
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L9797
	movzbl	53(%r13), %esi
	leal	0(,%rax,8), %ecx
	leaq	88(%r13), %rdx
	andb	$-9, %sil
	orb	%cl, %sil
	movb	%sil, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L9798
	movq	88(%r8), %rax
.L9799:
	movq	%rax, (%rdx)
	movq	136(%r8), %rsi
	movq	80(%r8), %r9
	movq	72(%r8), %rdx
	movzbl	17(%r13), %r10d
	movq	%rsi, 136(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %r10b
	shrb	$7, %dil
	movzbl	%dil, %ebx
	movl	%ebx, %r11d
	salb	$7, %r11b
	orb	%r11b, %r10b
	movb	%r10b, 17(%r13)
	movzbl	53(%r8), %ecx
.L9797:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L9800
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L9800:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L9794
	cmpq	$0, 88(%r8)
	je	.L9794
	movq	8(%r13), %r10
	cmpq	$0, 24(%r10)
	jne	.L9794
	movq	%rdx, 8(%r13)
	jmp	.L9794
.L9798:
	xorl	%eax, %eax
	jmp	.L9799
.L11365:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L9794
	.p2align 6,,7
.L11354:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9791
	jmp	.L11219
.L11353:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11366
.L9766:
	cmpq	$0, 40(%r15)
	jne	.L9767
	testb	$8, 18(%r13)
	je	.L9767
	orb	$8, 18(%r15)
.L9767:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11367
.L9769:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L9768:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L9780
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9770
	testb	$1, 18(%rcx)
	je	.L9770
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L9770:
	testq	%rax, %rax
	je	.L9780
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9775
	testb	$8, 17(%rcx)
	je	.L9775
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L9775:
	testq	%rax, %rax
	je	.L9780
	cmpq	$0, 8(%rax)
	je	.L9780
	cmpb	$29, %dl
	je	.L11368
.L9783:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L9780:
	testb	$8, 18(%r15)
	je	.L9789
	cmpb	$32, %dl
	je	.L9789
	testb	$8, 18(%r13)
	jne	.L9789
	testb	$1, 53(%r13)
	jne	.L9789
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L9785
	cmpq	$0, 8(%rax)
	jne	.L11369
.L9785:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11250:
	xorl	%eax, %eax
	call	warning
	jmp	.L10869
.L11369:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11250
.L11368:
	movq	8(%r13), %r9
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r9)
	jne	.L9783
	jmp	.L9780
	.p2align 6,,7
.L11367:
	cmpq	$0, -2800(%rbp)
	je	.L9769
	movq	-2800(%rbp), %r12
	cmpb	$32, 16(%r12)
	jne	.L9768
	jmp	.L9769
.L11366:
	testb	$8, 54(%r13)
	jne	.L9766
	andb	$-9, 18(%r13)
	jmp	.L9766
	.p2align 6,,7
.L10868:
	movq	global_binding_level(%rip), %rax
	jmp	.L9746
.L11352:
	testl	%esi, %esi
	jg	.L11218
	testl	%ebx, %ebx
	je	.L9752
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11218
	.p2align 6,,7
.L9760:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L9752
.L11351:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9752
	testq	%rax, %rax
	je	.L10216
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9750
	.p2align 6,,7
.L11350:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L9743
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L9743
	.p2align 6,,7
.L10923:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2784(%rbp)
	je	.L9607
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L9606
.L9607:
	movq	global_binding_level(%rip), %rsi
	movq	%r13, -2784(%rbp)
	cmpq	%rsi, current_binding_level(%rip)
	jne	.L10861
	movq	%r13, 80(%rdx)
.L10861:
	movzbl	16(%r13), %eax
.L9610:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2784(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L9734
	cmpq	$0, 72(%rax)
	je	.L11370
.L9734:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L9605
	cmpq	$0, 56(%rax)
	je	.L9605
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -1656(%rbp)
	je	.L9739
	movq	-1656(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
.L11217:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9740:
	movq	-1656(%rbp), %rsi
	movq	%r12, 8(%rsi)
	jmp	.L9605
.L9739:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9740
	movq	-1656(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
	jmp	.L11217
.L11370:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -1648(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L9735
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-1648(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L9734
.L9735:
	movq	%rbx, 72(%r13)
	jmp	.L9734
.L9606:
	movq	-2784(%rbp), %rcx
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rcx), %rbx
	movq	%rbx, -1608(%rbp)
	jne	.L9610
	movq	-1608(%rbp), %r12
	movq	32(%r12), %rcx
	cmpb	$36, (%rcx)
	jne	.L9610
	cmpb	$95, 1(%rcx)
	jne	.L9610
	movq	class_binding_level(%rip), %r10
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r10, %r10
	movq	%r10, -2768(%rbp)
	movq	%rdx, -1616(%rbp)
	jne	.L9614
	testb	$-128, 66(%rsi)
	movq	%rsi, -2768(%rbp)
	je	.L9614
.L9618:
	movq	-2768(%rbp), %rax
	movq	56(%rax), %r11
	testb	$-128, 66(%r11)
	movq	%r11, -2768(%rbp)
	jne	.L9618
.L9614:
	movq	-2768(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11371
	movq	-2768(%rbp), %r8
	movq	-1608(%rbp), %rdi
	xorl	%eax, %eax
	movq	-1616(%rbp), %rsi
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-2768(%rbp), %r9
	movq	%rax, 8(%r9)
.L9620:
	cmpq	$0, -1608(%rbp)
	je	.L9621
	movq	-1616(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L9622
	movq	-1608(%rbp), %rdi
	movq	%rdi, 80(%rcx)
.L9622:
	movq	-1608(%rbp), %rbx
	movq	-1616(%rbp), %rax
	cmpq	%rax, 8(%rbx)
	je	.L9623
	cmpb	$21, 16(%rax)
	je	.L11372
.L9624:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L9626
	cmpq	$0, 32(%rax)
	je	.L9625
.L9626:
	movq	lang_name_cplusplus(%rip), %r12
	cmpq	%r12, current_lang_name(%rip)
	je	.L11373
.L9627:
	xorl	%ecx, %ecx
.L9662:
	testq	%rcx, %rcx
	jne	.L9663
.L10281:
	movq	-1608(%rbp), %rsi
	movq	-1616(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1616(%rbp), %rdi
	movq	%rax, -2776(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2776(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L9664
	movq	-1608(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L11214:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9665:
	movq	-1616(%rbp), %r11
	movq	-1608(%rbp), %rdx
	movq	%r11, 8(%rdx)
.L9668:
	movq	-1608(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$36, (%rax)
	je	.L11374
.L9670:
	movq	-1616(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L9701
	cmpb	$32, 16(%rdx)
	je	.L11375
.L9671:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9690
	movq	-2776(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10863
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9692
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9692:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2776(%rbp), %rdi
	leaq	8(%rdx), %rsi
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rsi
	ja	.L11376
.L9694:
	movq	-2776(%rbp), %rbx
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r10)
	cmpb	$32, 16(%rbx)
	je	.L11377
.L10864:
	movq	-1608(%rbp), %rbx
	movq	32(%rbx), %rax
.L9701:
	cmpb	$36, (%rax)
	je	.L11378
.L9715:
	movq	current_class_type(%rip), %rdx
	movq	-2776(%rbp), %rcx
	movq	-1616(%rbp), %r8
	testq	%rdx, %rdx
	movq	%rcx, 80(%r8)
	jne	.L9718
	cmpq	$0, current_function_decl(%rip)
	je	.L9717
.L9718:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L9716
.L9717:
	movq	-1608(%rbp), %r12
	movq	-2776(%rbp), %rdi
	movq	%r12, 72(%rdi)
.L9623:
	movq	-2768(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L11379
.L9621:
	movq	-1616(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L11380
	movq	-1616(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1616(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2784(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L9610
.L11380:
	movq	%rax, (%rdx)
	movq	-2784(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L9610
.L11379:
	movq	-1616(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r12
	movq	-1608(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%r12, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L9621
	movq	-2768(%rbp), %r9
	movq	144(%rax), %r11
	movq	8(%r9), %rdx
	movq	%rdx, 72(%r11)
	jmp	.L9621
.L9716:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11381
	cmpq	$0, 32(%rdx)
	jne	.L9623
	movq	-2776(%rbp), %r9
	movq	80(%rdx), %r8
	movl	$136, %esi
	cmpb	$32, 16(%r9)
	movq	72(%r8), %rbx
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9726
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1608(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1608(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2776(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1616(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L9728:
	movq	-2776(%rbp), %rbx
	movq	current_class_type(%rip), %r8
	movq	152(%rbx), %r9
	movq	%r8, 64(%rbx)
	movq	%r8, 16(%r9)
	jmp	.L9623
.L9726:
	movq	-1608(%rbp), %r11
	movq	-2776(%rbp), %rsi
	movq	%r11, 72(%rsi)
	jmp	.L9728
.L11381:
	movq	-2776(%rbp), %rdx
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rdx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9721
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1608(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1608(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2776(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-1616(%rbp), %r11
	movq	%r11, 8(%rax)
.L9723:
	movq	current_function_decl(%rip), %rsi
	movq	-2776(%rbp), %rdx
	movq	%rsi, 64(%rdx)
	jmp	.L9623
.L9721:
	movq	-1608(%rbp), %r12
	movq	-2776(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L9723
.L11378:
	cmpb	$95, 1(%rax)
	jne	.L9715
	movq	-2776(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L9715
.L11377:
	cmpq	$0, 72(%rbx)
	je	.L11382
.L10865:
	movq	-1608(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L9701
.L11382:
	movq	-2776(%rbp), %r11
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r11), %rdx
	movq	%rdx, -1624(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9696
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2776(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-1624(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10862:
	movq	-1608(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L9701
.L9696:
	movq	-2776(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10864
.L11376:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9694
.L10863:
	movq	-1608(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L9701
.L9690:
	movq	-2776(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2776(%rbp)
	jmp	.L10865
.L11375:
	movq	global_binding_level(%rip), %rsi
	movl	$1, %r12d
	cmpq	%rsi, current_binding_level(%rip)
	je	.L9672
	movq	-1608(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L9673
.L9672:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9674
	movq	-1608(%rbp), %r10
	movq	56(%r10), %rcx
	testq	%rcx, %rcx
	jne	.L10213
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L9675
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11383
.L9675:
	testq	%rcx, %rcx
	jne	.L10213
.L10214:
	movq	-1608(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10213
	movq	-1608(%rbp), %r11
	movq	40(%r11), %rcx
.L9673:
	testq	%rcx, %rcx
	je	.L9677
.L10213:
	cmpb	$32, 16(%rcx)
	je	.L9677
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9677
	movq	-1608(%rbp), %r9
	movq	8(%r9), %rax
	testq	%rax, %rax
	je	.L9685
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11215
	testl	%r12d, %r12d
	jle	.L11384
.L11215:
	movq	%rax, %rcx
.L9677:
	movq	-1616(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L9671
	jmp	.L10862
.L11384:
	testl	%edx, %edx
	jg	.L11215
	testl	%r12d, %r12d
	je	.L9677
	movq	-1608(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11215
.L9685:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L9677
.L11383:
	xorl	%ecx, %ecx
	movq	-1608(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9677
	testq	%rax, %rax
	je	.L10214
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L9675
.L9674:
	movq	-1608(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L9673
.L11374:
	cmpb	$95, 1(%rax)
	jne	.L9670
	jmp	.L9701
.L9664:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9665
	movq	-1608(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L11214
.L9663:
	movq	80(%rcx), %rax
	movq	%rax, -2776(%rbp)
	jmp	.L9668
.L11373:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L9629
	movq	80(%rax), %rbx
.L9629:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9662
.L9661:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L9638
	cmpl	$32, %eax
	je	.L11385
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L9632:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9662
	jmp	.L9661
.L11385:
	movq	8(%rbx), %rdx
	movq	-1616(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r10
	movq	72(%r10), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10897
	movq	64(%rbx), %rbx
	jmp	.L9632
.L10897:
	movq	32(%rax), %rcx
	jmp	.L9662
.L9638:
	movq	-1616(%rbp), %rax
	movq	80(%rax), %r11
	movq	56(%r11), %rbx
	testq	%rbx, %rbx
	je	.L9627
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L9641
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L9642
.L9641:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9643
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10211
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9644
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L11386
.L9644:
	testq	%rcx, %rcx
	jne	.L10211
.L10212:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10211
.L9643:
	movq	40(%rbx), %rcx
.L9642:
	testq	%rcx, %rcx
	je	.L10281
.L10211:
	cmpb	$32, 16(%rcx)
	je	.L9662
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9662
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L9654
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11213
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11387
.L11213:
	movq	%rax, %rcx
	jmp	.L9662
.L11387:
	testl	%edx, %edx
	jg	.L11213
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L9662
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11213
.L9654:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L9662
	jmp	.L11213
.L11386:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9662
	testq	%rax, %rax
	je	.L10212
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9644
.L9625:
	movq	-1608(%rbp), %rsi
	movq	-1616(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1632(%rbp)
	je	.L9702
	movq	-1608(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11216:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9703:
	movq	-1616(%rbp), %rdx
	movq	-1608(%rbp), %rsi
	movq	%rdx, 8(%rsi)
	movq	-1632(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L9706
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9707
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9707:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1632(%rbp), %rbx
	leaq	8(%rdx), %r11
	movq	%rbx, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r11
	ja	.L11388
.L9709:
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-1632(%rbp), %rdx
	movq	%rdx, (%r9)
	cmpb	$32, 16(%rdx)
	je	.L11389
.L9706:
	movq	-1632(%rbp), %r12
	movq	%r12, -2776(%rbp)
	jmp	.L10865
.L11389:
	cmpq	$0, 72(%rdx)
	jne	.L9706
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -1640(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9711
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1632(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1640(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L9706
.L9711:
	movq	-1632(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L9706
.L11388:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9709
.L9702:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9703
	movq	-1608(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L11216
.L11372:
	cmpq	$0, class_binding_level(%rip)
	je	.L9624
	movq	144(%rax), %rsi
	testb	$16, 3(%rsi)
	jne	.L9623
	jmp	.L9624
.L11371:
	movq	-1608(%rbp), %rdi
	movq	-1616(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L9620
	.p2align 6,,7
.L9591:
	movq	-2800(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10856
	movl	flag_traditional(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L10860
	testb	$8, 18(%r15)
	je	.L10860
	testb	$8, 18(%r13)
	jne	.L10860
	testb	$9, 53(%r13)
	jne	.L10860
	cmpq	%r13, current_function_decl(%rip)
	je	.L11390
.L9600:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L9601
	cmpq	$0, 8(%rax)
	jne	.L11391
.L9601:
	movq	-2800(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11212:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2800(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10860
.L11391:
	movq	-2800(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11212
.L11390:
	movq	-2800(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L9600
	.p2align 6,,7
.L11349:
	cmpq	$0, 64(%rcx)
	jne	.L9589
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L9589
.L11348:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2800(%rbp)
	call	error_with_decl
	jmp	.L9587
.L9580:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L9582
.L9586:
	cmpq	%r15, 56(%rax)
	je	.L9582
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L9586
.L9582:
	movq	%rax, -2800(%rbp)
	jmp	.L9579
.L11347:
	movq	40(%r15), %rax
	jmp	.L9582
.L11346:
	movq	56(%r13), %r15
	jmp	.L9576
.L11345:
	testb	$32, 53(%r13)
	jne	.L9574
	jmp	.L9575
.L10852:
	movzbl	16(%r13), %edx
	jmp	.L9574
.L11337:
	movq	default_function_type(%rip), %rsi
	xorl	%edx, %edx
	xorl	%r8d, %r8d
	movl	$.LC84, %edi
	movl	$pushdecl, %ecx
	xorl	%eax, %eax
	xorl	%r12d, %r12d
	call	define_function
	xorl	%eax, %eax
	movl	$.LC84, %edi
	call	get_identifier
	movq	%rax, %rbx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L9277
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %r13
	jne	.L9278
.L9277:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9279
	movq	56(%rbx), %r13
	testq	%r13, %r13
	jne	.L10203
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L9280
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11392
.L9280:
	testq	%r13, %r13
	jne	.L10203
.L10204:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %r13
	jne	.L10203
.L9279:
	movq	40(%rbx), %r13
.L9278:
	testq	%r13, %r13
	je	.L10837
.L10203:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L9282
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L9282
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L9290
	movq	80(%rax), %rax
	cmpq	%rax, %r13
	je	.L11248
	testl	%r12d, %r12d
	jle	.L11393
.L11248:
	movq	%rax, %r13
.L10836:
	movzbl	16(%r13), %edx
.L9282:
	movq	current_function_decl(%rip), %rax
	movq	current_binding_level(%rip), %r11
	movq	112(%r13), %r14
	cmpq	%rax, %r13
	movq	%r11, -2752(%rbp)
	je	.L9295
	cmpb	$29, %dl
	je	.L11394
.L9296:
	movq	%rax, 64(%r13)
.L9295:
	cmpb	$32, %dl
	je	.L11395
.L9297:
	testq	%r14, %r14
	je	.L9298
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11396
	cmpq	$0, 48(%r14)
	jne	.L9301
	movq	$0, -2720(%rbp)
.L9300:
	cmpq	$0, -2720(%rbp)
	je	.L9318
	movq	-2720(%rbp), %r12
	cmpq	error_mark_node(%rip), %r12
	je	.L11397
.L9308:
	cmpq	$0, -2720(%rbp)
	je	.L10841
	movq	-2720(%rbp), %rsi
	cmpb	$34, 16(%rsi)
	je	.L11398
.L9310:
	movq	-2720(%rbp), %rax
	testq	%rax, %rax
	movq	24(%rax), %r12
	movl	32(%rax), %ebx
	je	.L10841
	movzbl	16(%rax), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L9312
	cmpb	$32, %al
	je	.L9318
	cmpb	$32, %dl
	je	.L10922
	movq	-2720(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	jne	.L9276
.L10841:
	movzbl	16(%r13), %edx
.L9318:
	cmpb	$32, %dl
	je	.L10922
.L9326:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L9464
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L9464
	testb	$1, 53(%rax)
	jne	.L9465
	testb	$8, 18(%rax)
	je	.L9464
.L9465:
	andb	$8, %dl
	je	.L11399
.L9464:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10849
	testb	$1, 53(%r13)
	je	.L10849
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rsi
	cmpq	%rsi, current_binding_level(%rip)
	je	.L9468
	movq	48(%r14), %r15
	testq	%r15, %r15
	movq	%r15, %rcx
	jne	.L9469
.L9468:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9470
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10209
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9471
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11400
.L9471:
	testq	%rcx, %rcx
	jne	.L10209
.L10210:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10209
.L9470:
	movq	40(%r14), %rcx
.L9469:
	testq	%rcx, %rcx
	je	.L10280
.L10209:
	cmpb	$32, 16(%rcx)
	je	.L9473
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L9473
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L9481
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11209
	testl	%ebx, %ebx
	jle	.L11401
.L11209:
	movq	%rax, %rcx
.L9473:
	testq	%rcx, %rcx
	jne	.L10849
.L10280:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2752(%rbp)
.L9467:
	cmpq	%rax, -2752(%rbp)
	je	.L11402
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11403
.L11210:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L9512:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11404
.L9523:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L9525
	testq	%r12, %r12
	je	.L9526
	testb	$1, 53(%r13)
	jne	.L9526
	cmpb	$34, 16(%r12)
	je	.L11405
.L9526:
	movl	warn_shadow(%rip), %edx
	testl	%edx, %edx
	je	.L9525
	testb	$1, 53(%r13)
	jne	.L9525
	movl	32(%r13), %r11d
	testl	%r11d, %r11d
	je	.L9525
	testq	%rax, %rax
	jne	.L9525
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L9531
	cmpb	$34, 16(%r12)
	je	.L11406
.L9531:
	cmpq	$0, 56(%r14)
	je	.L9533
	movl	$.LC41, %edi
.L9532:
	testq	%rdi, %rdi
	jne	.L11211
.L9525:
	testq	%r12, %r12
	je	.L10850
	movq	-2752(%rbp), %rbx
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%rbx), %rdx
	call	tree_cons
	movq	-2752(%rbp), %r12
	movq	%rax, 16(%r12)
.L10850:
	movzbl	16(%r13), %edx
.L9510:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L9298
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L9298
	cmpb	$18, 16(%rcx)
	je	.L11407
.L9542:
	testb	$64, 46(%rcx)
	je	.L9298
.L9541:
	movq	-2752(%rbp), %rsi
	movzwl	64(%rsi), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rsi)
	je	.L11408
.L10851:
	movzbl	16(%r13), %edx
.L9298:
	cmpb	$32, %dl
	je	.L11409
.L9544:
	movq	-2752(%rbp), %r11
	cmpq	global_binding_level(%rip), %r11
	movq	(%r11), %r9
	movq	%r9, (%r13)
	movq	%r13, (%r11)
	jne	.L9276
	testb	$4, 17(%r13)
	jne	.L9276
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9276
.L11409:
	testq	%r14, %r14
	je	.L9544
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9545
	cmpq	class_binding_level(%rip), %rax
	je	.L9546
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L9550
	cmpb	$32, 16(%rax)
	je	.L9548
.L9550:
	cmpq	$0, current_class_type(%rip)
	je	.L9545
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L9545
	cmpb	$32, 16(%rax)
	je	.L9548
.L9545:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L9549
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9548
	cmpb	$-127, %dl
	je	.L11410
.L9549:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L9544
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11411
.L9556:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9560
	cmpq	class_binding_level(%rip), %rax
	je	.L9561
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L9565
	cmpb	$32, 16(%rax)
	je	.L9563
.L9565:
	cmpq	$0, current_class_type(%rip)
	je	.L9560
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L9560
	cmpb	$32, 16(%rax)
	je	.L9563
.L9560:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L9544
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9563
	cmpb	$-127, %dl
	jne	.L9544
	movq	$0, 8(%rbx)
	jmp	.L9544
.L9563:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L9544
.L9561:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9565
.L11411:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%dl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L9556
.L11410:
	movq	$0, 8(%r14)
	jmp	.L9549
.L9548:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r14)
	jmp	.L9549
.L9546:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9550
.L11408:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10851
.L11407:
	movq	8(%rcx), %r8
	testb	$64, 46(%r8)
	jne	.L9541
	jmp	.L9542
.L11211:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L9525
.L9533:
	testq	%r12, %r12
	je	.L9535
	movl	$.LC42, %edi
	jmp	.L9532
.L9535:
	testq	%r15, %r15
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L9532
.L11406:
	movl	$.LC40, %edi
	jmp	.L9532
.L11405:
	cmpb	$34, 16(%r13)
	je	.L9526
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L9527
	movq	56(%rax), %rax
.L9527:
	movzbl	66(%rax), %r10d
	andl	$15, %r10d
	decl	%r10d
	jne	.L9525
	movl	$.LC40, %edi
	jmp	.L11211
	.p2align 6,,7
.L11404:
	movzbl	53(%r13), %r8d
	andb	$9, %r8b
	decb	%r8b
	je	.L11412
.L9515:
	testq	%r12, %r12
	jne	.L9523
	testq	%r15, %r15
	jne	.L9523
	testb	$1, 53(%r13)
	je	.L9523
	testb	$8, 18(%r13)
	je	.L9523
	orb	$8, 18(%r14)
	jmp	.L9523
	.p2align 6,,7
.L11412:
	testq	%r15, %r15
	je	.L9515
	cmpb	$29, 16(%r13)
	jne	.L9515
	cmpb	$29, 16(%r15)
	jne	.L9515
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11413
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L9518
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%sil, %cl
	movb	%cl, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L9519
	movq	88(%r15), %rax
.L9520:
	movq	136(%r15), %r8
	movq	72(%r15), %rbx
	movq	%rax, (%rdx)
	movq	80(%r15), %rdi
	movzbl	17(%r13), %r10d
	movq	%r8, 136(%r13)
	movq	%rbx, 72(%r13)
	movq	%rdi, 80(%r13)
	movzbl	17(%r15), %r9d
	movq	%r15, 96(%r13)
	andb	$127, %r10b
	shrb	$7, %r9b
	movzbl	%r9b, %edx
	movl	%edx, %r11d
	salb	$7, %r11b
	orb	%r11b, %r10b
	movb	%r10b, 17(%r13)
	movzbl	53(%r15), %ecx
.L9518:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L9521
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L9521:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L9515
	cmpq	$0, 88(%r15)
	je	.L9515
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L9515
	movq	%rdx, 8(%r13)
	jmp	.L9515
.L9519:
	xorl	%eax, %eax
	jmp	.L9520
.L11413:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L9515
	.p2align 6,,7
.L11403:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9512
	jmp	.L11210
.L11402:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11414
.L9487:
	cmpq	$0, 40(%r14)
	jne	.L9488
	testb	$8, 18(%r13)
	je	.L9488
	orb	$8, 18(%r14)
.L9488:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11415
.L9490:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L9489:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9501
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9491
	testb	$1, 18(%rcx)
	je	.L9491
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L9491:
	testq	%rax, %rax
	je	.L9501
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9496
	testb	$8, 17(%rcx)
	je	.L9496
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L9496:
	testq	%rax, %rax
	je	.L9501
	cmpq	$0, 8(%rax)
	je	.L9501
	cmpb	$29, %dl
	je	.L11416
.L9504:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L9501:
	testb	$8, 18(%r14)
	je	.L9510
	cmpb	$32, %dl
	je	.L9510
	testb	$8, 18(%r13)
	jne	.L9510
	testb	$1, 53(%r13)
	jne	.L9510
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9506
	cmpq	$0, 8(%rax)
	jne	.L11417
.L9506:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11249:
	xorl	%eax, %eax
	call	warning
	jmp	.L10850
.L11417:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11249
.L11416:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%rbx)
	jne	.L9504
	jmp	.L9501
	.p2align 6,,7
.L11415:
	cmpq	$0, -2720(%rbp)
	je	.L9490
	movq	-2720(%rbp), %r9
	cmpb	$32, 16(%r9)
	jne	.L9489
	jmp	.L9490
.L11414:
	testb	$8, 54(%r13)
	jne	.L9487
	andb	$-9, 18(%r13)
	jmp	.L9487
	.p2align 6,,7
.L10849:
	movq	global_binding_level(%rip), %rax
	jmp	.L9467
.L11401:
	testl	%esi, %esi
	jg	.L11209
	testl	%ebx, %ebx
	je	.L9473
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11209
	.p2align 6,,7
.L9481:
	movq	8(%rcx), %r11
	cmpq	error_mark_node(%rip), %r11
	cmove	%r11, %rcx
	jmp	.L9473
.L11400:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9473
	testq	%rax, %rax
	je	.L10210
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9471
	.p2align 6,,7
.L11399:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L9464
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L9464
	.p2align 6,,7
.L10922:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, -2744(%rbp)
	je	.L9328
	movzbl	16(%rbx), %eax
	cmpb	$32, %al
	je	.L9327
.L9328:
	movq	global_binding_level(%rip), %r15
	movq	%r13, -2744(%rbp)
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10842
	movq	%r13, 80(%rdx)
.L10842:
	movzbl	16(%r13), %eax
.L9331:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-2744(%rbp), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L9455
	cmpq	$0, 72(%rsi)
	je	.L11418
.L9455:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L9326
	cmpq	$0, 56(%rax)
	je	.L9326
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11419
.L11208:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9461:
	movq	%r12, 8(%r15)
	jmp	.L9326
.L11419:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9461
	jmp	.L11208
.L11418:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r8b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L9456
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L9455
.L9456:
	movq	%rbx, 72(%r13)
	jmp	.L9455
.L9327:
	movq	current_binding_level(%rip), %rsi
	movq	-2744(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rbx), %r15
	jne	.L9331
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L9331
	cmpb	$95, 1(%rcx)
	jne	.L9331
	movq	class_binding_level(%rip), %rcx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%rcx, %rcx
	movq	%rcx, -2728(%rbp)
	movq	%rax, -1576(%rbp)
	jne	.L9335
	testb	$-128, 66(%rsi)
	movq	%rsi, -2728(%rbp)
	je	.L9335
.L9339:
	movq	-2728(%rbp), %r8
	movq	56(%r8), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2728(%rbp)
	jne	.L9339
.L9335:
	movq	-2728(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11420
	movq	-2728(%rbp), %r9
	movq	-1576(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r9), %rdx
	call	saveable_tree_cons
	movq	-2728(%rbp), %r11
	movq	%rax, 8(%r11)
.L9341:
	testq	%r15, %r15
	je	.L9342
	movq	-1576(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L9343
	movq	%r15, 80(%rcx)
.L9343:
	movq	-1576(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L9344
	cmpb	$21, 16(%rbx)
	je	.L11421
.L9345:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L9347
	cmpq	$0, 32(%rax)
	je	.L9346
.L9347:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L11422
.L9348:
	xorl	%ecx, %ecx
.L9383:
	testq	%rcx, %rcx
	jne	.L9384
.L10279:
	movq	-1576(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1576(%rbp), %rdi
	movq	%rax, -2736(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	current_binding_level(%rip), %rbx
	movq	-2736(%rbp), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, 112(%rdi)
	movl	$0, 32(%rdi)
	je	.L11423
.L11205:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9386:
	movq	-1576(%rbp), %rdx
	movq	%rdx, 8(%r15)
.L9389:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11424
.L9391:
	movq	-1576(%rbp), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.L9422
	cmpb	$32, 16(%rdx)
	je	.L11425
.L9392:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9411
	movq	-2736(%rbp), %r9
	movq	56(%r9), %r12
	testq	%r12, %r12
	je	.L10846
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9413
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9413:
	movq	-2736(%rbp), %rdx
	movq	%rdx, 56(%r12)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rdi
	cmpq	decl_obstack+32(%rip), %rdi
	ja	.L11426
.L9415:
	movq	-2736(%rbp), %rsi
	movq	%rdx, %rcx
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%rcx)
	cmpb	$32, 16(%rsi)
	je	.L11427
.L10846:
	movq	32(%r15), %rax
.L9422:
	cmpb	$36, (%rax)
	je	.L11428
.L9436:
	movq	current_class_type(%rip), %rdx
	movq	-2736(%rbp), %r8
	movq	-1576(%rbp), %rdi
	testq	%rdx, %rdx
	movq	%r8, 80(%rdi)
	jne	.L9439
	cmpq	$0, current_function_decl(%rip)
	je	.L9438
.L9439:
	movq	lang_name_cplusplus(%rip), %rcx
	cmpq	%rcx, current_lang_name(%rip)
	je	.L9437
.L9438:
	movq	-2736(%rbp), %rdx
	movq	%r15, 72(%rdx)
.L9344:
	movq	-2728(%rbp), %r12
	movzbl	66(%r12), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L11429
.L9342:
	movq	-1576(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11430
	movq	-1576(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1576(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-2744(%rbp), %rdi
	movzbl	16(%rdi), %eax
	jmp	.L9331
.L11430:
	movq	%rax, (%rsi)
	movq	-2744(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L9331
.L11429:
	movq	-1576(%rbp), %rsi
	orb	$64, 18(%rsi)
	movq	80(%rsi), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L9342
	movq	-2728(%rbp), %r11
	movq	144(%rax), %r15
	movq	8(%r11), %r10
	movq	%r10, 72(%r15)
	jmp	.L9342
.L9437:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11431
	cmpq	$0, 32(%rdx)
	jne	.L9344
	movq	80(%rdx), %r12
	movl	$136, %esi
	movq	-2736(%rbp), %rdx
	cmpb	$32, 16(%rdx)
	movq	72(%r12), %rbx
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9447
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2736(%rbp), %r10
	movq	%rax, 72(%r10)
	movq	-1576(%rbp), %rsi
	movq	%rsi, 8(%rax)
.L9449:
	movq	-2736(%rbp), %r8
	movq	current_class_type(%rip), %rbx
	movq	152(%r8), %rdi
	movq	%rbx, 64(%r8)
	movq	%rbx, 16(%rdi)
	jmp	.L9344
.L9447:
	movq	-2736(%rbp), %r9
	movq	%r15, 72(%r9)
	jmp	.L9449
.L11431:
	movq	-2736(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9442
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2736(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-1576(%rbp), %r10
	movq	%r10, 8(%rax)
.L9444:
	movq	current_function_decl(%rip), %r8
	movq	-2736(%rbp), %rbx
	movq	%r8, 64(%rbx)
	jmp	.L9344
.L9442:
	movq	-2736(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L9444
.L11428:
	cmpb	$95, 1(%rax)
	jne	.L9436
	movq	-2736(%rbp), %r9
	orb	$64, 53(%r9)
	jmp	.L9436
.L11427:
	cmpq	$0, 72(%rsi)
	jne	.L10846
	movq	8(%rsi), %r8
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r8, -1584(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9417
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2736(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-1584(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L10846
.L9417:
	movq	-2736(%rbp), %rax
	movq	%r12, 72(%rax)
	jmp	.L10846
.L11426:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9415
.L9411:
	movq	-2736(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2736(%rbp)
	jmp	.L10846
.L11425:
	movq	global_binding_level(%rip), %rbx
	movl	$1, %r12d
	cmpq	%rbx, current_binding_level(%rip)
	je	.L9393
	movq	48(%r15), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L9394
.L9393:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9395
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10207
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L9396
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11432
.L9396:
	testq	%rcx, %rcx
	jne	.L10207
.L10208:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10207
.L9395:
	movq	40(%r15), %rcx
.L9394:
	testq	%rcx, %rcx
	je	.L9398
.L10207:
	cmpb	$32, 16(%rcx)
	je	.L9398
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9398
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L9406
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11206
	testl	%r12d, %r12d
	jle	.L11433
.L11206:
	movq	%rax, %rcx
.L9398:
	movq	-1576(%rbp), %rax
	cmpq	80(%rax), %rcx
	jne	.L9392
	jmp	.L10846
.L11433:
	testl	%edx, %edx
	jg	.L11206
	testl	%r12d, %r12d
	je	.L9398
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11206
.L9406:
	movq	8(%rcx), %r11
	cmpq	error_mark_node(%rip), %r11
	cmove	%r11, %rcx
	jmp	.L9398
.L11432:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9398
	testq	%rax, %rax
	je	.L10208
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L9396
.L11424:
	cmpb	$95, 1(%rax)
	jne	.L9391
	jmp	.L9422
.L11423:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9386
	jmp	.L11205
.L9384:
	movq	80(%rcx), %rsi
	movq	%rsi, -2736(%rbp)
	jmp	.L9389
.L11422:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L9350
	movq	80(%rax), %rbx
.L9350:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9383
.L9382:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L9359
	cmpl	$32, %eax
	je	.L11434
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L9353:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9383
	jmp	.L9382
.L11434:
	movq	8(%rbx), %rcx
	movq	-1576(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rcx), %rdx
	movq	72(%rdx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10896
	movq	64(%rbx), %rbx
	jmp	.L9353
.L10896:
	movq	32(%rax), %rcx
	jmp	.L9383
.L9359:
	movq	-1576(%rbp), %rbx
	movq	80(%rbx), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L9348
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L9362
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L9363
.L9362:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9364
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10205
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9365
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11435
.L9365:
	testq	%rcx, %rcx
	jne	.L10205
.L10206:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10205
.L9364:
	movq	40(%rbx), %rcx
.L9363:
	testq	%rcx, %rcx
	je	.L10279
.L10205:
	cmpb	$32, 16(%rcx)
	je	.L9383
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9383
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L9375
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11204
	movl	$1, %r9d
	testl	%r9d, %r9d
	jle	.L11436
.L11204:
	movq	%rax, %rcx
	jmp	.L9383
.L11436:
	testl	%edx, %edx
	jg	.L11204
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L9383
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11204
.L9375:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L9383
	jmp	.L11204
.L11435:
	movl	$1, %r11d
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %r11d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9383
	testq	%rax, %rax
	je	.L10206
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9365
.L9346:
	movq	-1576(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1592(%rbp)
	je	.L11437
.L11207:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9424:
	movq	-1576(%rbp), %r9
	movq	%r9, 8(%r15)
	movq	-1592(%rbp), %rbx
	movq	56(%rbx), %r12
	testq	%r12, %r12
	je	.L9427
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9428
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9428:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1592(%rbp), %rcx
	leaq	8(%rdx), %rdi
	movq	%rcx, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rdi
	ja	.L11438
.L9430:
	movq	-1592(%rbp), %rcx
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rcx, (%r8)
	cmpb	$32, 16(%rcx)
	je	.L11439
.L9427:
	movq	-1592(%rbp), %rbx
	movq	%rbx, -2736(%rbp)
	jmp	.L10846
.L11439:
	cmpq	$0, 72(%rcx)
	jne	.L9427
	movq	current_class_name(%rip), %rbx
	movq	8(%rcx), %rdx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%rdx, -1600(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9432
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1592(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-1600(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L9427
.L9432:
	movq	-1592(%rbp), %rax
	movq	%r12, 72(%rax)
	jmp	.L9427
.L11438:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9430
.L11437:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9424
	jmp	.L11207
.L11421:
	cmpq	$0, class_binding_level(%rip)
	je	.L9345
	movq	144(%rbx), %r12
	testb	$16, 3(%r12)
	jne	.L9344
	jmp	.L9345
.L11420:
	movq	-1576(%rbp), %rsi
	movq	8(%rcx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2728(%rbp), %r10
	movq	%rax, 8(%r10)
	jmp	.L9341
	.p2align 6,,7
.L9312:
	movq	-2720(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10841
	movl	flag_traditional(%rip), %edx
	testl	%edx, %edx
	jne	.L9276
	testb	$8, 18(%r14)
	je	.L9276
	testb	$8, 18(%r13)
	jne	.L9276
	testb	$9, 53(%r13)
	jne	.L9276
	cmpq	%r13, current_function_decl(%rip)
	je	.L11440
.L9321:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9322
	cmpq	$0, 8(%rax)
	jne	.L11441
.L9322:
	movq	-2720(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11203:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2720(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L9276
.L11441:
	movq	-2720(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11203
.L11440:
	movq	-2720(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L9321
	.p2align 6,,7
.L11398:
	cmpq	$0, 64(%rsi)
	jne	.L9310
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L9310
.L11397:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2720(%rbp)
	call	error_with_decl
	jmp	.L9308
.L9301:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L9303
.L9307:
	cmpq	%r14, 56(%rdi)
	je	.L9303
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L9307
.L9303:
	movq	%rdi, -2720(%rbp)
	jmp	.L9300
.L11396:
	movq	40(%r14), %r9
	movq	%r9, -2720(%rbp)
	jmp	.L9300
.L11395:
	movq	56(%r13), %r14
	jmp	.L9297
.L11394:
	testb	$32, 53(%r13)
	jne	.L9295
	jmp	.L9296
.L11393:
	testl	%ecx, %ecx
	jg	.L11248
	testl	%r12d, %r12d
	je	.L9282
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11248
	.p2align 6,,7
.L9290:
	movq	8(%r13), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L9282
	jmp	.L11248
.L10837:
	movzbl	16, %edx
	jmp	.L9282
.L11392:
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %r13
	je	.L10836
	testq	%rax, %rax
	je	.L10204
	cmpb	$32, 16(%rax)
	cmovne	%r14, %r13
	jmp	.L9280
	.p2align 6,,7
.L11336:
	leal	(%rcx,%rcx), %ebx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%ebx,%rsi
	movl	%ebx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L9272
.L11335:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9263
	jmp	.L11201
.L9261:
	movq	-1512(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11442
.L11202:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9268:
	movq	$0, 8(%r15)
	jmp	.L9260
.L11442:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9268
	jmp	.L11202
	.p2align 6,,7
.L11334:
	movq	-2672(%rbp), %rsi
	movq	-1512(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_binding_level(%rip), %rax
	movq	%rax, -2704(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10817
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11443
.L8980:
	movq	%rax, 64(%r13)
.L8979:
	cmpb	$32, %dl
	je	.L11444
.L8981:
	testq	%r14, %r14
	je	.L8982
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11445
	cmpq	$0, 48(%r14)
	jne	.L8985
	movq	$0, -2712(%rbp)
.L8984:
	cmpq	$0, -2712(%rbp)
	je	.L9002
	movq	-2712(%rbp), %r9
	cmpq	error_mark_node(%rip), %r9
	je	.L11446
.L8992:
	cmpq	$0, -2712(%rbp)
	je	.L10821
	movq	-2712(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11447
.L8994:
	movq	-2712(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movq	%rcx, %rsi
	movl	32(%rcx), %ebx
	je	.L10821
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L8996
	cmpb	$32, %al
	je	.L9002
	cmpb	$32, %dl
	je	.L10921
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10821
.L10825:
	movq	global_binding_level(%rip), %rax
.L9001:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L9255
	movq	-2672(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11200:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9256:
	movq	-2672(%rbp), %rdx
	movl	$24, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L8978
	movq	-2712(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L8978
.L9255:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9256
	movq	-2672(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
	jmp	.L11200
.L10821:
	movzbl	16(%r13), %edx
.L9002:
	cmpb	$32, %dl
	je	.L10921
.L9010:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L9148
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L9148
	testb	$1, 53(%rax)
	jne	.L9149
	testb	$8, 18(%rax)
	je	.L9148
.L9149:
	andb	$8, %dl
	je	.L11448
.L9148:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10833
	testb	$1, 53(%r13)
	je	.L10833
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L9152
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L9153
.L9152:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9154
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10201
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9155
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11449
.L9155:
	testq	%rcx, %rcx
	jne	.L10201
.L10202:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10201
.L9154:
	movq	40(%r14), %rcx
.L9153:
	testq	%rcx, %rcx
	je	.L10278
.L10201:
	cmpb	$32, 16(%rcx)
	je	.L9157
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L9157
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L9165
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11197
	testl	%ebx, %ebx
	jle	.L11450
.L11197:
	movq	%rax, %rcx
.L9157:
	testq	%rcx, %rcx
	jne	.L10833
.L10278:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2704(%rbp)
.L9151:
	cmpq	%rax, -2704(%rbp)
	je	.L11451
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L11452
.L11198:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L9196:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11453
.L9207:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L9209
	testq	%r12, %r12
	je	.L9210
	testb	$1, 53(%r13)
	jne	.L9210
	cmpb	$34, 16(%r12)
	je	.L11454
.L9210:
	movl	warn_shadow(%rip), %r11d
	testl	%r11d, %r11d
	je	.L9209
	testb	$1, 53(%r13)
	jne	.L9209
	movl	32(%r13), %r9d
	testl	%r9d, %r9d
	je	.L9209
	testq	%rax, %rax
	jne	.L9209
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L9215
	cmpb	$34, 16(%r12)
	je	.L11455
.L9215:
	cmpq	$0, 56(%r14)
	je	.L9217
	movl	$.LC41, %edi
.L9216:
	testq	%rdi, %rdi
	jne	.L11199
.L9209:
	testq	%r12, %r12
	je	.L10834
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-2704(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10834:
	movzbl	16(%r13), %edx
.L9194:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L8982
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L8982
	cmpb	$18, 16(%rcx)
	je	.L11456
.L9226:
	testb	$64, 46(%rcx)
	je	.L8982
.L9225:
	movq	-2704(%rbp), %rcx
	movzwl	64(%rcx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rcx)
	je	.L11457
.L10835:
	movzbl	16(%r13), %edx
.L8982:
	cmpb	$32, %dl
	je	.L11458
.L9228:
	movq	-2704(%rbp), %r11
	movq	global_binding_level(%rip), %rax
	movq	(%r11), %r9
	cmpq	%rax, %r11
	movq	%r9, (%r13)
	movq	%r13, (%r11)
	je	.L11459
.L9254:
	movq	%r13, -2712(%rbp)
	jmp	.L9001
.L11459:
	testb	$4, 17(%r13)
	jne	.L9254
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L9254
.L11458:
	testq	%r14, %r14
	je	.L9228
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9229
	cmpq	class_binding_level(%rip), %rax
	je	.L9230
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L9234
	cmpb	$32, 16(%rax)
	je	.L9232
.L9234:
	cmpq	$0, current_class_type(%rip)
	je	.L9229
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L9229
	cmpb	$32, 16(%rax)
	je	.L9232
.L9229:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L9233
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9232
	cmpb	$-127, %dl
	je	.L11460
.L9233:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L9228
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11461
.L9240:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L9244
	cmpq	class_binding_level(%rip), %rax
	je	.L9245
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L9249
	cmpb	$32, 16(%rax)
	je	.L9247
.L9249:
	cmpq	$0, current_class_type(%rip)
	je	.L9244
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L9244
	cmpb	$32, 16(%rax)
	je	.L9247
.L9244:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L9228
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L9247
	cmpb	$-127, %dl
	jne	.L9228
	movq	$0, 8(%rbx)
	jmp	.L9228
.L9247:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L9228
.L9245:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9249
.L11461:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r10b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L9240
.L11460:
	movq	$0, 8(%r14)
	jmp	.L9233
.L9232:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L9233
.L9230:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L9234
.L11457:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10835
.L11456:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L9225
	jmp	.L9226
.L11199:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L9209
.L9217:
	testq	%r12, %r12
	je	.L9219
	movl	$.LC42, %edi
	jmp	.L9216
.L9219:
	testq	%r8, %r8
	movl	$.LC43, %edx
	cmovne	%rdx, %rdi
	jmp	.L9216
.L11455:
	movl	$.LC40, %edi
	jmp	.L9216
.L11454:
	cmpb	$34, 16(%r13)
	je	.L9210
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L9211
	movq	56(%rax), %rax
.L9211:
	movzbl	66(%rax), %r10d
	andl	$15, %r10d
	decl	%r10d
	jne	.L9209
	movl	$.LC40, %edi
	jmp	.L11199
	.p2align 6,,7
.L11453:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11462
.L9199:
	testq	%r12, %r12
	jne	.L9207
	testq	%r8, %r8
	jne	.L9207
	testb	$1, 53(%r13)
	je	.L9207
	testb	$8, 18(%r13)
	je	.L9207
	orb	$8, 18(%r14)
	jmp	.L9207
	.p2align 6,,7
.L11462:
	testq	%r8, %r8
	je	.L9199
	cmpb	$29, 16(%r13)
	jne	.L9199
	cmpb	$29, 16(%r8)
	jne	.L9199
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L11463
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L9202
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %ebx
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%bl, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L9203
	movq	88(%r8), %rax
.L9204:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %rdi
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%rcx, 136(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %r9d
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %r9b
	movzbl	%r9b, %r11d
	movl	%r11d, %r10d
	salb	$7, %r10b
	orb	%r10b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L9202:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L9205
	movzbl	53(%r13), %ebx
	salb	$4, %al
	andb	$-17, %bl
	orb	%al, %bl
	movb	%bl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L9205:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L9199
	cmpq	$0, 88(%r8)
	je	.L9199
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L9199
	movq	%rdx, 8(%r13)
	jmp	.L9199
.L9203:
	xorl	%eax, %eax
	jmp	.L9204
.L11463:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L9199
	.p2align 6,,7
.L11452:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9196
	jmp	.L11198
.L11451:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11464
.L9171:
	cmpq	$0, 40(%r14)
	jne	.L9172
	testb	$8, 18(%r13)
	je	.L9172
	orb	$8, 18(%r14)
.L9172:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11465
.L9174:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L9173:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9185
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9175
	testb	$1, 18(%rcx)
	je	.L9175
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L9175:
	testq	%rax, %rax
	je	.L9185
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L9180
	testb	$8, 17(%rcx)
	je	.L9180
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L9180:
	testq	%rax, %rax
	je	.L9185
	cmpq	$0, 8(%rax)
	je	.L9185
	cmpb	$29, %dl
	je	.L11466
.L9188:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L9185:
	testb	$8, 18(%r14)
	je	.L9194
	cmpb	$32, %dl
	je	.L9194
	testb	$8, 18(%r13)
	jne	.L9194
	testb	$1, 53(%r13)
	jne	.L9194
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9190
	cmpq	$0, 8(%rax)
	jne	.L11467
.L9190:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11247:
	xorl	%eax, %eax
	call	warning
	jmp	.L10834
.L11467:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11247
.L11466:
	movq	8(%r13), %r8
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r8)
	jne	.L9188
	jmp	.L9185
	.p2align 6,,7
.L11465:
	cmpq	$0, -2712(%rbp)
	je	.L9174
	movq	-2712(%rbp), %r9
	cmpb	$32, 16(%r9)
	jne	.L9173
	jmp	.L9174
.L11464:
	testb	$8, 54(%r13)
	jne	.L9171
	andb	$-9, 18(%r13)
	jmp	.L9171
	.p2align 6,,7
.L10833:
	movq	global_binding_level(%rip), %rax
	jmp	.L9151
.L11450:
	testl	%esi, %esi
	jg	.L11197
	testl	%ebx, %ebx
	je	.L9157
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11197
	.p2align 6,,7
.L9165:
	movq	8(%rcx), %r11
	cmpq	error_mark_node(%rip), %r11
	cmove	%r11, %rcx
	jmp	.L9157
.L11449:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9157
	testq	%rax, %rax
	je	.L10202
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9155
	.p2align 6,,7
.L11448:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L9148
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L9148
	.p2align 6,,7
.L10921:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2696(%rbp)
	je	.L9012
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L9011
.L9012:
	movq	global_binding_level(%rip), %rdi
	movq	%r13, -2696(%rbp)
	cmpq	%rdi, current_binding_level(%rip)
	jne	.L10826
	movq	%r13, 80(%rdx)
.L10826:
	movzbl	16(%r13), %eax
.L9015:
	cmpb	$32, %al
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$140, %esi
	call	my_friendly_assert
	movq	-2696(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L9139
	cmpq	$0, 72(%rax)
	je	.L11468
.L9139:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L9010
	cmpq	$0, 56(%rax)
	je	.L9010
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -1568(%rbp)
	je	.L9144
	movq	-1568(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L11196:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9145:
	movq	-1568(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L9010
.L9144:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9145
	movq	-1568(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11196
.L11468:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -1560(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L9140
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-1560(%rbp), %r11
	movq	%r11, 8(%rax)
	jmp	.L9139
.L9140:
	movq	%rbx, 72(%r13)
	jmp	.L9139
.L9011:
	movq	-2696(%rbp), %r10
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r10), %rbx
	movq	%rbx, -1520(%rbp)
	jne	.L9015
	movq	-1520(%rbp), %r12
	movq	32(%r12), %rcx
	cmpb	$36, (%rcx)
	jne	.L9015
	cmpb	$95, 1(%rcx)
	jne	.L9015
	movq	class_binding_level(%rip), %r11
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r11, %r11
	movq	%r11, -2680(%rbp)
	movq	%rdx, -1528(%rbp)
	jne	.L9019
	testb	$-128, 66(%rsi)
	movq	%rsi, -2680(%rbp)
	je	.L9019
.L9023:
	movq	-2680(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2680(%rbp)
	jne	.L9023
.L9019:
	movq	-2680(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11469
	movq	-2680(%rbp), %rcx
	movq	-1520(%rbp), %rdi
	xorl	%eax, %eax
	movq	-1528(%rbp), %rsi
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-2680(%rbp), %r9
	movq	%rax, 8(%r9)
.L9025:
	cmpq	$0, -1520(%rbp)
	je	.L9026
	movq	-1528(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L9027
	movq	-1520(%rbp), %r8
	movq	%r8, 80(%rcx)
.L9027:
	movq	-1520(%rbp), %rdi
	movq	-1528(%rbp), %rax
	cmpq	%rax, 8(%rdi)
	je	.L9028
	cmpb	$21, 16(%rax)
	je	.L11470
.L9029:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L9031
	cmpq	$0, 32(%rax)
	je	.L9030
.L9031:
	movq	lang_name_cplusplus(%rip), %r12
	cmpq	%r12, current_lang_name(%rip)
	je	.L11471
.L9032:
	xorl	%ecx, %ecx
.L9067:
	testq	%rcx, %rcx
	jne	.L9068
.L10277:
	movq	-1520(%rbp), %rsi
	movq	-1528(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1528(%rbp), %rdi
	movq	%rax, -2688(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2688(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L9069
	movq	-1520(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L11193:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9070:
	movq	-1528(%rbp), %rsi
	movq	-1520(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L9073:
	movq	-1520(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$36, (%rax)
	je	.L11472
.L9075:
	movq	-1528(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L9106
	cmpb	$32, 16(%rdx)
	je	.L11473
.L9076:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9095
	movq	-2688(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10828
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9097
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9097:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2688(%rbp), %rdi
	leaq	8(%rdx), %r10
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r10
	ja	.L11474
.L9099:
	movq	-2688(%rbp), %rbx
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r11)
	cmpb	$32, 16(%rbx)
	je	.L11475
.L10829:
	movq	-1520(%rbp), %rsi
	movq	32(%rsi), %rax
.L9106:
	cmpb	$36, (%rax)
	je	.L11476
.L9120:
	movq	current_class_type(%rip), %rdx
	movq	-2688(%rbp), %rcx
	movq	-1528(%rbp), %r8
	testq	%rdx, %rdx
	movq	%rcx, 80(%r8)
	jne	.L9123
	cmpq	$0, current_function_decl(%rip)
	je	.L9122
.L9123:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L9121
.L9122:
	movq	-1520(%rbp), %rdi
	movq	-2688(%rbp), %r11
	movq	%rdi, 72(%r11)
.L9028:
	movq	-2680(%rbp), %rax
	movzbl	66(%rax), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L11477
.L9026:
	movq	-1528(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L11478
	movq	-1528(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1528(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2696(%rbp), %rbx
	movzbl	16(%rbx), %eax
	jmp	.L9015
.L11478:
	movq	%rax, (%rdx)
	movq	-2696(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L9015
.L11477:
	movq	-1528(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r11
	movq	-1520(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%r11, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L9026
	movq	-2680(%rbp), %r9
	movq	144(%rax), %r12
	movq	8(%r9), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L9026
.L9121:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11479
	cmpq	$0, 32(%rdx)
	jne	.L9028
	movq	-2688(%rbp), %r8
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r8)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9131
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1520(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1520(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2688(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1528(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L9133:
	movq	-2688(%rbp), %rcx
	movq	current_class_type(%rip), %rbx
	movq	152(%rcx), %r8
	movq	%rbx, 64(%rcx)
	movq	%rbx, 16(%r8)
	jmp	.L9028
.L9131:
	movq	-1520(%rbp), %r9
	movq	-2688(%rbp), %rdx
	movq	%r9, 72(%rdx)
	jmp	.L9133
.L11479:
	movq	-2688(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9126
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	-1520(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1520(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2688(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-1528(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L9128:
	movq	current_function_decl(%rip), %rdx
	movq	-2688(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L9028
.L9126:
	movq	-1520(%rbp), %rdi
	movq	-2688(%rbp), %r11
	movq	%rdi, 72(%r11)
	jmp	.L9128
.L11476:
	cmpb	$95, 1(%rax)
	jne	.L9120
	movq	-2688(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L9120
.L11475:
	cmpq	$0, 72(%rbx)
	je	.L11480
.L10830:
	movq	-1520(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L9106
.L11480:
	movq	-2688(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -1536(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9101
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2688(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-1536(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10827:
	movq	-1520(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L9106
.L9101:
	movq	-2688(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10829
.L11474:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9099
.L10828:
	movq	-1520(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L9106
.L9095:
	movq	-2688(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2688(%rbp)
	jmp	.L10830
.L11473:
	movq	global_binding_level(%rip), %r10
	movl	$1, %r12d
	cmpq	%r10, current_binding_level(%rip)
	je	.L9077
	movq	-1520(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L9078
.L9077:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9079
	movq	-1520(%rbp), %r11
	movq	56(%r11), %rcx
	testq	%rcx, %rcx
	jne	.L10199
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L9080
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11481
.L9080:
	testq	%rcx, %rcx
	jne	.L10199
.L10200:
	movq	-1520(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10199
	movq	-1520(%rbp), %rsi
	movq	40(%rsi), %rcx
.L9078:
	testq	%rcx, %rcx
	je	.L9082
.L10199:
	cmpb	$32, 16(%rcx)
	je	.L9082
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9082
	movq	-1520(%rbp), %r9
	movq	8(%r9), %rax
	testq	%rax, %rax
	je	.L9090
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11194
	testl	%r12d, %r12d
	jle	.L11482
.L11194:
	movq	%rax, %rcx
.L9082:
	movq	-1528(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L9076
	jmp	.L10827
.L11482:
	testl	%edx, %edx
	jg	.L11194
	testl	%r12d, %r12d
	je	.L9082
	movq	-1520(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11194
.L9090:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L9082
.L11481:
	xorl	%ecx, %ecx
	movq	-1520(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9082
	testq	%rax, %rax
	je	.L10200
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L9080
.L9079:
	movq	-1520(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L9078
.L11472:
	cmpb	$95, 1(%rax)
	jne	.L9075
	jmp	.L9106
.L9069:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9070
	movq	-1520(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L11193
.L9068:
	movq	80(%rcx), %rax
	movq	%rax, -2688(%rbp)
	jmp	.L9073
.L11471:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L9034
	movq	80(%rax), %rbx
.L9034:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9067
.L9066:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L9043
	cmpl	$32, %eax
	je	.L11483
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L9037:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L9067
	jmp	.L9066
.L11483:
	movq	8(%rbx), %rdx
	movq	-1528(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r11
	movq	72(%r11), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10895
	movq	64(%rbx), %rbx
	jmp	.L9037
.L10895:
	movq	32(%rax), %rcx
	jmp	.L9067
.L9043:
	movq	-1528(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L9032
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L9046
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L9047
.L9046:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L9048
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10197
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L9049
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11484
.L9049:
	testq	%rcx, %rcx
	jne	.L10197
.L10198:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10197
.L9048:
	movq	40(%rbx), %rcx
.L9047:
	testq	%rcx, %rcx
	je	.L10277
.L10197:
	cmpb	$32, 16(%rcx)
	je	.L9067
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L9067
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L9059
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11192
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11485
.L11192:
	movq	%rax, %rcx
	jmp	.L9067
.L11485:
	testl	%edx, %edx
	jg	.L11192
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L9067
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11192
.L9059:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L9067
	jmp	.L11192
.L11484:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L9067
	testq	%rax, %rax
	je	.L10198
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L9049
.L9030:
	movq	-1520(%rbp), %rsi
	movq	-1528(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1544(%rbp)
	je	.L9107
	movq	-1520(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11195:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L9108:
	movq	-1528(%rbp), %rdx
	movq	-1520(%rbp), %r11
	movq	%rdx, 8(%r11)
	movq	-1544(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L9111
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L9112
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L9112:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1544(%rbp), %rsi
	leaq	8(%rdx), %rbx
	movq	%rsi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L11486
.L9114:
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-1544(%rbp), %rdx
	movq	%rdx, (%r9)
	cmpb	$32, 16(%rdx)
	je	.L11487
.L9111:
	movq	-1544(%rbp), %r12
	movq	%r12, -2688(%rbp)
	jmp	.L10830
.L11487:
	cmpq	$0, 72(%rdx)
	jne	.L9111
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -1552(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L9116
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1544(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1552(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L9111
.L9116:
	movq	-1544(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L9111
.L11486:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L9114
.L9107:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L9108
	movq	-1520(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L11195
.L11470:
	cmpq	$0, class_binding_level(%rip)
	je	.L9029
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L9028
	jmp	.L9029
.L11469:
	movq	-1520(%rbp), %rdi
	movq	-1528(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L9025
	.p2align 6,,7
.L8996:
	movq	-2712(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10821
	movl	flag_traditional(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L10825
	testb	$8, 18(%r14)
	je	.L10825
	testb	$8, 18(%r13)
	jne	.L10825
	testb	$9, 53(%r13)
	jne	.L10825
	cmpq	%r13, current_function_decl(%rip)
	je	.L11488
.L9005:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L9006
	cmpq	$0, 8(%rax)
	jne	.L11489
.L9006:
	movq	-2712(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11191:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2712(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10825
.L11489:
	movq	-2712(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11191
.L11488:
	movq	-2712(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L9005
	.p2align 6,,7
.L11447:
	cmpq	$0, 64(%rcx)
	jne	.L8994
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L8994
.L11446:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2712(%rbp)
	call	error_with_decl
	jmp	.L8992
.L8985:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L8987
.L8991:
	cmpq	%r14, 56(%rax)
	je	.L8987
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L8991
.L8987:
	movq	%rax, -2712(%rbp)
	jmp	.L8984
.L11445:
	movq	40(%r14), %rax
	jmp	.L8987
.L11444:
	movq	56(%r13), %r14
	jmp	.L8981
.L11443:
	testb	$32, 53(%r13)
	jne	.L8979
	jmp	.L8980
.L10817:
	movzbl	16(%r13), %edx
	jmp	.L8979
.L10806:
	movzbl	16(%r13), %edx
.L8721:
	cmpb	$32, %dl
	je	.L10920
.L8729:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L8867
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L8867
	testb	$1, 53(%rax)
	jne	.L8868
	testb	$8, 18(%rax)
	je	.L8867
.L8868:
	andb	$8, %dl
	je	.L11490
	.p2align 4,,7
.L8867:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10814
	testb	$1, 53(%r13)
	je	.L10814
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rsi
	cmpq	%rsi, current_binding_level(%rip)
	je	.L8871
	movq	48(%r14), %r11
	testq	%r11, %r11
	movq	%r11, %rdx
	jne	.L8872
.L8871:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L8873
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10195
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L8874
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L11491
.L8874:
	testq	%rdx, %rdx
	jne	.L10195
.L10196:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10195
.L8873:
	movq	40(%r14), %rdx
.L8872:
	testq	%rdx, %rdx
	je	.L10276
.L10195:
	cmpb	$32, 16(%rdx)
	je	.L8876
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L8876
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L8884
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11188
	testl	%ebx, %ebx
	jle	.L11492
.L11188:
	movq	%rax, %rdx
.L8876:
	testq	%rdx, %rdx
	jne	.L10814
.L10276:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2664(%rbp)
.L8870:
	cmpq	%rax, -2664(%rbp)
	je	.L11493
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11494
.L11189:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L8915:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11495
.L8926:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L8928
	testq	%r12, %r12
	je	.L8929
	testb	$1, 53(%r13)
	jne	.L8929
	cmpb	$34, 16(%r12)
	je	.L11496
.L8929:
	movl	warn_shadow(%rip), %r11d
	testl	%r11d, %r11d
	je	.L8928
	testb	$1, 53(%r13)
	jne	.L8928
	movl	32(%r13), %r9d
	testl	%r9d, %r9d
	je	.L8928
	testq	%rax, %rax
	jne	.L8928
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L8934
	cmpb	$34, 16(%r12)
	je	.L11497
.L8934:
	cmpq	$0, 56(%r14)
	je	.L8936
	movl	$.LC41, %edi
.L8935:
	testq	%rdi, %rdi
	jne	.L11190
	.p2align 4,,7
.L8928:
	testq	%r12, %r12
	je	.L10815
	movq	-2664(%rbp), %rbx
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%rbx), %rdx
	call	tree_cons
	movq	-2664(%rbp), %r12
	movq	%rax, 16(%r12)
.L10815:
	movzbl	16(%r13), %edx
.L8913:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L8701
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L8701
	cmpb	$18, 16(%rcx)
	je	.L11498
.L8945:
	testb	$64, 46(%rcx)
	je	.L8701
.L8944:
	movq	-2664(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11499
.L10816:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L8701:
	cmpb	$32, %dl
	je	.L11500
.L8947:
	movq	-2664(%rbp), %r9
	cmpq	global_binding_level(%rip), %r9
	movq	(%r9), %rcx
	movq	%rcx, (%r13)
	movq	%r13, (%r9)
	jne	.L8720
	testb	$4, 17(%r13)
	jne	.L8720
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L8720
.L11500:
	testq	%r14, %r14
	je	.L8947
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L8948
	cmpq	class_binding_level(%rip), %rax
	je	.L8949
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L8953
	cmpb	$32, 16(%rax)
	je	.L8951
.L8953:
	cmpq	$0, current_class_type(%rip)
	je	.L8948
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L8948
	cmpb	$32, 16(%rax)
	je	.L8951
.L8948:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L8952
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L8951
	cmpb	$-127, %dl
	je	.L11501
.L8952:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L8947
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11502
.L8959:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L8963
	cmpq	class_binding_level(%rip), %rax
	je	.L8964
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L8968
	cmpb	$32, 16(%rax)
	je	.L8966
.L8968:
	cmpq	$0, current_class_type(%rip)
	je	.L8963
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L8963
	cmpb	$32, 16(%rax)
	je	.L8966
.L8963:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L8947
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L8966
	cmpb	$-127, %dl
	jne	.L8947
	movq	$0, 8(%rbx)
	jmp	.L8947
.L8966:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L8947
.L8964:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L8968
.L11502:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%r11b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	cmpb	$1, 16(%r14)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L8959
.L11501:
	movq	$0, 8(%r14)
	jmp	.L8952
.L8951:
	movq	8(%rax), %r10
	movq	%r10, 8(%r14)
	jmp	.L8952
.L8949:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L8953
.L11499:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10816
.L11498:
	movq	8(%rcx), %r8
	testb	$64, 46(%r8)
	jne	.L8944
	jmp	.L8945
.L11190:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L8928
.L8936:
	testq	%r12, %r12
	je	.L8938
	movl	$.LC42, %edi
	jmp	.L8935
.L8938:
	testq	%r15, %r15
	movl	$.LC43, %ecx
	cmovne	%rcx, %rdi
	jmp	.L8935
.L11497:
	movl	$.LC40, %edi
	jmp	.L8935
.L11496:
	cmpb	$34, 16(%r13)
	je	.L8929
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L8930
	movq	56(%rax), %rax
.L8930:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L8928
	movl	$.LC40, %edi
	jmp	.L11190
	.p2align 6,,7
.L11495:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11503
.L8918:
	testq	%r12, %r12
	jne	.L8926
	testq	%r15, %r15
	jne	.L8926
	testb	$1, 53(%r13)
	je	.L8926
	testb	$8, 18(%r13)
	je	.L8926
	orb	$8, 18(%r14)
	jmp	.L8926
	.p2align 6,,7
.L11503:
	testq	%r15, %r15
	je	.L8918
	cmpb	$29, 16(%r13)
	jne	.L8918
	cmpb	$29, 16(%r15)
	jne	.L8918
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11504
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L8921
	movzbl	53(%r13), %r8d
	leal	0(,%rax,8), %r10d
	leaq	88(%r13), %rdx
	andb	$-9, %r8b
	orb	%r10b, %r8b
	movb	%r8b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L8922
	movq	88(%r15), %rax
.L8923:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %rdi
	movq	136(%r15), %rdx
	movzbl	17(%r13), %esi
	movq	%rbx, 72(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %ecx
	movq	%r15, 96(%r13)
	andb	$127, %sil
	shrb	$7, %cl
	movzbl	%cl, %r9d
	movl	%r9d, %r11d
	salb	$7, %r11b
	orb	%r11b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r15), %ecx
.L8921:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L8924
	movzbl	53(%r13), %r8d
	salb	$4, %al
	andb	$-17, %r8b
	orb	%al, %r8b
	movb	%r8b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L8924:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L8918
	cmpq	$0, 88(%r15)
	je	.L8918
	movq	8(%r13), %r10
	cmpq	$0, 24(%r10)
	jne	.L8918
	movq	%rdx, 8(%r13)
	jmp	.L8918
.L8922:
	xorl	%eax, %eax
	jmp	.L8923
	.p2align 6,,7
.L11504:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L8918
	.p2align 6,,7
.L11494:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L8915
	jmp	.L11189
.L11493:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11505
.L8890:
	cmpq	$0, 40(%r14)
	jne	.L8891
	testb	$8, 18(%r13)
	je	.L8891
	orb	$8, 18(%r14)
.L8891:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11506
.L8893:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L8892:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L8904
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L8894
	testb	$1, 18(%rcx)
	je	.L8894
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L8894:
	testq	%rax, %rax
	je	.L8904
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L8899
	testb	$8, 17(%rcx)
	je	.L8899
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L8899:
	testq	%rax, %rax
	je	.L8904
	cmpq	$0, 8(%rax)
	je	.L8904
	cmpb	$29, %dl
	je	.L11507
.L8907:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L8904:
	testb	$8, 18(%r14)
	je	.L8913
	cmpb	$32, %dl
	je	.L8913
	testb	$8, 18(%r13)
	jne	.L8913
	testb	$1, 53(%r13)
	jne	.L8913
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L8909
	cmpq	$0, 8(%rax)
	jne	.L11508
.L8909:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11246:
	xorl	%eax, %eax
	call	warning
	jmp	.L10815
.L11508:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11246
.L11507:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%rbx)
	jne	.L8907
	jmp	.L8904
	.p2align 6,,7
.L11506:
	cmpq	$0, -2640(%rbp)
	je	.L8893
	movq	-2640(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L8892
	jmp	.L8893
.L11505:
	testb	$8, 54(%r13)
	jne	.L8890
	andb	$-9, 18(%r13)
	jmp	.L8890
	.p2align 6,,7
.L10814:
	movq	global_binding_level(%rip), %rax
	jmp	.L8870
.L11492:
	testl	%ecx, %ecx
	jg	.L11188
	testl	%ebx, %ebx
	je	.L8876
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11188
	.p2align 6,,7
.L8884:
	movq	8(%rdx), %r15
	cmpq	error_mark_node(%rip), %r15
	cmove	%r15, %rdx
	jmp	.L8876
.L11491:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L8876
	testq	%rax, %rax
	je	.L10196
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L8874
	.p2align 6,,7
.L11490:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L8867
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L8867
	.p2align 6,,7
.L10920:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L8731
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L8730
.L8731:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10807
	movq	%r13, 80(%rdx)
.L10807:
	movzbl	16(%r13), %eax
.L8734:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L8858
	cmpq	$0, 72(%r12)
	je	.L11509
.L8858:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L8729
	cmpq	$0, 56(%rax)
	je	.L8729
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11510
.L11187:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L8864:
	movq	%r12, 8(%r15)
	jmp	.L8729
.L11510:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L8864
	jmp	.L11187
.L11509:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r8b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L8859
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L8858
.L8859:
	movq	%rbx, 72(%r13)
	jmp	.L8858
.L8730:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L8734
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L8734
	cmpb	$95, 1(%rcx)
	jne	.L8734
	movq	class_binding_level(%rip), %rbx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %r8
	testq	%rbx, %rbx
	movq	%rbx, -2648(%rbp)
	movq	%r8, -1456(%rbp)
	jne	.L8738
	testb	$-128, 66(%rsi)
	movq	%rsi, -2648(%rbp)
	je	.L8738
.L8742:
	movq	-2648(%rbp), %rax
	movq	56(%rax), %r10
	testb	$-128, 66(%r10)
	movq	%r10, -2648(%rbp)
	jne	.L8742
.L8738:
	movq	-2648(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11511
	movq	-2648(%rbp), %rcx
	movq	-1456(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-2648(%rbp), %r11
	movq	%rax, 8(%r11)
.L8744:
	testq	%r15, %r15
	je	.L8745
	movq	-1456(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L8746
	movq	%r15, 80(%rcx)
.L8746:
	movq	-1456(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L8747
	cmpb	$21, 16(%rbx)
	je	.L11512
.L8748:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L8750
	cmpq	$0, 32(%rax)
	je	.L8749
.L8750:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11513
.L8751:
	xorl	%ecx, %ecx
.L8786:
	testq	%rcx, %rcx
	jne	.L8787
.L10275:
	movq	-1456(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1456(%rbp), %rdi
	movq	%rax, -2656(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	current_binding_level(%rip), %rbx
	movq	-2656(%rbp), %rsi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, 112(%rsi)
	movl	$0, 32(%rsi)
	je	.L11514
.L11183:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L8789:
	movq	-1456(%rbp), %rcx
	movq	%rcx, 8(%r15)
.L8792:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11515
.L8794:
	movq	-1456(%rbp), %rbx
	movq	80(%rbx), %rdx
	testq	%rdx, %rdx
	je	.L8825
	cmpb	$32, 16(%rdx)
	je	.L11516
.L8795:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L8814
	movq	-2656(%rbp), %rcx
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1472(%rbp)
	je	.L10811
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L8816
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L8816:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2656(%rbp), %r11
	movq	-1472(%rbp), %r10
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	movq	%r11, 56(%r10)
	ja	.L11517
.L8818:
	movq	-2656(%rbp), %rsi
	movq	%rdx, %rdi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%rdi)
	cmpb	$32, 16(%rsi)
	je	.L11518
.L10811:
	movq	32(%r15), %rax
.L8825:
	cmpb	$36, (%rax)
	je	.L11519
.L8839:
	movq	-2656(%rbp), %rdx
	movq	-1456(%rbp), %r9
	movq	%rdx, 80(%r9)
	movq	current_class_type(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L8842
	cmpq	$0, current_function_decl(%rip)
	je	.L8841
.L8842:
	movq	lang_name_cplusplus(%rip), %r8
	cmpq	%r8, current_lang_name(%rip)
	je	.L8840
.L8841:
	movq	-2656(%rbp), %rax
	movq	%r15, 72(%rax)
.L8747:
	movq	-2648(%rbp), %rsi
	movzbl	66(%rsi), %r10d
	andl	$15, %r10d
	cmpl	$2, %r10d
	je	.L11520
.L8745:
	movq	-1456(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11521
	movq	-1456(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1456(%rbp), %rdi
	movq	%rax, (%rdi)
.L11186:
	movzbl	16(%r12), %eax
	jmp	.L8734
.L11521:
	movq	%rax, (%rsi)
	jmp	.L11186
.L11520:
	movq	-1456(%rbp), %r9
	orb	$64, 18(%r9)
	movq	80(%r9), %r11
	movq	current_class_type(%rip), %rax
	movq	%r11, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L8745
	movq	-2648(%rbp), %rdx
	movq	144(%rax), %r15
	movq	8(%rdx), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L8745
.L8840:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11522
	cmpq	$0, 32(%rdx)
	jne	.L8747
	movq	-2656(%rbp), %r10
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r10)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L8850
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2656(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1456(%rbp), %r11
	movq	%r11, 8(%rax)
.L8852:
	movq	-2656(%rbp), %rax
	movq	current_class_type(%rip), %r8
	movq	152(%rax), %rbx
	movq	%r8, 64(%rax)
	movq	%r8, 16(%rbx)
	jmp	.L8747
.L8850:
	movq	-2656(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L8852
.L11522:
	movq	-2656(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L8845
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2656(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1456(%rbp), %r11
	movq	%r11, 8(%rax)
.L8847:
	movq	current_function_decl(%rip), %r8
	movq	-2656(%rbp), %rbx
	movq	%r8, 64(%rbx)
	jmp	.L8747
.L8845:
	movq	-2656(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L8847
.L11519:
	cmpb	$95, 1(%rax)
	jne	.L8839
	movq	-2656(%rbp), %rdi
	orb	$64, 53(%rdi)
	jmp	.L8839
.L11518:
	cmpq	$0, 72(%rsi)
	jne	.L10811
	movq	8(%rsi), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%rdx, -1480(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L8820
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	-1472(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1472(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2656(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-1480(%rbp), %r9
	movq	%r9, 8(%rax)
	jmp	.L10811
.L8820:
	movq	-1472(%rbp), %rdx
	movq	-2656(%rbp), %rdi
	movq	%rdx, 72(%rdi)
	jmp	.L10811
.L11517:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L8818
.L8814:
	movq	-2656(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2656(%rbp)
	jmp	.L10811
.L11516:
	movq	global_binding_level(%rip), %r10
	cmpq	%r10, current_binding_level(%rip)
	je	.L8796
	movq	48(%r15), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L8797
.L8796:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L8798
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10193
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L8799
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11523
.L8799:
	testq	%rcx, %rcx
	jne	.L10193
.L10194:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10193
.L8798:
	movq	40(%r15), %rcx
.L8797:
	testq	%rcx, %rcx
	je	.L8801
.L10193:
	cmpb	$32, 16(%rcx)
	je	.L8801
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L8801
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L8809
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11184
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11524
.L11184:
	movq	%rax, %rcx
.L8801:
	movq	-1456(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L8795
	jmp	.L10811
.L11524:
	testl	%edx, %edx
	jg	.L11184
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L8801
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11184
.L8809:
	movq	8(%rcx), %rsi
	cmpq	error_mark_node(%rip), %rsi
	cmove	%rsi, %rcx
	jmp	.L8801
.L11523:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L8801
	testq	%rax, %rax
	je	.L10194
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L8799
.L11515:
	cmpb	$95, 1(%rax)
	jne	.L8794
	jmp	.L8825
.L11514:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L8789
	jmp	.L11183
	.p2align 6,,7
.L8787:
	movq	80(%rcx), %r8
	movq	%r8, -2656(%rbp)
	jmp	.L8792
.L11513:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L8753
	movq	80(%rax), %rbx
.L8753:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L8786
.L8785:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L8762
	cmpl	$32, %eax
	je	.L11525
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L8756:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L8786
	jmp	.L8785
.L11525:
	movq	8(%rbx), %rsi
	movq	-1456(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10894
	movq	64(%rbx), %rbx
	jmp	.L8756
.L10894:
	movq	32(%rax), %rcx
	jmp	.L8786
.L8762:
	movq	-1456(%rbp), %rbx
	movq	80(%rbx), %r8
	movq	56(%r8), %rbx
	testq	%rbx, %rbx
	je	.L8751
	movq	global_binding_level(%rip), %r10
	cmpq	%r10, current_binding_level(%rip)
	je	.L8765
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L8766
.L8765:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L8767
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10191
	movq	32(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, -1464(%rbp)
	jne	.L8768
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11526
.L8768:
	testq	%rcx, %rcx
	jne	.L10191
.L10192:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10191
.L8767:
	movq	40(%rbx), %rcx
.L8766:
	testq	%rcx, %rcx
	je	.L10275
.L10191:
	cmpb	$32, 16(%rcx)
	je	.L8786
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L8786
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L8778
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11182
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11527
.L11182:
	movq	%rax, %rcx
	jmp	.L8786
.L11527:
	testl	%edx, %edx
	jg	.L11182
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L8786
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11182
.L8778:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L8786
	jmp	.L11182
.L11526:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L8786
	testq	%rax, %rax
	je	.L10192
	cmpb	$32, 16(%rax)
	cmovne	-1464(%rbp), %rcx
	jmp	.L8768
.L8749:
	movq	-1456(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1488(%rbp)
	je	.L11528
.L11185:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L8827:
	movq	-1456(%rbp), %r9
	movq	%r9, 8(%r15)
	movq	-1488(%rbp), %rbx
	movq	56(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, -1496(%rbp)
	je	.L8830
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L8831
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L8831:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1488(%rbp), %r10
	movq	-1496(%rbp), %rax
	leaq	8(%rdx), %r8
	cmpq	decl_obstack+32(%rip), %r8
	movq	%r10, 56(%rax)
	ja	.L11529
.L8833:
	movq	-1488(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11530
.L8830:
	movq	-1488(%rbp), %rbx
	movq	%rbx, -2656(%rbp)
	jmp	.L10811
.L11530:
	cmpq	$0, 72(%rbx)
	jne	.L8830
	movq	-1488(%rbp), %r11
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r11), %rcx
	movq	%rcx, -1504(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L8835
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1496(%rbp), %r10
	cmpb	$1, 16(%r10)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1496(%rbp), %r9
	movq	32(%rbx), %rdx
	movq	-1496(%rbp), %r8
	movl	$.LC35, %esi
	movq	32(%r9), %rcx
	movl	24(%r8), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1488(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-1504(%rbp), %rdi
	movq	%rdi, 8(%rax)
	jmp	.L8830
.L8835:
	movq	-1496(%rbp), %rcx
	movq	-1488(%rbp), %r11
	movq	%rcx, 72(%r11)
	jmp	.L8830
.L11529:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L8833
.L11528:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L8827
	jmp	.L11185
	.p2align 6,,7
.L11512:
	cmpq	$0, class_binding_level(%rip)
	je	.L8748
	movq	144(%rbx), %rdi
	testb	$16, 3(%rdi)
	jne	.L8747
	jmp	.L8748
	.p2align 6,,7
.L11511:
	movq	-1456(%rbp), %rsi
	movq	8(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L8744
	.p2align 6,,7
.L8715:
	movq	-2640(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10806
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L8720
	testb	$8, 18(%r14)
	je	.L8720
	testb	$8, 18(%r13)
	jne	.L8720
	testb	$9, 53(%r13)
	jne	.L8720
	cmpq	%r13, current_function_decl(%rip)
	je	.L11531
.L8724:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L8725
	cmpq	$0, 8(%rax)
	jne	.L11532
.L8725:
	movq	-2640(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11181:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2640(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L8720
.L11532:
	movq	-2640(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11181
.L11531:
	movq	-2640(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L8724
	.p2align 6,,7
.L11333:
	cmpq	$0, 64(%rsi)
	jne	.L8713
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L8713
.L11332:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2640(%rbp)
	call	error_with_decl
	jmp	.L8711
.L8704:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L8706
	.p2align 4,,7
.L8710:
	cmpq	%r14, 56(%rdi)
	je	.L8706
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L8710
.L8706:
	movq	%rdi, -2640(%rbp)
	jmp	.L8703
.L11331:
	movq	40(%r14), %rdi
	movq	%rdi, -2640(%rbp)
	jmp	.L8703
.L11330:
	movq	56(%r13), %r14
	jmp	.L8700
.L11329:
	testb	$32, 53(%r13)
	jne	.L8698
	jmp	.L8699
.L10802:
	movzbl	16(%r13), %edx
	jmp	.L8698
.L11328:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L8694
	.p2align 6,,7
.L8396:
	movq	ptr_type_node(%rip), %rbx
	jmp	.L8397
.L11327:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L8392
.L11326:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L8092
.L11325:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L7792
.L7782:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7783
	movq	8(%r12), %rsi
	movq	%r12, %rdi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11179
.L7781:
	movl	$32, %edi
	movq	%r12, %rsi
	movq	%r13, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11533
.L11180:
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7788:
	movq	$0, 8(%r12)
	jmp	.L7780
.L11533:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7788
	jmp	.L11180
	.p2align 6,,7
.L11324:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L7492
.L7482:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7483
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11177
.L7481:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r15, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11534
.L11178:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7488:
	movq	$0, 8
	jmp	.L7480
.L11534:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7488
	jmp	.L11178
	.p2align 6,,7
.L11323:
	movq	-2584(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rdx
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_binding_level(%rip), %rax
	movq	%rax, -2616(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10707
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11535
.L7200:
	movq	%rax, 64(%r13)
.L7199:
	cmpb	$32, %dl
	je	.L11536
.L7201:
	testq	%r14, %r14
	je	.L7202
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11537
	cmpq	$0, 48(%r14)
	jne	.L7205
	movq	$0, -2624(%rbp)
.L7204:
	cmpq	$0, -2624(%rbp)
	je	.L7222
	movq	-2624(%rbp), %rcx
	cmpq	error_mark_node(%rip), %rcx
	je	.L11538
.L7212:
	cmpq	$0, -2624(%rbp)
	je	.L10711
	movq	-2624(%rbp), %rsi
	cmpb	$34, 16(%rsi)
	je	.L11539
.L7214:
	movq	-2624(%rbp), %rax
	testq	%rax, %rax
	movq	24(%rax), %r12
	movl	32(%rax), %ebx
	je	.L10711
	movzbl	16(%rax), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L7216
	cmpb	$32, %al
	je	.L7222
	cmpb	$32, %dl
	je	.L10919
	movq	-2624(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10711
.L10715:
	movq	global_binding_level(%rip), %rax
.L7221:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L7475
	movq	-2584(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11176:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7476:
	movq	-2584(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L7198
	movq	-2624(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L7198
.L7475:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7476
	movq	-2584(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11176
.L10711:
	movzbl	16(%r13), %edx
.L7222:
	cmpb	$32, %dl
	je	.L10919
.L7230:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L7368
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L7368
	testb	$1, 53(%rax)
	jne	.L7369
	testb	$8, 18(%rax)
	je	.L7368
.L7369:
	andb	$8, %dl
	je	.L11540
.L7368:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10723
	testb	$1, 53(%r13)
	je	.L10723
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L7372
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L7373
.L7372:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L7374
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10165
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L7375
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11541
.L7375:
	testq	%rcx, %rcx
	jne	.L10165
.L10166:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10165
.L7374:
	movq	40(%r14), %rcx
.L7373:
	testq	%rcx, %rcx
	je	.L10266
.L10165:
	cmpb	$32, 16(%rcx)
	je	.L7377
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L7377
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L7385
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11173
	testl	%ebx, %ebx
	jle	.L11542
.L11173:
	movq	%rax, %rcx
.L7377:
	testq	%rcx, %rcx
	jne	.L10723
.L10266:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2616(%rbp)
.L7371:
	cmpq	%rax, -2616(%rbp)
	je	.L11543
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L11544
.L11174:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L7416:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11545
.L7427:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L7429
	testq	%r12, %r12
	je	.L7430
	testb	$1, 53(%r13)
	jne	.L7430
	cmpb	$34, 16(%r12)
	je	.L11546
.L7430:
	movl	warn_shadow(%rip), %r11d
	testl	%r11d, %r11d
	je	.L7429
	testb	$1, 53(%r13)
	jne	.L7429
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L7429
	testq	%rax, %rax
	jne	.L7429
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L7435
	cmpb	$34, 16(%r12)
	je	.L11547
.L7435:
	cmpq	$0, 56(%r14)
	je	.L7437
	movl	$.LC41, %edi
.L7436:
	testq	%rdi, %rdi
	jne	.L11175
.L7429:
	testq	%r12, %r12
	je	.L10724
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-2616(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10724:
	movzbl	16(%r13), %edx
.L7414:
	leal	-128(%rdx), %ecx
	cmpb	$1, %cl
	jbe	.L7202
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L7202
	cmpb	$18, 16(%rcx)
	je	.L11548
.L7446:
	testb	$64, 46(%rcx)
	je	.L7202
.L7445:
	movq	-2616(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11549
.L10725:
	movzbl	16(%r13), %edx
.L7202:
	cmpb	$32, %dl
	je	.L11550
.L7448:
	movq	-2616(%rbp), %r11
	movq	global_binding_level(%rip), %rax
	movq	(%r11), %rdi
	cmpq	%rax, %r11
	movq	%rdi, (%r13)
	movq	%r13, (%r11)
	je	.L11551
.L7474:
	movq	%r13, -2624(%rbp)
	jmp	.L7221
.L11551:
	testb	$4, 17(%r13)
	jne	.L7474
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L7474
.L11550:
	testq	%r14, %r14
	je	.L7448
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L7449
	cmpq	class_binding_level(%rip), %rax
	je	.L7450
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L7454
	cmpb	$32, 16(%rax)
	je	.L7452
.L7454:
	cmpq	$0, current_class_type(%rip)
	je	.L7449
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L7449
	cmpb	$32, 16(%rax)
	je	.L7452
.L7449:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L7453
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L7452
	cmpb	$-127, %dl
	je	.L11552
.L7453:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L7448
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11553
.L7460:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L7464
	cmpq	class_binding_level(%rip), %rax
	je	.L7465
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L7469
	cmpb	$32, 16(%rax)
	je	.L7467
.L7469:
	cmpq	$0, current_class_type(%rip)
	je	.L7464
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L7464
	cmpb	$32, 16(%rax)
	je	.L7467
.L7464:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L7448
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L7467
	cmpb	$-127, %dl
	jne	.L7448
	movq	$0, 8(%rbx)
	jmp	.L7448
.L7467:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L7448
.L7465:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L7469
.L11553:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r10b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L7460
.L11552:
	movq	$0, 8(%r14)
	jmp	.L7453
.L7452:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L7453
.L7450:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L7454
.L11549:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10725
.L11548:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L7445
	jmp	.L7446
.L11175:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L7429
.L7437:
	testq	%r12, %r12
	je	.L7439
	movl	$.LC42, %edi
	jmp	.L7436
.L7439:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L7436
.L11547:
	movl	$.LC40, %edi
	jmp	.L7436
.L11546:
	cmpb	$34, 16(%r13)
	je	.L7430
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L7431
	movq	56(%rax), %rax
.L7431:
	movzbl	66(%rax), %r10d
	andl	$15, %r10d
	decl	%r10d
	jne	.L7429
	movl	$.LC40, %edi
	jmp	.L11175
	.p2align 6,,7
.L11545:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11554
.L7419:
	testq	%r12, %r12
	jne	.L7427
	testq	%r8, %r8
	jne	.L7427
	testb	$1, 53(%r13)
	je	.L7427
	testb	$8, 18(%r13)
	je	.L7427
	orb	$8, 18(%r14)
	jmp	.L7427
	.p2align 6,,7
.L11554:
	testq	%r8, %r8
	je	.L7419
	cmpb	$29, 16(%r13)
	jne	.L7419
	cmpb	$29, 16(%r8)
	jne	.L7419
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L11555
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L7422
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %ebx
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%bl, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L7423
	movq	88(%r8), %rax
.L7424:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %r9
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%rcx, 136(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %dil
	movzbl	%dil, %r11d
	movl	%r11d, %r10d
	salb	$7, %r10b
	orb	%r10b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L7422:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L7425
	movzbl	53(%r13), %ebx
	salb	$4, %al
	andb	$-17, %bl
	orb	%al, %bl
	movb	%bl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L7425:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L7419
	cmpq	$0, 88(%r8)
	je	.L7419
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L7419
	movq	%rdx, 8(%r13)
	jmp	.L7419
.L7423:
	xorl	%eax, %eax
	jmp	.L7424
.L11555:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L7419
	.p2align 6,,7
.L11544:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7416
	jmp	.L11174
.L11543:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11556
.L7391:
	cmpq	$0, 40(%r14)
	jne	.L7392
	testb	$8, 18(%r13)
	je	.L7392
	orb	$8, 18(%r14)
.L7392:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11557
.L7394:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L7393:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L7405
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L7395
	testb	$1, 18(%rcx)
	je	.L7395
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L7395:
	testq	%rax, %rax
	je	.L7405
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L7400
	testb	$8, 17(%rcx)
	je	.L7400
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L7400:
	testq	%rax, %rax
	je	.L7405
	cmpq	$0, 8(%rax)
	je	.L7405
	cmpb	$29, %dl
	je	.L11558
.L7408:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L7405:
	testb	$8, 18(%r14)
	je	.L7414
	cmpb	$32, %dl
	je	.L7414
	testb	$8, 18(%r13)
	jne	.L7414
	testb	$1, 53(%r13)
	jne	.L7414
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L7410
	cmpq	$0, 8(%rax)
	jne	.L11559
.L7410:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11245:
	xorl	%eax, %eax
	call	warning
	jmp	.L10724
.L11559:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11245
.L11558:
	movq	8(%r13), %r9
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r9)
	jne	.L7408
	jmp	.L7405
	.p2align 6,,7
.L11557:
	cmpq	$0, -2624(%rbp)
	je	.L7394
	movq	-2624(%rbp), %r11
	cmpb	$32, 16(%r11)
	jne	.L7393
	jmp	.L7394
.L11556:
	testb	$8, 54(%r13)
	jne	.L7391
	andb	$-9, 18(%r13)
	jmp	.L7391
	.p2align 6,,7
.L10723:
	movq	global_binding_level(%rip), %rax
	jmp	.L7371
.L11542:
	testl	%esi, %esi
	jg	.L11173
	testl	%ebx, %ebx
	je	.L7377
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11173
	.p2align 6,,7
.L7385:
	movq	8(%rcx), %r8
	cmpq	error_mark_node(%rip), %r8
	cmove	%r8, %rcx
	jmp	.L7377
.L11541:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L7377
	testq	%rax, %rax
	je	.L10166
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L7375
	.p2align 6,,7
.L11540:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L7368
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L7368
	.p2align 6,,7
.L10919:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, -2608(%rbp)
	je	.L7232
	movzbl	16(%rbx), %eax
	cmpb	$32, %al
	je	.L7231
.L7232:
	movq	global_binding_level(%rip), %rbx
	movq	%r13, -2608(%rbp)
	cmpq	%rbx, current_binding_level(%rip)
	jne	.L10716
	movq	%r13, 80(%rdx)
.L10716:
	movzbl	16(%r13), %eax
.L7235:
	cmpb	$32, %al
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$140, %esi
	call	my_friendly_assert
	movq	-2608(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L7359
	cmpq	$0, 72(%rax)
	je	.L11560
.L7359:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L7230
	cmpq	$0, 56(%rax)
	je	.L7230
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -1448(%rbp)
	je	.L7364
	movq	-1448(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
.L11172:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7365:
	movq	-1448(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L7230
.L7364:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7365
	movq	-1448(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11172
.L11560:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -1440(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L7360
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-1440(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L7359
.L7360:
	movq	%rbx, 72(%r13)
	jmp	.L7359
.L7231:
	movq	-2608(%rbp), %r10
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r10), %rcx
	movq	%rcx, -1400(%rbp)
	jne	.L7235
	movq	32(%rcx), %rcx
	cmpb	$36, (%rcx)
	jne	.L7235
	cmpb	$95, 1(%rcx)
	jne	.L7235
	movq	class_binding_level(%rip), %r8
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r8, %r8
	movq	%r8, -2592(%rbp)
	movq	%rdx, -1408(%rbp)
	jne	.L7239
	testb	$-128, 66(%rsi)
	movq	%rsi, -2592(%rbp)
	je	.L7239
.L7243:
	movq	-2592(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2592(%rbp)
	jne	.L7243
.L7239:
	movq	-2592(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11561
	movq	-2592(%rbp), %rcx
	movq	-1400(%rbp), %rdi
	xorl	%eax, %eax
	movq	-1408(%rbp), %rsi
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-2592(%rbp), %rdi
	movq	%rax, 8(%rdi)
.L7245:
	cmpq	$0, -1400(%rbp)
	je	.L7246
	movq	-1408(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L7247
	movq	-1400(%rbp), %r11
	movq	%r11, 80(%rcx)
.L7247:
	movq	-1400(%rbp), %r9
	movq	-1408(%rbp), %rax
	cmpq	%rax, 8(%r9)
	je	.L7248
	cmpb	$21, 16(%rax)
	je	.L11562
.L7249:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L7251
	cmpq	$0, 32(%rax)
	je	.L7250
.L7251:
	movq	lang_name_cplusplus(%rip), %r12
	cmpq	%r12, current_lang_name(%rip)
	je	.L11563
.L7252:
	xorl	%ecx, %ecx
.L7287:
	testq	%rcx, %rcx
	jne	.L7288
.L10265:
	movq	-1400(%rbp), %rsi
	movq	-1408(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1408(%rbp), %rdi
	movq	%rax, -2600(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2600(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L7289
	movq	-1400(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L11169:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7290:
	movq	-1408(%rbp), %rsi
	movq	-1400(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L7293:
	movq	-1400(%rbp), %rdi
	movq	32(%rdi), %rax
	cmpb	$36, (%rax)
	je	.L11564
.L7295:
	movq	-1408(%rbp), %r11
	movq	80(%r11), %rdx
	testq	%rdx, %rdx
	je	.L7326
	cmpb	$32, 16(%rdx)
	je	.L11565
.L7296:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7315
	movq	-2600(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10718
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L7317
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L7317:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2600(%rbp), %r10
	leaq	8(%rdx), %r9
	movq	%r10, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L11566
.L7319:
	movq	-2600(%rbp), %rbx
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r8)
	cmpb	$32, 16(%rbx)
	je	.L11567
.L10719:
	movq	-1400(%rbp), %rsi
	movq	32(%rsi), %rax
.L7326:
	cmpb	$36, (%rax)
	je	.L11568
.L7340:
	movq	current_class_type(%rip), %rdx
	movq	-2600(%rbp), %rdi
	movq	-1408(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rdi, 80(%rcx)
	jne	.L7343
	cmpq	$0, current_function_decl(%rip)
	je	.L7342
.L7343:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L7341
.L7342:
	movq	-1400(%rbp), %r8
	movq	-2600(%rbp), %r10
	movq	%r8, 72(%r10)
.L7248:
	movq	-2592(%rbp), %rax
	movzbl	66(%rax), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L11569
.L7246:
	movq	-1408(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L11570
	movq	-1408(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1408(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2608(%rbp), %rbx
	movzbl	16(%rbx), %eax
	jmp	.L7235
.L11570:
	movq	%rax, (%rdx)
	movq	-2608(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L7235
.L11569:
	movq	-1408(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r8
	movq	-1400(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%r8, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L7246
	movq	-2592(%rbp), %r11
	movq	144(%rax), %r12
	movq	8(%r11), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L7246
.L7341:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11571
	cmpq	$0, 32(%rdx)
	jne	.L7248
	movq	-2600(%rbp), %r9
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r9)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L7351
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1400(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1400(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2600(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1408(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L7353:
	movq	-2600(%rbp), %rcx
	movq	current_class_type(%rip), %rbx
	movq	152(%rcx), %r9
	movq	%rbx, 64(%rcx)
	movq	%rbx, 16(%r9)
	jmp	.L7248
.L7351:
	movq	-1400(%rbp), %r11
	movq	-2600(%rbp), %rdx
	movq	%r11, 72(%rdx)
	jmp	.L7353
.L11571:
	movq	-2600(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L7346
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	-1400(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1400(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2600(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-1408(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L7348:
	movq	current_function_decl(%rip), %rdx
	movq	-2600(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L7248
.L7346:
	movq	-1400(%rbp), %r8
	movq	-2600(%rbp), %rdi
	movq	%r8, 72(%rdi)
	jmp	.L7348
.L11568:
	cmpb	$95, 1(%rax)
	jne	.L7340
	movq	-2600(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L7340
.L11567:
	cmpq	$0, 72(%rbx)
	je	.L11572
.L10720:
	movq	-1400(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L7326
.L11572:
	movq	-2600(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -1416(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L7321
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2600(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-1416(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10717:
	movq	-1400(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L7326
.L7321:
	movq	-2600(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L10719
.L11566:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L7319
.L10718:
	movq	-1400(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L7326
.L7315:
	movq	-2600(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2600(%rbp)
	jmp	.L10720
.L11565:
	movq	global_binding_level(%rip), %r9
	movl	$1, %r12d
	cmpq	%r9, current_binding_level(%rip)
	je	.L7297
	movq	-1400(%rbp), %rbx
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L7298
.L7297:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L7299
	movq	-1400(%rbp), %r8
	movq	56(%r8), %rcx
	testq	%rcx, %rcx
	jne	.L10163
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L7300
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11573
.L7300:
	testq	%rcx, %rcx
	jne	.L10163
.L10164:
	movq	-1400(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10163
	movq	-1400(%rbp), %rsi
	movq	40(%rsi), %rcx
.L7298:
	testq	%rcx, %rcx
	je	.L7302
.L10163:
	cmpb	$32, 16(%rcx)
	je	.L7302
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L7302
	movq	-1400(%rbp), %rdi
	movq	8(%rdi), %rax
	testq	%rax, %rax
	je	.L7310
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11170
	testl	%r12d, %r12d
	jle	.L11574
.L11170:
	movq	%rax, %rcx
.L7302:
	movq	-1408(%rbp), %r11
	cmpq	80(%r11), %rcx
	jne	.L7296
	jmp	.L10717
.L11574:
	testl	%edx, %edx
	jg	.L11170
	testl	%r12d, %r12d
	je	.L7302
	movq	-1400(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11170
.L7310:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L7302
.L11573:
	xorl	%ecx, %ecx
	movq	-1400(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L7302
	testq	%rax, %rax
	je	.L10164
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L7300
.L7299:
	movq	-1400(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L7298
.L11564:
	cmpb	$95, 1(%rax)
	jne	.L7295
	jmp	.L7326
.L7289:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7290
	movq	-1400(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
	jmp	.L11169
.L7288:
	movq	80(%rcx), %rax
	movq	%rax, -2600(%rbp)
	jmp	.L7293
.L11563:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L7254
	movq	80(%rax), %rbx
.L7254:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L7287
.L7286:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L7263
	cmpl	$32, %eax
	je	.L11575
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L7257:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L7287
	jmp	.L7286
.L11575:
	movq	8(%rbx), %rdx
	movq	-1408(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r8
	movq	72(%r8), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10893
	movq	64(%rbx), %rbx
	jmp	.L7257
.L10893:
	movq	32(%rax), %rcx
	jmp	.L7287
.L7263:
	movq	-1408(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L7252
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L7266
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L7267
.L7266:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L7268
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10161
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L7269
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L11576
.L7269:
	testq	%rcx, %rcx
	jne	.L10161
.L10162:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10161
.L7268:
	movq	40(%rbx), %rcx
.L7267:
	testq	%rcx, %rcx
	je	.L10265
.L10161:
	cmpb	$32, 16(%rcx)
	je	.L7287
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L7287
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L7279
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11168
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L11577
.L11168:
	movq	%rax, %rcx
	jmp	.L7287
.L11577:
	testl	%edx, %edx
	jg	.L11168
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L7287
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11168
.L7279:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L7287
	jmp	.L11168
.L11576:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L7287
	testq	%rax, %rax
	je	.L10162
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L7269
.L7250:
	movq	-1400(%rbp), %rsi
	movq	-1408(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1424(%rbp)
	je	.L7327
	movq	-1400(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11171:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7328:
	movq	-1408(%rbp), %rdx
	movq	-1400(%rbp), %r8
	movq	%rdx, 8(%r8)
	movq	-1424(%rbp), %r10
	movq	56(%r10), %r12
	testq	%r12, %r12
	je	.L7331
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L7332
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L7332:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1424(%rbp), %rsi
	leaq	8(%rdx), %rbx
	movq	%rsi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L11578
.L7334:
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-1424(%rbp), %rdx
	movq	%rdx, (%r11)
	cmpb	$32, 16(%rdx)
	je	.L11579
.L7331:
	movq	-1424(%rbp), %r12
	movq	%r12, -2600(%rbp)
	jmp	.L10720
.L11579:
	cmpq	$0, 72(%rdx)
	jne	.L7331
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -1432(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L7336
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1424(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-1432(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L7331
.L7336:
	movq	-1424(%rbp), %r8
	movq	%r12, 72(%r8)
	jmp	.L7331
.L11578:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L7334
.L7327:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7328
	movq	-1400(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11171
.L11562:
	cmpq	$0, class_binding_level(%rip)
	je	.L7249
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L7248
	jmp	.L7249
.L11561:
	movq	-1400(%rbp), %rdi
	movq	-1408(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L7245
	.p2align 6,,7
.L7216:
	movq	-2624(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10711
	movl	flag_traditional(%rip), %r9d
	testl	%r9d, %r9d
	jne	.L10715
	testb	$8, 18(%r14)
	je	.L10715
	testb	$8, 18(%r13)
	jne	.L10715
	testb	$9, 53(%r13)
	jne	.L10715
	cmpq	%r13, current_function_decl(%rip)
	je	.L11580
.L7225:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L7226
	cmpq	$0, 8(%rax)
	jne	.L11581
.L7226:
	movq	-2624(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11167:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2624(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10715
.L11581:
	movq	-2624(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11167
.L11580:
	movq	-2624(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L7225
	.p2align 6,,7
.L11539:
	cmpq	$0, 64(%rsi)
	jne	.L7214
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L7214
.L11538:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2624(%rbp)
	call	error_with_decl
	jmp	.L7212
.L7205:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L7207
.L7211:
	cmpq	%r14, 56(%rax)
	je	.L7207
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L7211
.L7207:
	movq	%rax, -2624(%rbp)
	jmp	.L7204
.L11537:
	movq	40(%r14), %rdi
	movq	%rdi, -2624(%rbp)
	jmp	.L7204
.L11536:
	movq	56(%r13), %r14
	jmp	.L7201
.L11535:
	testb	$32, 53(%r13)
	jne	.L7199
	jmp	.L7200
.L10707:
	movzbl	16(%r13), %edx
	jmp	.L7199
.L11322:
	leal	(%rcx,%rcx), %r8d
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%r8d,%rsi
	movl	%r8d, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L7192
.L7182:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7183
	movq	8(%r12), %rsi
	movq	%r12, %rdi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11165
.L7181:
	movl	$32, %edi
	movq	%r12, %rsi
	movq	%r13, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11582
.L11166:
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L7188:
	movq	$0, 8(%r12)
	jmp	.L7180
.L11582:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L7188
	jmp	.L11166
	.p2align 6,,7
.L6894:
	movb	$64, 45(%rax)
	jmp	.L6895
.L11321:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L6890
.L6880:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6881
	movq	8(%r12), %rsi
	movq	%r12, %rdi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11163
.L6879:
	movl	$32, %edi
	movq	%r12, %rsi
	movq	%r13, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11583
.L11164:
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6886:
	movq	$0, 8(%r12)
	jmp	.L6878
.L11583:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6886
	jmp	.L11164
	.p2align 6,,7
.L10658:
	movzbl	16(%r13), %edx
.L6341:
	cmpb	$32, %dl
	je	.L10918
.L6349:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L6487
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L6487
	testb	$1, 53(%rax)
	jne	.L6488
	testb	$8, 18(%rax)
	je	.L6487
.L6488:
	andb	$8, %dl
	je	.L11584
	.p2align 4,,7
.L6487:
	movl	flag_traditional(%rip), %edi
	testl	%edi, %edi
	je	.L10666
	testb	$1, 53(%r13)
	je	.L10666
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %r12
	cmpq	%r12, current_binding_level(%rip)
	je	.L6491
	movq	48(%r14), %r8
	testq	%r8, %r8
	movq	%r8, %rdx
	jne	.L6492
.L6491:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6493
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10147
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L6494
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11585
.L6494:
	testq	%rdx, %rdx
	jne	.L10147
.L10148:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10147
.L6493:
	movq	40(%r14), %rdx
.L6492:
	testq	%rdx, %rdx
	je	.L10260
.L10147:
	cmpb	$32, 16(%rdx)
	je	.L6496
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L6496
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L6504
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11160
	testl	%ebx, %ebx
	jle	.L11586
.L11160:
	movq	%rax, %rdx
.L6496:
	testq	%rdx, %rdx
	jne	.L10666
.L10260:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2560(%rbp)
.L6490:
	cmpq	%rax, -2560(%rbp)
	je	.L11587
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11588
.L11161:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L6535:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11589
.L6546:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L6548
	testq	%r12, %r12
	je	.L6549
	testb	$1, 53(%r13)
	jne	.L6549
	cmpb	$34, 16(%r12)
	je	.L11590
.L6549:
	movl	warn_shadow(%rip), %r8d
	testl	%r8d, %r8d
	je	.L6548
	testb	$1, 53(%r13)
	jne	.L6548
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L6548
	testq	%rax, %rax
	jne	.L6548
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L6554
	cmpb	$34, 16(%r12)
	je	.L11591
.L6554:
	cmpq	$0, 56(%r14)
	je	.L6556
	movl	$.LC41, %edi
.L6555:
	testq	%rdi, %rdi
	jne	.L11162
	.p2align 4,,7
.L6548:
	testq	%r12, %r12
	je	.L10667
	movq	-2560(%rbp), %r9
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r9), %rdx
	call	tree_cons
	movq	-2560(%rbp), %r12
	movq	%rax, 16(%r12)
.L10667:
	movzbl	16(%r13), %edx
.L6533:
	leal	-128(%rdx), %ebx
	cmpb	$1, %bl
	jbe	.L6321
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L6321
	cmpb	$18, 16(%rcx)
	je	.L11592
.L6565:
	testb	$64, 46(%rcx)
	je	.L6321
.L6564:
	movq	-2560(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11593
.L10668:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L6321:
	cmpb	$32, %dl
	je	.L11594
.L6567:
	movq	-2560(%rbp), %rdi
	cmpq	global_binding_level(%rip), %rdi
	movq	(%rdi), %r11
	movq	%r11, (%r13)
	movq	%r13, (%rdi)
	jne	.L6340
	testb	$4, 17(%r13)
	jne	.L6340
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6340
.L11594:
	testq	%r14, %r14
	je	.L6567
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6568
	cmpq	class_binding_level(%rip), %rax
	je	.L6569
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L6573
	cmpb	$32, 16(%rax)
	je	.L6571
.L6573:
	cmpq	$0, current_class_type(%rip)
	je	.L6568
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L6568
	cmpb	$32, 16(%rax)
	je	.L6571
.L6568:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L6572
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6571
	cmpb	$-127, %dl
	je	.L11595
.L6572:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L6567
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11596
.L6579:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6583
	cmpq	class_binding_level(%rip), %rax
	je	.L6584
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L6588
	cmpb	$32, 16(%rax)
	je	.L6586
.L6588:
	cmpq	$0, current_class_type(%rip)
	je	.L6583
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L6583
	cmpb	$32, 16(%rax)
	je	.L6586
.L6583:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L6567
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6586
	cmpb	$-127, %dl
	jne	.L6567
	movq	$0, 8(%rbx)
	jmp	.L6567
.L6586:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L6567
.L6584:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6588
.L11596:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%cl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L6579
.L11595:
	movq	$0, 8(%r14)
	jmp	.L6572
.L6571:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L6572
.L6569:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6573
.L11593:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10668
.L11592:
	movq	8(%rcx), %r10
	testb	$64, 46(%r10)
	jne	.L6564
	jmp	.L6565
.L11162:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L6548
.L6556:
	testq	%r12, %r12
	je	.L6558
	movl	$.LC42, %edi
	jmp	.L6555
.L6558:
	testq	%r15, %r15
	movl	$.LC43, %r11d
	cmovne	%r11, %rdi
	jmp	.L6555
.L11591:
	movl	$.LC40, %edi
	jmp	.L6555
.L11590:
	cmpb	$34, 16(%r13)
	je	.L6549
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L6550
	movq	56(%rax), %rax
.L6550:
	movzbl	66(%rax), %edi
	andl	$15, %edi
	decl	%edi
	jne	.L6548
	movl	$.LC40, %edi
	jmp	.L11162
	.p2align 6,,7
.L11589:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11597
.L6538:
	testq	%r12, %r12
	jne	.L6546
	testq	%r15, %r15
	jne	.L6546
	testb	$1, 53(%r13)
	je	.L6546
	testb	$8, 18(%r13)
	je	.L6546
	orb	$8, 18(%r14)
	jmp	.L6546
	.p2align 6,,7
.L11597:
	testq	%r15, %r15
	je	.L6538
	cmpb	$29, 16(%r13)
	jne	.L6538
	cmpb	$29, 16(%r15)
	jne	.L6538
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11598
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L6541
	movzbl	53(%r13), %r10d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r10b
	orb	%sil, %r10b
	movb	%r10b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L6542
	movq	88(%r15), %rax
.L6543:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %r9
	movq	136(%r15), %rdx
	movzbl	17(%r13), %edi
	movq	%rbx, 72(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %ecx
	movq	%r15, 96(%r13)
	andb	$127, %dil
	shrb	$7, %cl
	movzbl	%cl, %r11d
	movl	%r11d, %r8d
	salb	$7, %r8b
	orb	%r8b, %dil
	movb	%dil, 17(%r13)
	movzbl	53(%r15), %ecx
.L6541:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L6544
	movzbl	53(%r13), %r10d
	salb	$4, %al
	andb	$-17, %r10b
	orb	%al, %r10b
	movb	%r10b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L6544:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L6538
	cmpq	$0, 88(%r15)
	je	.L6538
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L6538
	movq	%rdx, 8(%r13)
	jmp	.L6538
.L6542:
	xorl	%eax, %eax
	jmp	.L6543
	.p2align 6,,7
.L11598:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L6538
	.p2align 6,,7
.L11588:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6535
	jmp	.L11161
.L11587:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11599
.L6510:
	cmpq	$0, 40(%r14)
	jne	.L6511
	testb	$8, 18(%r13)
	je	.L6511
	orb	$8, 18(%r14)
.L6511:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11600
.L6513:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L6512:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6524
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L6514
	testb	$1, 18(%rcx)
	je	.L6514
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L6514:
	testq	%rax, %rax
	je	.L6524
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L6519
	testb	$8, 17(%rcx)
	je	.L6519
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L6519:
	testq	%rax, %rax
	je	.L6524
	cmpq	$0, 8(%rax)
	je	.L6524
	cmpb	$29, %dl
	je	.L11601
.L6527:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L6524:
	testb	$8, 18(%r14)
	je	.L6533
	cmpb	$32, %dl
	je	.L6533
	testb	$8, 18(%r13)
	jne	.L6533
	testb	$1, 53(%r13)
	jne	.L6533
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6529
	cmpq	$0, 8(%rax)
	jne	.L11602
.L6529:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11244:
	xorl	%eax, %eax
	call	warning
	jmp	.L10667
.L11602:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11244
.L11601:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %r9
	cmpq	%r9, 8(%rbx)
	jne	.L6527
	jmp	.L6524
	.p2align 6,,7
.L11600:
	cmpq	$0, -2536(%rbp)
	je	.L6513
	movq	-2536(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L6512
	jmp	.L6513
.L11599:
	testb	$8, 54(%r13)
	jne	.L6510
	andb	$-9, 18(%r13)
	jmp	.L6510
	.p2align 6,,7
.L10666:
	movq	global_binding_level(%rip), %rax
	jmp	.L6490
.L11586:
	testl	%ecx, %ecx
	jg	.L11160
	testl	%ebx, %ebx
	je	.L6496
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11160
	.p2align 6,,7
.L6504:
	movq	8(%rdx), %r15
	cmpq	error_mark_node(%rip), %r15
	cmove	%r15, %rdx
	jmp	.L6496
.L11585:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L6496
	testq	%rax, %rax
	je	.L10148
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L6494
	.p2align 6,,7
.L11584:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L6487
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L6487
	.p2align 6,,7
.L10918:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L6351
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L6350
.L6351:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10659
	movq	%r13, 80(%rdx)
.L10659:
	movzbl	16(%r13), %eax
.L6354:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L6478
	cmpq	$0, 72(%r12)
	je	.L11603
.L6478:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L6349
	cmpq	$0, 56(%rax)
	je	.L6349
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11604
.L11159:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6484:
	movq	%r12, 8(%r15)
	jmp	.L6349
.L11604:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6484
	jmp	.L11159
.L11603:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%al
	movq	8(%r13), %r15
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L6479
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L6478
.L6479:
	movq	%rbx, 72(%r13)
	jmp	.L6478
.L6350:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L6354
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L6354
	cmpb	$95, 1(%rcx)
	jne	.L6354
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r9, %r9
	movq	%r9, -2544(%rbp)
	movq	%rax, -1344(%rbp)
	jne	.L6358
	testb	$-128, 66(%rsi)
	movq	%rsi, -2544(%rbp)
	je	.L6358
.L6362:
	movq	-2544(%rbp), %rcx
	movq	56(%rcx), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2544(%rbp)
	jne	.L6362
.L6358:
	movq	-2544(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11605
	movq	-2544(%rbp), %r8
	movq	-1344(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-2544(%rbp), %rbx
	movq	%rax, 8(%rbx)
.L6364:
	testq	%r15, %r15
	je	.L6365
	movq	-1344(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L6366
	movq	%r15, 80(%rcx)
.L6366:
	movq	-1344(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L6367
	cmpb	$21, 16(%rbx)
	je	.L11606
.L6368:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L6370
	cmpq	$0, 32(%rax)
	je	.L6369
.L6370:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11607
.L6371:
	xorl	%ecx, %ecx
.L6406:
	testq	%rcx, %rcx
	jne	.L6407
.L10259:
	movq	-1344(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1344(%rbp), %rdi
	movq	%rax, -2552(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2552(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11608
.L11155:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6409:
	movq	-1344(%rbp), %rcx
	movq	%rcx, 8(%r15)
.L6412:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11609
.L6414:
	movq	-1344(%rbp), %r10
	movq	80(%r10), %rdx
	testq	%rdx, %rdx
	je	.L6445
	cmpb	$32, 16(%rdx)
	je	.L11610
.L6415:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6434
	movq	-2552(%rbp), %rcx
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1360(%rbp)
	je	.L10663
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L6436
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L6436:
	movq	-2552(%rbp), %rdx
	movq	-1360(%rbp), %r8
	movq	%rdx, 56(%r8)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %r10
	cmpq	decl_obstack+32(%rip), %r10
	ja	.L11611
.L6438:
	movq	-2552(%rbp), %rsi
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r11)
	cmpb	$32, 16(%rsi)
	je	.L11612
.L10663:
	movq	32(%r15), %rax
.L6445:
	cmpb	$36, (%rax)
	je	.L11613
.L6459:
	movq	-2552(%rbp), %rdx
	movq	-1344(%rbp), %r9
	movq	%rdx, 80(%r9)
	movq	current_class_type(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L6462
	cmpq	$0, current_function_decl(%rip)
	je	.L6461
.L6462:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L6460
.L6461:
	movq	-2552(%rbp), %rax
	movq	%r15, 72(%rax)
.L6367:
	movq	-2544(%rbp), %rsi
	movzbl	66(%rsi), %r10d
	andl	$15, %r10d
	cmpl	$2, %r10d
	je	.L11614
.L6365:
	movq	-1344(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11615
	movq	-1344(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1344(%rbp), %rbx
	movq	%rax, (%rbx)
.L11158:
	movzbl	16(%r12), %eax
	jmp	.L6354
.L11615:
	movq	%rax, (%rsi)
	jmp	.L11158
.L11614:
	movq	-1344(%rbp), %r11
	orb	$64, 18(%r11)
	movq	80(%r11), %r8
	movq	current_class_type(%rip), %rax
	movq	%r8, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L6365
	movq	-2544(%rbp), %r9
	movq	144(%rax), %r15
	movq	8(%r9), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L6365
.L6460:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11616
	cmpq	$0, 32(%rdx)
	jne	.L6367
	movq	-2552(%rbp), %r10
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r10)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6470
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2552(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1344(%rbp), %r8
	movq	%r8, 8(%rax)
.L6472:
	movq	-2552(%rbp), %rax
	movq	current_class_type(%rip), %rdi
	movq	152(%rax), %rbx
	movq	%rdi, 64(%rax)
	movq	%rdi, 16(%rbx)
	jmp	.L6367
.L6470:
	movq	-2552(%rbp), %rdx
	movq	%r15, 72(%rdx)
	jmp	.L6472
.L11616:
	movq	-2552(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6465
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2552(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1344(%rbp), %r8
	movq	%r8, 8(%rax)
.L6467:
	movq	current_function_decl(%rip), %rdi
	movq	-2552(%rbp), %rbx
	movq	%rdi, 64(%rbx)
	jmp	.L6367
.L6465:
	movq	-2552(%rbp), %rdx
	movq	%r15, 72(%rdx)
	jmp	.L6467
.L11613:
	cmpb	$95, 1(%rax)
	jne	.L6459
	movq	-2552(%rbp), %r11
	orb	$64, 53(%r11)
	jmp	.L6459
.L11612:
	cmpq	$0, 72(%rsi)
	jne	.L10663
	movq	8(%rsi), %rdi
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	%rdi, -1368(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6440
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	-1360(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1360(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2552(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-1368(%rbp), %r9
	movq	%r9, 8(%rax)
	jmp	.L10663
.L6440:
	movq	-1360(%rbp), %r11
	movq	-2552(%rbp), %rdx
	movq	%r11, 72(%rdx)
	jmp	.L10663
.L11611:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L6438
.L6434:
	movq	-2552(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2552(%rbp)
	jmp	.L10663
.L11610:
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	je	.L6416
	movq	48(%r15), %rdx
	testq	%rdx, %rdx
	movq	%rdx, %rcx
	jne	.L6417
.L6416:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6418
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10145
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L6419
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11617
.L6419:
	testq	%rcx, %rcx
	jne	.L10145
.L10146:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10145
.L6418:
	movq	40(%r15), %rcx
.L6417:
	testq	%rcx, %rcx
	je	.L6421
.L10145:
	cmpb	$32, 16(%rcx)
	je	.L6421
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L6421
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L6429
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11156
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11618
.L11156:
	movq	%rax, %rcx
.L6421:
	movq	-1344(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L6415
	jmp	.L10663
.L11618:
	testl	%edx, %edx
	jg	.L11156
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L6421
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11156
.L6429:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L6421
.L11617:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L6421
	testq	%rax, %rax
	je	.L10146
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L6419
.L11609:
	cmpb	$95, 1(%rax)
	jne	.L6414
	jmp	.L6445
.L11608:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6409
	jmp	.L11155
	.p2align 6,,7
.L6407:
	movq	80(%rcx), %rsi
	movq	%rsi, -2552(%rbp)
	jmp	.L6412
.L11607:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L6373
	movq	80(%rax), %rbx
.L6373:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L6406
.L6405:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L6382
	cmpl	$32, %eax
	je	.L11619
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L6376:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L6406
	jmp	.L6405
.L11619:
	movq	8(%rbx), %r9
	movq	-1344(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r9), %r11
	movq	72(%r11), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10892
	movq	64(%rbx), %rbx
	jmp	.L6376
.L10892:
	movq	32(%rax), %rcx
	jmp	.L6406
.L6382:
	movq	-1344(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L6371
	movq	global_binding_level(%rip), %r10
	cmpq	%r10, current_binding_level(%rip)
	je	.L6385
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L6386
.L6385:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6387
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10143
	movq	32(%rdi), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1352(%rbp)
	jne	.L6388
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11620
.L6388:
	testq	%rcx, %rcx
	jne	.L10143
.L10144:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10143
.L6387:
	movq	40(%rbx), %rcx
.L6386:
	testq	%rcx, %rcx
	je	.L10259
.L10143:
	cmpb	$32, 16(%rcx)
	je	.L6406
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L6406
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L6398
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11154
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11621
.L11154:
	movq	%rax, %rcx
	jmp	.L6406
.L11621:
	testl	%edx, %edx
	jg	.L11154
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L6406
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11154
.L6398:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L6406
	jmp	.L11154
.L11620:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L6406
	testq	%rax, %rax
	je	.L10144
	cmpb	$32, 16(%rax)
	cmovne	-1352(%rbp), %rcx
	jmp	.L6388
.L6369:
	movq	-1344(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1376(%rbp)
	je	.L11622
.L11157:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6447:
	movq	-1344(%rbp), %r9
	movq	%r9, 8(%r15)
	movq	-1376(%rbp), %rdi
	movq	56(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, -1384(%rbp)
	je	.L6450
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L6451
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L6451:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1376(%rbp), %r10
	movq	-1384(%rbp), %rax
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	movq	%r10, 56(%rax)
	ja	.L11623
.L6453:
	movq	-1376(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11624
.L6450:
	movq	-1376(%rbp), %rbx
	movq	%rbx, -2552(%rbp)
	jmp	.L10663
.L11624:
	cmpq	$0, 72(%rbx)
	jne	.L6450
	movq	-1376(%rbp), %r8
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r8), %rcx
	movq	%rcx, -1392(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6455
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1384(%rbp), %r10
	cmpb	$1, 16(%r10)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1384(%rbp), %r9
	movq	32(%rbx), %rdx
	movq	-1384(%rbp), %rdi
	movl	$.LC35, %esi
	movq	32(%r9), %rcx
	movl	24(%rdi), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1376(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-1392(%rbp), %r11
	movq	%r11, 8(%rax)
	jmp	.L6450
.L6455:
	movq	-1384(%rbp), %rcx
	movq	-1376(%rbp), %r8
	movq	%rcx, 72(%r8)
	jmp	.L6450
.L11623:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L6453
.L11622:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6447
	jmp	.L11157
	.p2align 6,,7
.L11606:
	cmpq	$0, class_binding_level(%rip)
	je	.L6368
	movq	144(%rbx), %rdi
	testb	$16, 3(%rdi)
	jne	.L6367
	jmp	.L6368
	.p2align 6,,7
.L11605:
	movq	-1344(%rbp), %rsi
	movq	8(%rcx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2544(%rbp), %r10
	movq	%rax, 8(%r10)
	jmp	.L6364
	.p2align 6,,7
.L6335:
	movq	-2536(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10658
	movl	flag_traditional(%rip), %r11d
	testl	%r11d, %r11d
	jne	.L6340
	testb	$8, 18(%r14)
	je	.L6340
	testb	$8, 18(%r13)
	jne	.L6340
	testb	$9, 53(%r13)
	jne	.L6340
	cmpq	%r13, current_function_decl(%rip)
	je	.L11625
.L6344:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6345
	cmpq	$0, 8(%rax)
	jne	.L11626
.L6345:
	movq	-2536(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11153:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2536(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L6340
.L11626:
	movq	-2536(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11153
.L11625:
	movq	-2536(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L6344
	.p2align 6,,7
.L11320:
	cmpq	$0, 64(%rcx)
	jne	.L6333
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L6333
.L11319:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2536(%rbp)
	call	error_with_decl
	jmp	.L6331
.L6324:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L6326
	.p2align 4,,7
.L6330:
	cmpq	%r14, 56(%rdi)
	je	.L6326
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L6330
.L6326:
	movq	%rdi, -2536(%rbp)
	jmp	.L6323
.L11318:
	movq	40(%r14), %rdi
	movq	%rdi, -2536(%rbp)
	jmp	.L6323
.L11317:
	movq	56(%r13), %r14
	jmp	.L6320
.L11316:
	testb	$32, 53(%r13)
	jne	.L6318
	jmp	.L6319
.L10654:
	movzbl	16(%r13), %edx
	jmp	.L6318
.L10643:
	movzbl	16(%r13), %edx
.L6065:
	cmpb	$32, %dl
	je	.L10917
.L6073:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L6211
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L6211
	testb	$1, 53(%rax)
	jne	.L6212
	testb	$8, 18(%rax)
	je	.L6211
.L6212:
	andb	$8, %dl
	je	.L11627
	.p2align 4,,7
.L6211:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10651
	testb	$1, 53(%r13)
	je	.L10651
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rsi
	cmpq	%rsi, current_binding_level(%rip)
	je	.L6215
	movq	48(%r14), %r9
	testq	%r9, %r9
	movq	%r9, %rdx
	jne	.L6216
.L6215:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6217
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10141
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L6218
	movq	144(%rdi), %r15
	testb	$1, 3(%r15)
	jne	.L11628
.L6218:
	testq	%rdx, %rdx
	jne	.L10141
.L10142:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10141
.L6217:
	movq	40(%r14), %rdx
.L6216:
	testq	%rdx, %rdx
	je	.L10258
.L10141:
	cmpb	$32, 16(%rdx)
	je	.L6220
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L6220
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L6228
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11150
	testl	%ebx, %ebx
	jle	.L11629
.L11150:
	movq	%rax, %rdx
.L6220:
	testq	%rdx, %rdx
	jne	.L10651
.L10258:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2528(%rbp)
.L6214:
	cmpq	%rax, -2528(%rbp)
	je	.L11630
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11631
.L11151:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L6259:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11632
.L6270:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L6272
	testq	%r12, %r12
	je	.L6273
	testb	$1, 53(%r13)
	jne	.L6273
	cmpb	$34, 16(%r12)
	je	.L11633
.L6273:
	movl	warn_shadow(%rip), %r9d
	testl	%r9d, %r9d
	je	.L6272
	testb	$1, 53(%r13)
	jne	.L6272
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L6272
	testq	%rax, %rax
	jne	.L6272
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L6278
	cmpb	$34, 16(%r12)
	je	.L11634
.L6278:
	cmpq	$0, 56(%r14)
	je	.L6280
	movl	$.LC41, %edi
.L6279:
	testq	%rdi, %rdi
	jne	.L11152
	.p2align 4,,7
.L6272:
	testq	%r12, %r12
	je	.L10652
	movq	-2528(%rbp), %rbx
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%rbx), %rdx
	call	tree_cons
	movq	-2528(%rbp), %r12
	movq	%rax, 16(%r12)
.L10652:
	movzbl	16(%r13), %edx
.L6257:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L6045
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L6045
	cmpb	$18, 16(%rcx)
	je	.L11635
.L6289:
	testb	$64, 46(%rcx)
	je	.L6045
.L6288:
	movq	-2528(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11636
.L10653:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L6045:
	cmpb	$32, %dl
	je	.L11637
.L6291:
	movq	-2528(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	movq	(%rcx), %r10
	movq	%r10, (%r13)
	movq	%r13, (%rcx)
	jne	.L6064
	testb	$4, 17(%r13)
	jne	.L6064
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6064
.L11637:
	testq	%r14, %r14
	je	.L6291
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6292
	cmpq	class_binding_level(%rip), %rax
	je	.L6293
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L6297
	cmpb	$32, 16(%rax)
	je	.L6295
.L6297:
	cmpq	$0, current_class_type(%rip)
	je	.L6292
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L6292
	cmpb	$32, 16(%rax)
	je	.L6295
.L6292:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L6296
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6295
	cmpb	$-127, %dl
	je	.L11638
.L6296:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L6291
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11639
.L6303:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6307
	cmpq	class_binding_level(%rip), %rax
	je	.L6308
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L6312
	cmpb	$32, 16(%rax)
	je	.L6310
.L6312:
	cmpq	$0, current_class_type(%rip)
	je	.L6307
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L6307
	cmpb	$32, 16(%rax)
	je	.L6310
.L6307:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L6291
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6310
	cmpb	$-127, %dl
	jne	.L6291
	movq	$0, 8(%rbx)
	jmp	.L6291
.L6310:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L6291
.L6308:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6312
.L11639:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%r9b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	cmpb	$1, 16(%r14)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L6303
.L11638:
	movq	$0, 8(%r14)
	jmp	.L6296
.L6295:
	movq	8(%rax), %r11
	movq	%r11, 8(%r14)
	jmp	.L6296
.L6293:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6297
.L11636:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10653
.L11635:
	movq	8(%rcx), %r8
	testb	$64, 46(%r8)
	jne	.L6288
	jmp	.L6289
.L11152:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L6272
.L6280:
	testq	%r12, %r12
	je	.L6282
	movl	$.LC42, %edi
	jmp	.L6279
.L6282:
	testq	%r15, %r15
	movl	$.LC43, %r10d
	cmovne	%r10, %rdi
	jmp	.L6279
.L11634:
	movl	$.LC40, %edi
	jmp	.L6279
.L11633:
	cmpb	$34, 16(%r13)
	je	.L6273
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L6274
	movq	56(%rax), %rax
.L6274:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L6272
	movl	$.LC40, %edi
	jmp	.L11152
	.p2align 6,,7
.L11632:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11640
.L6262:
	testq	%r12, %r12
	jne	.L6270
	testq	%r15, %r15
	jne	.L6270
	testb	$1, 53(%r13)
	je	.L6270
	testb	$8, 18(%r13)
	je	.L6270
	orb	$8, 18(%r14)
	jmp	.L6270
	.p2align 6,,7
.L11640:
	testq	%r15, %r15
	je	.L6262
	cmpb	$29, 16(%r13)
	jne	.L6262
	cmpb	$29, 16(%r15)
	jne	.L6262
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11641
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L6265
	movzbl	53(%r13), %r8d
	leal	0(,%rax,8), %r11d
	leaq	88(%r13), %rdx
	andb	$-9, %r8b
	orb	%r11b, %r8b
	movb	%r8b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L6266
	movq	88(%r15), %rax
.L6267:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %rdi
	movq	136(%r15), %rdx
	movzbl	17(%r13), %esi
	movq	%rbx, 72(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %r10d
	movq	%r15, 96(%r13)
	andb	$127, %sil
	shrb	$7, %r10b
	movzbl	%r10b, %ecx
	movl	%ecx, %r9d
	salb	$7, %r9b
	orb	%r9b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r15), %ecx
.L6265:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L6268
	movzbl	53(%r13), %r8d
	salb	$4, %al
	andb	$-17, %r8b
	orb	%al, %r8b
	movb	%r8b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L6268:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L6262
	cmpq	$0, 88(%r15)
	je	.L6262
	movq	8(%r13), %r11
	cmpq	$0, 24(%r11)
	jne	.L6262
	movq	%rdx, 8(%r13)
	jmp	.L6262
.L6266:
	xorl	%eax, %eax
	jmp	.L6267
	.p2align 6,,7
.L11641:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L6262
	.p2align 6,,7
.L11631:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6259
	jmp	.L11151
.L11630:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11642
.L6234:
	cmpq	$0, 40(%r14)
	jne	.L6235
	testb	$8, 18(%r13)
	je	.L6235
	orb	$8, 18(%r14)
.L6235:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11643
.L6237:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L6236:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6248
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L6238
	testb	$1, 18(%rcx)
	je	.L6238
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L6238:
	testq	%rax, %rax
	je	.L6248
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L6243
	testb	$8, 17(%rcx)
	je	.L6243
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L6243:
	testq	%rax, %rax
	je	.L6248
	cmpq	$0, 8(%rax)
	je	.L6248
	cmpb	$29, %dl
	je	.L11644
.L6251:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L6248:
	testb	$8, 18(%r14)
	je	.L6257
	cmpb	$32, %dl
	je	.L6257
	testb	$8, 18(%r13)
	jne	.L6257
	testb	$1, 53(%r13)
	jne	.L6257
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6253
	cmpq	$0, 8(%rax)
	jne	.L11645
.L6253:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11243:
	xorl	%eax, %eax
	call	warning
	jmp	.L10652
.L11645:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11243
.L11644:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%rbx)
	jne	.L6251
	jmp	.L6248
	.p2align 6,,7
.L11643:
	cmpq	$0, -2504(%rbp)
	je	.L6237
	movq	-2504(%rbp), %r10
	cmpb	$32, 16(%r10)
	jne	.L6236
	jmp	.L6237
.L11642:
	testb	$8, 54(%r13)
	jne	.L6234
	andb	$-9, 18(%r13)
	jmp	.L6234
	.p2align 6,,7
.L10651:
	movq	global_binding_level(%rip), %rax
	jmp	.L6214
.L11629:
	testl	%ecx, %ecx
	jg	.L11150
	testl	%ebx, %ebx
	je	.L6220
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11150
	.p2align 6,,7
.L6228:
	movq	8(%rdx), %rcx
	cmpq	error_mark_node(%rip), %rcx
	cmove	%rcx, %rdx
	jmp	.L6220
.L11628:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L6220
	testq	%rax, %rax
	je	.L10142
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L6218
	.p2align 6,,7
.L11627:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L6211
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L6211
	.p2align 6,,7
.L10917:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L6075
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L6074
.L6075:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10644
	movq	%r13, 80(%rdx)
.L10644:
	movzbl	16(%r13), %eax
.L6078:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L6202
	cmpq	$0, 72(%r12)
	je	.L11646
.L6202:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L6073
	cmpq	$0, 56(%rax)
	je	.L6073
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11647
.L11149:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6208:
	movq	%r12, 8(%r15)
	jmp	.L6073
.L11647:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6208
	jmp	.L11149
.L11646:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r8b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L6203
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L6202
.L6203:
	movq	%rbx, 72(%r13)
	jmp	.L6202
.L6074:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L6078
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L6078
	cmpb	$95, 1(%rcx)
	jne	.L6078
	movq	class_binding_level(%rip), %r8
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r8, %r8
	movq	%r8, -2512(%rbp)
	movq	%rax, -1288(%rbp)
	jne	.L6082
	testb	$-128, 66(%rsi)
	movq	%rsi, -2512(%rbp)
	je	.L6082
.L6086:
	movq	-2512(%rbp), %rdi
	movq	56(%rdi), %rcx
	testb	$-128, 66(%rcx)
	movq	%rcx, -2512(%rbp)
	jne	.L6086
.L6082:
	movq	-2512(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11648
	movq	-2512(%rbp), %rbx
	movq	-1288(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	saveable_tree_cons
	movq	-2512(%rbp), %r9
	movq	%rax, 8(%r9)
.L6088:
	testq	%r15, %r15
	je	.L6089
	movq	-1288(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L6090
	movq	%r15, 80(%rcx)
.L6090:
	movq	-1288(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L6091
	cmpb	$21, 16(%rbx)
	je	.L11649
.L6092:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L6094
	cmpq	$0, 32(%rax)
	je	.L6093
.L6094:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11650
.L6095:
	xorl	%ecx, %ecx
.L6130:
	testq	%rcx, %rcx
	jne	.L6131
.L10257:
	movq	-1288(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1288(%rbp), %rdi
	movq	%rax, -2520(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2520(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11651
.L11145:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6133:
	movq	-1288(%rbp), %rdx
	movq	%rdx, 8(%r15)
.L6136:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11652
.L6138:
	movq	-1288(%rbp), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.L6169
	cmpb	$32, 16(%rdx)
	je	.L11653
.L6139:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6158
	movq	-2520(%rbp), %rax
	movq	56(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1304(%rbp)
	je	.L10648
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L6160
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L6160:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2520(%rbp), %r11
	movq	-1304(%rbp), %rdi
	leaq	8(%rdx), %rcx
	cmpq	decl_obstack+32(%rip), %rcx
	movq	%r11, 56(%rdi)
	ja	.L11654
.L6162:
	movq	-2520(%rbp), %rsi
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r9)
	cmpb	$32, 16(%rsi)
	je	.L11655
.L10648:
	movq	32(%r15), %rax
.L6169:
	cmpb	$36, (%rax)
	je	.L11656
.L6183:
	movq	current_class_type(%rip), %rdx
	movq	-2520(%rbp), %r10
	movq	-1288(%rbp), %r9
	testq	%rdx, %rdx
	movq	%r10, 80(%r9)
	jne	.L6186
	cmpq	$0, current_function_decl(%rip)
	je	.L6185
.L6186:
	movq	lang_name_cplusplus(%rip), %r8
	cmpq	%r8, current_lang_name(%rip)
	je	.L6184
.L6185:
	movq	-2520(%rbp), %rax
	movq	%r15, 72(%rax)
.L6091:
	movq	-2512(%rbp), %rsi
	movzbl	66(%rsi), %r11d
	andl	$15, %r11d
	cmpl	$2, %r11d
	je	.L11657
.L6089:
	movq	-1288(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11658
	movq	-1288(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1288(%rbp), %rdi
	movq	%rax, (%rdi)
.L11148:
	movzbl	16(%r12), %eax
	jmp	.L6078
.L11658:
	movq	%rax, (%rsi)
	jmp	.L11148
.L11657:
	movq	-1288(%rbp), %r9
	orb	$64, 18(%r9)
	movq	80(%r9), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L6089
	movq	-2512(%rbp), %r10
	movq	144(%rax), %r15
	movq	8(%r10), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L6089
.L6184:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11659
	cmpq	$0, 32(%rdx)
	jne	.L6091
	movq	-2520(%rbp), %r11
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r11)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6194
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2520(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1288(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L6196:
	movq	-2520(%rbp), %rax
	movq	current_class_type(%rip), %r8
	movq	152(%rax), %rbx
	movq	%r8, 64(%rax)
	movq	%r8, 16(%rbx)
	jmp	.L6091
.L6194:
	movq	-2520(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L6196
.L11659:
	movq	-2520(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6189
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2520(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1288(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L6191:
	movq	current_function_decl(%rip), %r8
	movq	-2520(%rbp), %rbx
	movq	%r8, 64(%rbx)
	jmp	.L6091
.L6189:
	movq	-2520(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L6191
.L11656:
	cmpb	$95, 1(%rax)
	jne	.L6183
	movq	-2520(%rbp), %rdi
	orb	$64, 53(%rdi)
	jmp	.L6183
.L11655:
	cmpq	$0, 72(%rsi)
	jne	.L10648
	movq	8(%rsi), %r10
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r10, -1312(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6164
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-1304(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1304(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2520(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-1312(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L10648
.L6164:
	movq	-1304(%rbp), %r9
	movq	-2520(%rbp), %rdi
	movq	%r9, 72(%rdi)
	jmp	.L10648
.L11654:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L6162
.L6158:
	movq	-2520(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2520(%rbp)
	jmp	.L10648
.L11653:
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L6140
	movq	48(%r15), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L6141
.L6140:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6142
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10139
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L6143
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L11660
.L6143:
	testq	%rcx, %rcx
	jne	.L10139
.L10140:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10139
.L6142:
	movq	40(%r15), %rcx
.L6141:
	testq	%rcx, %rcx
	je	.L6145
.L10139:
	cmpb	$32, 16(%rcx)
	je	.L6145
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L6145
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L6153
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11146
	movl	$1, %r8d
	testl	%r8d, %r8d
	jle	.L11661
.L11146:
	movq	%rax, %rcx
.L6145:
	movq	-1288(%rbp), %rdx
	cmpq	80(%rdx), %rcx
	jne	.L6139
	jmp	.L10648
.L11661:
	testl	%edx, %edx
	jg	.L11146
	movl	$1, %ebx
	testl	%ebx, %ebx
	je	.L6145
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11146
.L6153:
	movq	8(%rcx), %rsi
	cmpq	error_mark_node(%rip), %rsi
	cmove	%rsi, %rcx
	jmp	.L6145
.L11660:
	movl	$1, %r10d
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %r10d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L6145
	testq	%rax, %rax
	je	.L10140
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L6143
.L11652:
	cmpb	$95, 1(%rax)
	jne	.L6138
	jmp	.L6169
.L11651:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6133
	jmp	.L11145
	.p2align 6,,7
.L6131:
	movq	80(%rcx), %rax
	movq	%rax, -2520(%rbp)
	jmp	.L6136
.L11650:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L6097
	movq	80(%rax), %rbx
.L6097:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L6130
.L6129:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L6106
	cmpl	$32, %eax
	je	.L11662
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L6100:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L6130
	jmp	.L6129
.L11662:
	movq	8(%rbx), %rsi
	movq	-1288(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %r8
	movq	72(%r8), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10891
	movq	64(%rbx), %rbx
	jmp	.L6100
.L10891:
	movq	32(%rax), %rcx
	jmp	.L6130
.L6106:
	movq	-1288(%rbp), %rax
	movq	80(%rax), %rcx
	movq	56(%rcx), %rbx
	testq	%rbx, %rbx
	je	.L6095
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L6109
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L6110
.L6109:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L6111
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10137
	movq	32(%rdi), %r9
	testq	%r9, %r9
	movq	%r9, -1296(%rbp)
	jne	.L6112
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L11663
.L6112:
	testq	%rcx, %rcx
	jne	.L10137
.L10138:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10137
.L6111:
	movq	40(%rbx), %rcx
.L6110:
	testq	%rcx, %rcx
	je	.L10257
.L10137:
	cmpb	$32, 16(%rcx)
	je	.L6130
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L6130
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L6122
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11144
	movl	$1, %r8d
	testl	%r8d, %r8d
	jle	.L11664
.L11144:
	movq	%rax, %rcx
	jmp	.L6130
.L11664:
	testl	%edx, %edx
	jg	.L11144
	movl	$1, %esi
	testl	%esi, %esi
	je	.L6130
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11144
.L6122:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L6130
	jmp	.L11144
.L11663:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L6130
	testq	%rax, %rax
	je	.L10138
	cmpb	$32, 16(%rax)
	cmovne	-1296(%rbp), %rcx
	jmp	.L6112
.L6093:
	movq	-1288(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1320(%rbp)
	je	.L11665
.L11147:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L6171:
	movq	-1288(%rbp), %r8
	movq	%r8, 8(%r15)
	movq	-1320(%rbp), %r10
	movq	56(%r10), %rax
	testq	%rax, %rax
	movq	%rax, -1328(%rbp)
	je	.L6174
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L6175
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L6175:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1320(%rbp), %r11
	movq	-1328(%rbp), %rax
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	movq	%r11, 56(%rax)
	ja	.L11666
.L6177:
	movq	-1320(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11667
.L6174:
	movq	-1320(%rbp), %rbx
	movq	%rbx, -2520(%rbp)
	jmp	.L10648
.L11667:
	cmpq	$0, 72(%rbx)
	jne	.L6174
	movq	-1320(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %rcx
	movq	%rcx, -1336(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L6179
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1328(%rbp), %r11
	cmpb	$1, 16(%r11)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1328(%rbp), %r10
	movq	32(%rbx), %rdx
	movq	-1328(%rbp), %r8
	movl	$.LC35, %esi
	movq	32(%r10), %rcx
	movl	24(%r8), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1320(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-1336(%rbp), %rdi
	movq	%rdi, 8(%rax)
	jmp	.L6174
.L6179:
	movq	-1328(%rbp), %rcx
	movq	-1320(%rbp), %rdx
	movq	%rcx, 72(%rdx)
	jmp	.L6174
.L11666:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L6177
.L11665:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L6171
	jmp	.L11147
	.p2align 6,,7
.L11649:
	cmpq	$0, class_binding_level(%rip)
	je	.L6092
	movq	144(%rbx), %r10
	testb	$16, 3(%r10)
	jne	.L6091
	jmp	.L6092
	.p2align 6,,7
.L11648:
	movq	-1288(%rbp), %rsi
	movq	8(%rcx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2512(%rbp), %r11
	movq	%rax, 8(%r11)
	jmp	.L6088
	.p2align 6,,7
.L6059:
	movq	-2504(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10643
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L6064
	testb	$8, 18(%r14)
	je	.L6064
	testb	$8, 18(%r13)
	jne	.L6064
	testb	$9, 53(%r13)
	jne	.L6064
	cmpq	%r13, current_function_decl(%rip)
	je	.L11668
.L6068:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L6069
	cmpq	$0, 8(%rax)
	jne	.L11669
.L6069:
	movq	-2504(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11143:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2504(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L6064
.L11669:
	movq	-2504(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11143
.L11668:
	movq	-2504(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L6068
	.p2align 6,,7
.L11315:
	cmpq	$0, 64(%rcx)
	jne	.L6057
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L6057
.L11314:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2504(%rbp)
	call	error_with_decl
	jmp	.L6055
.L6048:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L6050
	.p2align 4,,7
.L6054:
	cmpq	%r14, 56(%rdi)
	je	.L6050
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L6054
.L6050:
	movq	%rdi, -2504(%rbp)
	jmp	.L6047
.L11313:
	movq	40(%r14), %rbx
	movq	%rbx, -2504(%rbp)
	jmp	.L6047
.L11312:
	movq	56(%r13), %r14
	jmp	.L6044
.L11311:
	testb	$32, 53(%r13)
	jne	.L6042
	jmp	.L6043
.L10639:
	movzbl	16(%r13), %edx
	jmp	.L6042
.L10628:
	movzbl	16(%r13), %edx
.L5789:
	cmpb	$32, %dl
	je	.L10916
.L5797:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L5935
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5935
	testb	$1, 53(%rax)
	jne	.L5936
	testb	$8, 18(%rax)
	je	.L5935
.L5936:
	andb	$8, %dl
	je	.L11670
	.p2align 4,,7
.L5935:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10636
	testb	$1, 53(%r13)
	je	.L10636
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L5939
	movq	48(%r14), %r8
	testq	%r8, %r8
	movq	%r8, %rdx
	jne	.L5940
.L5939:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5941
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10135
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L5942
	movq	144(%rdi), %r15
	testb	$1, 3(%r15)
	jne	.L11671
.L5942:
	testq	%rdx, %rdx
	jne	.L10135
.L10136:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10135
.L5941:
	movq	40(%r14), %rdx
.L5940:
	testq	%rdx, %rdx
	je	.L10256
.L10135:
	cmpb	$32, 16(%rdx)
	je	.L5944
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L5944
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L5952
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11140
	testl	%ebx, %ebx
	jle	.L11672
.L11140:
	movq	%rax, %rdx
.L5944:
	testq	%rdx, %rdx
	jne	.L10636
.L10256:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2496(%rbp)
.L5938:
	cmpq	%rax, -2496(%rbp)
	je	.L11673
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11674
.L11141:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L5983:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11675
.L5994:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L5996
	testq	%r12, %r12
	je	.L5997
	testb	$1, 53(%r13)
	jne	.L5997
	cmpb	$34, 16(%r12)
	je	.L11676
.L5997:
	movl	warn_shadow(%rip), %r8d
	testl	%r8d, %r8d
	je	.L5996
	testb	$1, 53(%r13)
	jne	.L5996
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L5996
	testq	%rax, %rax
	jne	.L5996
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L6002
	cmpb	$34, 16(%r12)
	je	.L11677
.L6002:
	cmpq	$0, 56(%r14)
	je	.L6004
	movl	$.LC41, %edi
.L6003:
	testq	%rdi, %rdi
	jne	.L11142
	.p2align 4,,7
.L5996:
	testq	%r12, %r12
	je	.L10637
	movq	-2496(%rbp), %r9
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r9), %rdx
	call	tree_cons
	movq	-2496(%rbp), %r12
	movq	%rax, 16(%r12)
.L10637:
	movzbl	16(%r13), %edx
.L5981:
	leal	-128(%rdx), %ebx
	cmpb	$1, %bl
	jbe	.L5769
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L5769
	cmpb	$18, 16(%rcx)
	je	.L11678
.L6013:
	testb	$64, 46(%rcx)
	je	.L5769
.L6012:
	movq	-2496(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11679
.L10638:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L5769:
	cmpb	$32, %dl
	je	.L11680
.L6015:
	movq	-2496(%rbp), %rdi
	cmpq	global_binding_level(%rip), %rdi
	movq	(%rdi), %r11
	movq	%r11, (%r13)
	movq	%r13, (%rdi)
	jne	.L5788
	testb	$4, 17(%r13)
	jne	.L5788
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5788
.L11680:
	testq	%r14, %r14
	je	.L6015
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6016
	cmpq	class_binding_level(%rip), %rax
	je	.L6017
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L6021
	cmpb	$32, 16(%rax)
	je	.L6019
.L6021:
	cmpq	$0, current_class_type(%rip)
	je	.L6016
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L6016
	cmpb	$32, 16(%rax)
	je	.L6019
.L6016:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L6020
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6019
	cmpb	$-127, %dl
	je	.L11681
.L6020:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L6015
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11682
.L6027:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L6031
	cmpq	class_binding_level(%rip), %rax
	je	.L6032
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L6036
	cmpb	$32, 16(%rax)
	je	.L6034
.L6036:
	cmpq	$0, current_class_type(%rip)
	je	.L6031
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L6031
	cmpb	$32, 16(%rax)
	je	.L6034
.L6031:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L6015
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L6034
	cmpb	$-127, %dl
	jne	.L6015
	movq	$0, 8(%rbx)
	jmp	.L6015
.L6034:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L6015
.L6032:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6036
.L11682:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%cl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L6027
.L11681:
	movq	$0, 8(%r14)
	jmp	.L6020
.L6019:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L6020
.L6017:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L6021
.L11679:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10638
.L11678:
	movq	8(%rcx), %r10
	testb	$64, 46(%r10)
	jne	.L6012
	jmp	.L6013
.L11142:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L5996
.L6004:
	testq	%r12, %r12
	je	.L6006
	movl	$.LC42, %edi
	jmp	.L6003
.L6006:
	testq	%r15, %r15
	movl	$.LC43, %r11d
	cmovne	%r11, %rdi
	jmp	.L6003
.L11677:
	movl	$.LC40, %edi
	jmp	.L6003
.L11676:
	cmpb	$34, 16(%r13)
	je	.L5997
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L5998
	movq	56(%rax), %rax
.L5998:
	movzbl	66(%rax), %edi
	andl	$15, %edi
	decl	%edi
	jne	.L5996
	movl	$.LC40, %edi
	jmp	.L11142
	.p2align 6,,7
.L11675:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11683
.L5986:
	testq	%r12, %r12
	jne	.L5994
	testq	%r15, %r15
	jne	.L5994
	testb	$1, 53(%r13)
	je	.L5994
	testb	$8, 18(%r13)
	je	.L5994
	orb	$8, 18(%r14)
	jmp	.L5994
	.p2align 6,,7
.L11683:
	testq	%r15, %r15
	je	.L5986
	cmpb	$29, 16(%r13)
	jne	.L5986
	cmpb	$29, 16(%r15)
	jne	.L5986
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11684
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L5989
	movzbl	53(%r13), %r10d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r10b
	orb	%sil, %r10b
	movb	%r10b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L5990
	movq	88(%r15), %rax
.L5991:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %r9
	movq	136(%r15), %rdx
	movzbl	17(%r13), %edi
	movq	%rbx, 72(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %r11d
	movq	%r15, 96(%r13)
	andb	$127, %dil
	shrb	$7, %r11b
	movzbl	%r11b, %ecx
	movl	%ecx, %r8d
	salb	$7, %r8b
	orb	%r8b, %dil
	movb	%dil, 17(%r13)
	movzbl	53(%r15), %ecx
.L5989:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L5992
	movzbl	53(%r13), %r10d
	salb	$4, %al
	andb	$-17, %r10b
	orb	%al, %r10b
	movb	%r10b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L5992:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L5986
	cmpq	$0, 88(%r15)
	je	.L5986
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L5986
	movq	%rdx, 8(%r13)
	jmp	.L5986
.L5990:
	xorl	%eax, %eax
	jmp	.L5991
	.p2align 6,,7
.L11684:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L5986
	.p2align 6,,7
.L11674:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5983
	jmp	.L11141
.L11673:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11685
.L5958:
	cmpq	$0, 40(%r14)
	jne	.L5959
	testb	$8, 18(%r13)
	je	.L5959
	orb	$8, 18(%r14)
.L5959:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11686
.L5961:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L5960:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5972
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5962
	testb	$1, 18(%rcx)
	je	.L5962
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L5962:
	testq	%rax, %rax
	je	.L5972
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5967
	testb	$8, 17(%rcx)
	je	.L5967
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L5967:
	testq	%rax, %rax
	je	.L5972
	cmpq	$0, 8(%rax)
	je	.L5972
	cmpb	$29, %dl
	je	.L11687
.L5975:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L5972:
	testb	$8, 18(%r14)
	je	.L5981
	cmpb	$32, %dl
	je	.L5981
	testb	$8, 18(%r13)
	jne	.L5981
	testb	$1, 53(%r13)
	jne	.L5981
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5977
	cmpq	$0, 8(%rax)
	jne	.L11688
.L5977:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11242:
	xorl	%eax, %eax
	call	warning
	jmp	.L10637
.L11688:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11242
.L11687:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %r9
	cmpq	%r9, 8(%rbx)
	jne	.L5975
	jmp	.L5972
	.p2align 6,,7
.L11686:
	cmpq	$0, -2472(%rbp)
	je	.L5961
	movq	-2472(%rbp), %r11
	cmpb	$32, 16(%r11)
	jne	.L5960
	jmp	.L5961
.L11685:
	testb	$8, 54(%r13)
	jne	.L5958
	andb	$-9, 18(%r13)
	jmp	.L5958
	.p2align 6,,7
.L10636:
	movq	global_binding_level(%rip), %rax
	jmp	.L5938
.L11672:
	testl	%ecx, %ecx
	jg	.L11140
	testl	%ebx, %ebx
	je	.L5944
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11140
	.p2align 6,,7
.L5952:
	movq	8(%rdx), %rcx
	cmpq	error_mark_node(%rip), %rcx
	cmove	%rcx, %rdx
	jmp	.L5944
.L11671:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L5944
	testq	%rax, %rax
	je	.L10136
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L5942
	.p2align 6,,7
.L11670:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L5935
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L5935
	.p2align 6,,7
.L10916:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L5799
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L5798
.L5799:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10629
	movq	%r13, 80(%rdx)
.L10629:
	movzbl	16(%r13), %eax
.L5802:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L5926
	cmpq	$0, 72(%r12)
	je	.L11689
.L5926:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L5797
	cmpq	$0, 56(%rax)
	je	.L5797
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11690
.L11139:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5932:
	movq	%r12, 8(%r15)
	jmp	.L5797
.L11690:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5932
	jmp	.L11139
.L11689:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%al
	movq	8(%r13), %r15
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L5927
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L5926
.L5927:
	movq	%rbx, 72(%r13)
	jmp	.L5926
.L5798:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L5802
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L5802
	cmpb	$95, 1(%rcx)
	jne	.L5802
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r9, %r9
	movq	%r9, -2480(%rbp)
	movq	%rax, -1232(%rbp)
	jne	.L5806
	testb	$-128, 66(%rsi)
	movq	%rsi, -2480(%rbp)
	je	.L5806
.L5810:
	movq	-2480(%rbp), %rbx
	movq	56(%rbx), %rcx
	testb	$-128, 66(%rcx)
	movq	%rcx, -2480(%rbp)
	jne	.L5810
.L5806:
	movq	-2480(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11691
	movq	-2480(%rbp), %r8
	movq	-1232(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-2480(%rbp), %r10
	movq	%rax, 8(%r10)
.L5812:
	testq	%r15, %r15
	je	.L5813
	movq	-1232(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L5814
	movq	%r15, 80(%rcx)
.L5814:
	movq	-1232(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L5815
	cmpb	$21, 16(%rbx)
	je	.L11692
.L5816:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L5818
	cmpq	$0, 32(%rax)
	je	.L5817
.L5818:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L11693
.L5819:
	xorl	%ecx, %ecx
.L5854:
	testq	%rcx, %rcx
	jne	.L5855
.L10255:
	movq	-1232(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1232(%rbp), %rdi
	movq	%rax, -2488(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2488(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11694
.L11135:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5857:
	movq	-1232(%rbp), %rsi
	movq	%rsi, 8(%r15)
.L5860:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11695
.L5862:
	movq	-1232(%rbp), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.L5893
	cmpb	$32, 16(%rdx)
	je	.L11696
.L5863:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5882
	movq	-2488(%rbp), %rax
	movq	56(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1248(%rbp)
	je	.L10633
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L5884
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5884:
	movq	-1248(%rbp), %rdx
	movq	-2488(%rbp), %r10
	movq	%r10, 56(%rdx)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rcx
	cmpq	decl_obstack+32(%rip), %rcx
	ja	.L11697
.L5886:
	movq	-2488(%rbp), %rsi
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r8)
	cmpb	$32, 16(%rsi)
	je	.L11698
.L10633:
	movq	32(%r15), %rax
.L5893:
	cmpb	$36, (%rax)
	je	.L11699
.L5907:
	movq	current_class_type(%rip), %rdx
	movq	-2488(%rbp), %r9
	movq	-1232(%rbp), %r11
	testq	%rdx, %rdx
	movq	%r9, 80(%r11)
	jne	.L5910
	cmpq	$0, current_function_decl(%rip)
	je	.L5909
.L5910:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L5908
.L5909:
	movq	-2488(%rbp), %rax
	movq	%r15, 72(%rax)
.L5815:
	movq	-2480(%rbp), %rsi
	movzbl	66(%rsi), %r10d
	andl	$15, %r10d
	cmpl	$2, %r10d
	je	.L11700
.L5813:
	movq	-1232(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11701
	movq	-1232(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1232(%rbp), %r9
	movq	%rax, (%r9)
.L11138:
	movzbl	16(%r12), %eax
	jmp	.L5802
.L11701:
	movq	%rax, (%rsi)
	jmp	.L11138
.L11700:
	movq	-1232(%rbp), %r8
	orb	$64, 18(%r8)
	movq	80(%r8), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L5813
	movq	-2480(%rbp), %r11
	movq	144(%rax), %r15
	movq	8(%r11), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L5813
.L5908:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11702
	cmpq	$0, 32(%rdx)
	jne	.L5815
	movq	-2488(%rbp), %r10
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r10)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5918
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2488(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1232(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L5920:
	movq	-2488(%rbp), %rax
	movq	current_class_type(%rip), %rdi
	movq	152(%rax), %rbx
	movq	%rdi, 64(%rax)
	movq	%rdi, 16(%rbx)
	jmp	.L5815
.L5918:
	movq	-2488(%rbp), %r9
	movq	%r15, 72(%r9)
	jmp	.L5920
.L11702:
	movq	-2488(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5913
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2488(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1232(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L5915:
	movq	current_function_decl(%rip), %rdi
	movq	-2488(%rbp), %rbx
	movq	%rdi, 64(%rbx)
	jmp	.L5815
.L5913:
	movq	-2488(%rbp), %r9
	movq	%r15, 72(%r9)
	jmp	.L5915
.L11699:
	cmpb	$95, 1(%rax)
	jne	.L5907
	movq	-2488(%rbp), %r8
	orb	$64, 53(%r8)
	jmp	.L5907
.L11698:
	cmpq	$0, 72(%rsi)
	jne	.L10633
	movq	8(%rsi), %r11
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r11, -1256(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5888
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-1248(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1248(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2488(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-1256(%rbp), %rdi
	movq	%rdi, 8(%rax)
	jmp	.L10633
.L5888:
	movq	-1248(%rbp), %r11
	movq	-2488(%rbp), %r8
	movq	%r11, 72(%r8)
	jmp	.L10633
.L11697:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5886
.L5882:
	movq	-2488(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2488(%rbp)
	jmp	.L10633
.L11696:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L5864
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L5865
.L5864:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5866
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10133
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L5867
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11703
.L5867:
	testq	%rcx, %rcx
	jne	.L10133
.L10134:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10133
.L5866:
	movq	40(%r15), %rcx
.L5865:
	testq	%rcx, %rcx
	je	.L5869
.L10133:
	cmpb	$32, 16(%rcx)
	je	.L5869
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5869
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L5877
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11136
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11704
.L11136:
	movq	%rax, %rcx
.L5869:
	movq	-1232(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L5863
	jmp	.L10633
.L11704:
	testl	%edx, %edx
	jg	.L11136
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L5869
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11136
.L5877:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L5869
.L11703:
	movl	$1, %r11d
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %r11d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5869
	testq	%rax, %rax
	je	.L10134
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L5867
.L11695:
	cmpb	$95, 1(%rax)
	jne	.L5862
	jmp	.L5893
.L11694:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5857
	jmp	.L11135
	.p2align 6,,7
.L5855:
	movq	80(%rcx), %rax
	movq	%rax, -2488(%rbp)
	jmp	.L5860
.L11693:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L5821
	movq	80(%rax), %rbx
.L5821:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5854
.L5853:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L5830
	cmpl	$32, %eax
	je	.L11705
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L5824:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5854
	jmp	.L5853
.L11705:
	movq	8(%rbx), %rsi
	movq	-1232(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10890
	movq	64(%rbx), %rbx
	jmp	.L5824
.L10890:
	movq	32(%rax), %rcx
	jmp	.L5854
.L5830:
	movq	-1232(%rbp), %rax
	movq	80(%rax), %rcx
	movq	56(%rcx), %rbx
	testq	%rbx, %rbx
	je	.L5819
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L5833
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L5834
.L5833:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5835
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10131
	movq	32(%rdi), %r8
	testq	%r8, %r8
	movq	%r8, -1240(%rbp)
	jne	.L5836
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11706
.L5836:
	testq	%rcx, %rcx
	jne	.L10131
.L10132:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10131
.L5835:
	movq	40(%rbx), %rcx
.L5834:
	testq	%rcx, %rcx
	je	.L10255
.L10131:
	cmpb	$32, 16(%rcx)
	je	.L5854
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5854
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L5846
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11134
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11707
.L11134:
	movq	%rax, %rcx
	jmp	.L5854
.L11707:
	testl	%edx, %edx
	jg	.L11134
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L5854
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11134
.L5846:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L5854
	jmp	.L11134
.L11706:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5854
	testq	%rax, %rax
	je	.L10132
	cmpb	$32, 16(%rax)
	cmovne	-1240(%rbp), %rcx
	jmp	.L5836
.L5817:
	movq	-1232(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1264(%rbp)
	je	.L11708
.L11137:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5895:
	movq	-1232(%rbp), %rdi
	movq	%rdi, 8(%r15)
	movq	-1264(%rbp), %rbx
	movq	56(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, -1272(%rbp)
	je	.L5898
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L5899
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5899:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1264(%rbp), %r10
	movq	-1272(%rbp), %rax
	leaq	8(%rdx), %r9
	cmpq	decl_obstack+32(%rip), %r9
	movq	%r10, 56(%rax)
	ja	.L11709
.L5901:
	movq	-1264(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11710
.L5898:
	movq	-1264(%rbp), %rbx
	movq	%rbx, -2488(%rbp)
	jmp	.L10633
.L11710:
	cmpq	$0, 72(%rbx)
	jne	.L5898
	movq	-1264(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %rcx
	movq	%rcx, -1280(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5903
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1272(%rbp), %r10
	cmpb	$1, 16(%r10)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1272(%rbp), %r9
	movq	32(%rbx), %rdx
	movq	-1272(%rbp), %rdi
	movl	$.LC35, %esi
	movq	32(%r9), %rcx
	movl	24(%rdi), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1264(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-1280(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L5898
.L5903:
	movq	-1272(%rbp), %rcx
	movq	-1264(%rbp), %rdx
	movq	%rcx, 72(%rdx)
	jmp	.L5898
.L11709:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5901
.L11708:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5895
	jmp	.L11137
	.p2align 6,,7
.L11692:
	cmpq	$0, class_binding_level(%rip)
	je	.L5816
	movq	144(%rbx), %rdi
	testb	$16, 3(%rdi)
	jne	.L5815
	jmp	.L5816
	.p2align 6,,7
.L11691:
	movq	8(%rcx), %rdx
	movq	-1232(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2480(%rbp), %rdx
	movq	%rax, 8(%rdx)
	jmp	.L5812
	.p2align 6,,7
.L5783:
	movq	-2472(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10628
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L5788
	testb	$8, 18(%r14)
	je	.L5788
	testb	$8, 18(%r13)
	jne	.L5788
	testb	$9, 53(%r13)
	jne	.L5788
	cmpq	%r13, current_function_decl(%rip)
	je	.L11711
.L5792:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5793
	cmpq	$0, 8(%rax)
	jne	.L11712
.L5793:
	movq	-2472(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11133:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2472(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L5788
.L11712:
	movq	-2472(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11133
.L11711:
	movq	-2472(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L5792
	.p2align 6,,7
.L11310:
	cmpq	$0, 64(%rcx)
	jne	.L5781
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L5781
.L11309:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2472(%rbp)
	call	error_with_decl
	jmp	.L5779
.L5772:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L5774
	.p2align 4,,7
.L5778:
	cmpq	%r14, 56(%rdi)
	je	.L5774
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L5778
.L5774:
	movq	%rdi, -2472(%rbp)
	jmp	.L5771
.L11308:
	movq	40(%r14), %rdi
	movq	%rdi, -2472(%rbp)
	jmp	.L5771
.L11307:
	movq	56(%r13), %r14
	jmp	.L5768
.L11306:
	testb	$32, 53(%r13)
	jne	.L5766
	jmp	.L5767
.L10624:
	movzbl	16(%r13), %edx
	jmp	.L5766
.L10613:
	movzbl	16(%r13), %edx
.L5513:
	cmpb	$32, %dl
	je	.L10915
.L5521:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L5659
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5659
	testb	$1, 53(%rax)
	jne	.L5660
	testb	$8, 18(%rax)
	je	.L5659
.L5660:
	andb	$8, %dl
	je	.L11713
	.p2align 4,,7
.L5659:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10621
	testb	$1, 53(%r13)
	je	.L10621
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L5663
	movq	48(%r14), %r15
	testq	%r15, %r15
	movq	%r15, %rdx
	jne	.L5664
.L5663:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5665
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10129
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L5666
	movq	144(%rdi), %rcx
	testb	$1, 3(%rcx)
	jne	.L11714
.L5666:
	testq	%rdx, %rdx
	jne	.L10129
.L10130:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10129
.L5665:
	movq	40(%r14), %rdx
.L5664:
	testq	%rdx, %rdx
	je	.L10254
.L10129:
	cmpb	$32, 16(%rdx)
	je	.L5668
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L5668
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L5676
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11130
	testl	%ebx, %ebx
	jle	.L11715
.L11130:
	movq	%rax, %rdx
.L5668:
	testq	%rdx, %rdx
	jne	.L10621
.L10254:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2464(%rbp)
.L5662:
	cmpq	%rax, -2464(%rbp)
	je	.L11716
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11717
.L11131:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L5707:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11718
.L5718:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L5720
	testq	%r12, %r12
	je	.L5721
	testb	$1, 53(%r13)
	jne	.L5721
	cmpb	$34, 16(%r12)
	je	.L11719
.L5721:
	movl	warn_shadow(%rip), %ecx
	testl	%ecx, %ecx
	je	.L5720
	testb	$1, 53(%r13)
	jne	.L5720
	movl	32(%r13), %edx
	testl	%edx, %edx
	je	.L5720
	testq	%rax, %rax
	jne	.L5720
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L5726
	cmpb	$34, 16(%r12)
	je	.L11720
.L5726:
	cmpq	$0, 56(%r14)
	je	.L5728
	movl	$.LC41, %edi
.L5727:
	testq	%rdi, %rdi
	jne	.L11132
	.p2align 4,,7
.L5720:
	testq	%r12, %r12
	je	.L10622
	movq	-2464(%rbp), %r10
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r10), %rdx
	call	tree_cons
	movq	-2464(%rbp), %r12
	movq	%rax, 16(%r12)
.L10622:
	movzbl	16(%r13), %edx
.L5705:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L5493
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L5493
	cmpb	$18, 16(%rcx)
	je	.L11721
.L5737:
	testb	$64, 46(%rcx)
	je	.L5493
.L5736:
	movq	-2464(%rbp), %r11
	movzwl	64(%r11), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%r11)
	je	.L11722
.L10623:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L5493:
	cmpb	$32, %dl
	je	.L11723
.L5739:
	movq	-2464(%rbp), %rdx
	cmpq	global_binding_level(%rip), %rdx
	movq	(%rdx), %rbx
	movq	%rbx, (%r13)
	movq	%r13, (%rdx)
	jne	.L5512
	testb	$4, 17(%r13)
	jne	.L5512
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5512
.L11723:
	testq	%r14, %r14
	je	.L5739
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5740
	cmpq	class_binding_level(%rip), %rax
	je	.L5741
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L5745
	cmpb	$32, 16(%rax)
	je	.L5743
.L5745:
	cmpq	$0, current_class_type(%rip)
	je	.L5740
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L5740
	cmpb	$32, 16(%rax)
	je	.L5743
.L5740:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5744
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5743
	cmpb	$-127, %dl
	je	.L11724
.L5744:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L5739
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11725
.L5751:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5755
	cmpq	class_binding_level(%rip), %rax
	je	.L5756
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L5760
	cmpb	$32, 16(%rax)
	je	.L5758
.L5760:
	cmpq	$0, current_class_type(%rip)
	je	.L5755
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L5755
	cmpb	$32, 16(%rax)
	je	.L5758
.L5755:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L5739
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5758
	cmpb	$-127, %dl
	jne	.L5739
	movq	$0, 8(%rbx)
	jmp	.L5739
.L5758:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L5739
.L5756:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5760
.L11725:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%cl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L5751
.L11724:
	movq	$0, 8(%r14)
	jmp	.L5744
.L5743:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L5744
.L5741:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5745
.L11722:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10623
.L11721:
	movq	8(%rcx), %r8
	testb	$64, 46(%r8)
	jne	.L5736
	jmp	.L5737
.L11132:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L5720
.L5728:
	testq	%r12, %r12
	je	.L5730
	movl	$.LC42, %edi
	jmp	.L5727
.L5730:
	testq	%r15, %r15
	movl	$.LC43, %ebx
	cmovne	%rbx, %rdi
	jmp	.L5727
.L11720:
	movl	$.LC40, %edi
	jmp	.L5727
.L11719:
	cmpb	$34, 16(%r13)
	je	.L5721
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L5722
	movq	56(%rax), %rax
.L5722:
	movzbl	66(%rax), %r9d
	andl	$15, %r9d
	decl	%r9d
	jne	.L5720
	movl	$.LC40, %edi
	jmp	.L11132
	.p2align 6,,7
.L11718:
	movzbl	53(%r13), %r8d
	andb	$9, %r8b
	decb	%r8b
	je	.L11726
.L5710:
	testq	%r12, %r12
	jne	.L5718
	testq	%r15, %r15
	jne	.L5718
	testb	$1, 53(%r13)
	je	.L5718
	testb	$8, 18(%r13)
	je	.L5718
	orb	$8, 18(%r14)
	jmp	.L5718
	.p2align 6,,7
.L11726:
	testq	%r15, %r15
	je	.L5710
	cmpb	$29, 16(%r13)
	jne	.L5710
	cmpb	$29, 16(%r15)
	jne	.L5710
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11727
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L5713
	movzbl	53(%r13), %r11d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r11b
	orb	%sil, %r11b
	movb	%r11b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L5714
	movq	88(%r15), %rax
.L5715:
	movq	136(%r15), %r8
	movq	72(%r15), %rdi
	movq	%rax, (%rdx)
	movq	80(%r15), %r10
	movzbl	17(%r13), %r9d
	movq	%r8, 136(%r13)
	movq	%rdi, 72(%r13)
	movq	%r10, 80(%r13)
	movzbl	17(%r15), %ebx
	movq	%r15, 96(%r13)
	andb	$127, %r9b
	shrb	$7, %bl
	movzbl	%bl, %edx
	movl	%edx, %ecx
	salb	$7, %cl
	orb	%cl, %r9b
	movb	%r9b, 17(%r13)
	movzbl	53(%r15), %ecx
.L5713:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L5716
	movzbl	53(%r13), %r11d
	salb	$4, %al
	andb	$-17, %r11b
	orb	%al, %r11b
	movb	%r11b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L5716:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L5710
	cmpq	$0, 88(%r15)
	je	.L5710
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L5710
	movq	%rdx, 8(%r13)
	jmp	.L5710
.L5714:
	xorl	%eax, %eax
	jmp	.L5715
	.p2align 6,,7
.L11727:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L5710
	.p2align 6,,7
.L11717:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5707
	jmp	.L11131
.L11716:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11728
.L5682:
	cmpq	$0, 40(%r14)
	jne	.L5683
	testb	$8, 18(%r13)
	je	.L5683
	orb	$8, 18(%r14)
.L5683:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11729
.L5685:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L5684:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5696
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5686
	testb	$1, 18(%rcx)
	je	.L5686
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L5686:
	testq	%rax, %rax
	je	.L5696
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5691
	testb	$8, 17(%rcx)
	je	.L5691
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L5691:
	testq	%rax, %rax
	je	.L5696
	cmpq	$0, 8(%rax)
	je	.L5696
	cmpb	$29, %dl
	je	.L11730
.L5699:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L5696:
	testb	$8, 18(%r14)
	je	.L5705
	cmpb	$32, %dl
	je	.L5705
	testb	$8, 18(%r13)
	jne	.L5705
	testb	$1, 53(%r13)
	jne	.L5705
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5701
	cmpq	$0, 8(%rax)
	jne	.L11731
.L5701:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11241:
	xorl	%eax, %eax
	call	warning
	jmp	.L10622
.L11731:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11241
.L11730:
	movq	8(%r13), %rdi
	movq	integer_type_node(%rip), %r10
	cmpq	%r10, 8(%rdi)
	jne	.L5699
	jmp	.L5696
	.p2align 6,,7
.L11729:
	cmpq	$0, -2440(%rbp)
	je	.L5685
	movq	-2440(%rbp), %rax
	cmpb	$32, 16(%rax)
	jne	.L5684
	jmp	.L5685
.L11728:
	testb	$8, 54(%r13)
	jne	.L5682
	andb	$-9, 18(%r13)
	jmp	.L5682
	.p2align 6,,7
.L10621:
	movq	global_binding_level(%rip), %rax
	jmp	.L5662
.L11715:
	testl	%ecx, %ecx
	jg	.L11130
	testl	%ebx, %ebx
	je	.L5668
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11130
	.p2align 6,,7
.L5676:
	movq	8(%rdx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rdx
	jmp	.L5668
.L11714:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L5668
	testq	%rax, %rax
	je	.L10130
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L5666
	.p2align 6,,7
.L11713:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L5659
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L5659
	.p2align 6,,7
.L10915:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L5523
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L5522
.L5523:
	movq	global_binding_level(%rip), %rsi
	movq	%r13, %r12
	cmpq	%rsi, current_binding_level(%rip)
	jne	.L10614
	movq	%r13, 80(%rdx)
.L10614:
	movzbl	16(%r13), %eax
.L5526:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L5650
	cmpq	$0, 72(%r12)
	je	.L11732
.L5650:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L5521
	cmpq	$0, 56(%rax)
	je	.L5521
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11733
.L11129:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5656:
	movq	%r12, 8(%r15)
	jmp	.L5521
.L11733:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5656
	jmp	.L11129
.L11732:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r11b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L5651
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L5650
.L5651:
	movq	%rbx, 72(%r13)
	jmp	.L5650
.L5522:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L5526
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L5526
	cmpb	$95, 1(%rcx)
	jne	.L5526
	movq	class_binding_level(%rip), %r10
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r10, %r10
	movq	%r10, -2448(%rbp)
	movq	%rax, -1176(%rbp)
	jne	.L5530
	testb	$-128, 66(%rsi)
	movq	%rsi, -2448(%rbp)
	je	.L5530
.L5534:
	movq	-2448(%rbp), %rdi
	movq	56(%rdi), %r8
	testb	$-128, 66(%r8)
	movq	%r8, -2448(%rbp)
	jne	.L5534
.L5530:
	movq	-2448(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11734
	movq	-2448(%rbp), %rbx
	movq	-1176(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	saveable_tree_cons
	movq	-2448(%rbp), %r11
	movq	%rax, 8(%r11)
.L5536:
	testq	%r15, %r15
	je	.L5537
	movq	-1176(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L5538
	movq	%r15, 80(%rcx)
.L5538:
	movq	-1176(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L5539
	cmpb	$21, 16(%rbx)
	je	.L11735
.L5540:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L5542
	cmpq	$0, 32(%rax)
	je	.L5541
.L5542:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11736
.L5543:
	xorl	%ecx, %ecx
.L5578:
	testq	%rcx, %rcx
	jne	.L5579
.L10253:
	movq	-1176(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1176(%rbp), %rdi
	movq	%rax, -2456(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2456(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11737
.L11125:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5581:
	movq	-1176(%rbp), %rcx
	movq	%rcx, 8(%r15)
.L5584:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11738
.L5586:
	movq	-1176(%rbp), %rdi
	movq	80(%rdi), %rdx
	testq	%rdx, %rdx
	je	.L5617
	cmpb	$32, 16(%rdx)
	je	.L11739
.L5587:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5606
	movq	-2456(%rbp), %rcx
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1192(%rbp)
	je	.L10618
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L5608
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5608:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2456(%rbp), %r9
	movq	-1192(%rbp), %r11
	leaq	8(%rdx), %rdi
	cmpq	decl_obstack+32(%rip), %rdi
	movq	%r9, 56(%r11)
	ja	.L11740
.L5610:
	movq	-2456(%rbp), %rsi
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r10)
	cmpb	$32, 16(%rsi)
	je	.L11741
.L10618:
	movq	32(%r15), %rax
.L5617:
	cmpb	$36, (%rax)
	je	.L11742
.L5631:
	movq	current_class_type(%rip), %rdx
	movq	-2456(%rbp), %r8
	movq	-1176(%rbp), %r10
	testq	%rdx, %rdx
	movq	%r8, 80(%r10)
	jne	.L5634
	cmpq	$0, current_function_decl(%rip)
	je	.L5633
.L5634:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L5632
.L5633:
	movq	-2456(%rbp), %rdx
	movq	%r15, 72(%rdx)
.L5539:
	movq	-2448(%rbp), %r9
	movzbl	66(%r9), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L11743
.L5537:
	movq	-1176(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11744
	movq	-1176(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1176(%rbp), %rdi
	movq	%rax, (%rdi)
.L11128:
	movzbl	16(%r12), %eax
	jmp	.L5526
.L11744:
	movq	%rax, (%rsi)
	jmp	.L11128
.L11743:
	movq	-1176(%rbp), %rax
	orb	$64, 18(%rax)
	movq	80(%rax), %rbx
	movq	current_class_type(%rip), %rax
	movq	%rbx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L5537
	movq	-2448(%rbp), %r10
	movq	144(%rax), %r15
	movq	8(%r10), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L5537
.L5632:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11745
	cmpq	$0, 32(%rdx)
	jne	.L5539
	movq	-2456(%rbp), %rsi
	movq	80(%rdx), %r9
	cmpb	$32, 16(%rsi)
	movq	72(%r9), %rbx
	movl	$136, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5642
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2456(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1176(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L5644:
	movq	-2456(%rbp), %rdx
	movq	current_class_type(%rip), %r11
	movq	152(%rdx), %r8
	movq	%r11, 64(%rdx)
	movq	%r11, 16(%r8)
	jmp	.L5539
.L5642:
	movq	-2456(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L5644
.L11745:
	movq	-2456(%rbp), %r9
	movq	112(%rax), %rbx
	cmpb	$32, 16(%r9)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5637
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2456(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1176(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L5639:
	movq	current_function_decl(%rip), %r11
	movq	-2456(%rbp), %r8
	movq	%r11, 64(%r8)
	jmp	.L5539
.L5637:
	movq	-2456(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L5639
.L11742:
	cmpb	$95, 1(%rax)
	jne	.L5631
	movq	-2456(%rbp), %rdi
	orb	$64, 53(%rdi)
	jmp	.L5631
.L11741:
	cmpq	$0, 72(%rsi)
	jne	.L10618
	movq	8(%rsi), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%rdx, -1200(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5612
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	-1192(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1192(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2456(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-1200(%rbp), %rbx
	movq	%rbx, 8(%rax)
	jmp	.L10618
.L5612:
	movq	-1192(%rbp), %r10
	movq	-2456(%rbp), %rdi
	movq	%r10, 72(%rdi)
	jmp	.L10618
.L11740:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5610
.L5606:
	movq	-2456(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2456(%rbp)
	jmp	.L10618
.L11739:
	movq	global_binding_level(%rip), %r11
	cmpq	%r11, current_binding_level(%rip)
	je	.L5588
	movq	48(%r15), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L5589
.L5588:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5590
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10127
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L5591
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11746
.L5591:
	testq	%rcx, %rcx
	jne	.L10127
.L10128:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10127
.L5590:
	movq	40(%r15), %rcx
.L5589:
	testq	%rcx, %rcx
	je	.L5593
.L10127:
	cmpb	$32, 16(%rcx)
	je	.L5593
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5593
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L5601
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11126
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L11747
.L11126:
	movq	%rax, %rcx
.L5593:
	movq	-1176(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L5587
	jmp	.L10618
.L11747:
	testl	%edx, %edx
	jg	.L11126
	movl	$1, %ebx
	testl	%ebx, %ebx
	je	.L5593
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11126
.L5601:
	movq	8(%rcx), %rsi
	cmpq	error_mark_node(%rip), %rsi
	cmove	%rsi, %rcx
	jmp	.L5593
.L11746:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5593
	testq	%rax, %rax
	je	.L10128
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L5591
.L11738:
	cmpb	$95, 1(%rax)
	jne	.L5586
	jmp	.L5617
.L11737:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5581
	jmp	.L11125
	.p2align 6,,7
.L5579:
	movq	80(%rcx), %r8
	movq	%r8, -2456(%rbp)
	jmp	.L5584
.L11736:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L5545
	movq	80(%rax), %rbx
.L5545:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5578
.L5577:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L5554
	cmpl	$32, %eax
	je	.L11748
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L5548:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5578
	jmp	.L5577
.L11748:
	movq	8(%rbx), %rsi
	movq	-1176(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %r10
	movq	72(%r10), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10889
	movq	64(%rbx), %rbx
	jmp	.L5548
.L10889:
	movq	32(%rax), %rcx
	jmp	.L5578
.L5554:
	movq	-1176(%rbp), %rax
	movq	80(%rax), %r8
	movq	56(%r8), %rbx
	testq	%rbx, %rbx
	je	.L5543
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L5557
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L5558
.L5557:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5559
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10125
	movq	32(%rdi), %r9
	testq	%r9, %r9
	movq	%r9, -1184(%rbp)
	jne	.L5560
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11749
.L5560:
	testq	%rcx, %rcx
	jne	.L10125
.L10126:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10125
.L5559:
	movq	40(%rbx), %rcx
.L5558:
	testq	%rcx, %rcx
	je	.L10253
.L10125:
	cmpb	$32, 16(%rcx)
	je	.L5578
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5578
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L5570
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11124
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L11750
.L11124:
	movq	%rax, %rcx
	jmp	.L5578
.L11750:
	testl	%edx, %edx
	jg	.L11124
	movl	$1, %esi
	testl	%esi, %esi
	je	.L5578
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11124
.L5570:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L5578
	jmp	.L11124
.L11749:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5578
	testq	%rax, %rax
	je	.L10126
	cmpb	$32, 16(%rax)
	cmovne	-1184(%rbp), %rcx
	jmp	.L5560
.L5541:
	movq	-1176(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1208(%rbp)
	je	.L11751
.L11127:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5619:
	movq	-1176(%rbp), %rbx
	movq	%rbx, 8(%r15)
	movq	-1208(%rbp), %rdx
	movq	56(%rdx), %rax
	testq	%rax, %rax
	movq	%rax, -1216(%rbp)
	je	.L5622
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L5623
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5623:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1208(%rbp), %r11
	movq	-1216(%rbp), %rax
	leaq	8(%rdx), %r8
	cmpq	decl_obstack+32(%rip), %r8
	movq	%r11, 56(%rax)
	ja	.L11752
.L5625:
	movq	-1208(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11753
.L5622:
	movq	-1208(%rbp), %rbx
	movq	%rbx, -2456(%rbp)
	jmp	.L10618
.L11753:
	cmpq	$0, 72(%rbx)
	jne	.L5622
	movq	-1208(%rbp), %r9
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r9), %rcx
	movq	%rcx, -1224(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5627
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1216(%rbp), %rsi
	cmpb	$1, 16(%rsi)
	movl	$138, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-1216(%rbp), %r8
	movq	32(%rbx), %rdx
	movq	-1216(%rbp), %r11
	movl	$.LC35, %esi
	movq	32(%r8), %rcx
	movl	24(%r11), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1208(%rbp), %r10
	movq	%rax, 72(%r10)
	movq	-1224(%rbp), %rdi
	movq	%rdi, 8(%rax)
	jmp	.L5622
.L5627:
	movq	-1216(%rbp), %rcx
	movq	-1208(%rbp), %r9
	movq	%rcx, 72(%r9)
	jmp	.L5622
.L11752:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5625
.L11751:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5619
	jmp	.L11127
	.p2align 6,,7
.L11735:
	cmpq	$0, class_binding_level(%rip)
	je	.L5540
	movq	144(%rbx), %r9
	testb	$16, 3(%r9)
	jne	.L5539
	jmp	.L5540
	.p2align 6,,7
.L11734:
	movq	8(%rcx), %rdx
	movq	-1176(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2448(%rbp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.L5536
	.p2align 6,,7
.L5507:
	movq	-2440(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10613
	movl	flag_traditional(%rip), %r15d
	testl	%r15d, %r15d
	jne	.L5512
	testb	$8, 18(%r14)
	je	.L5512
	testb	$8, 18(%r13)
	jne	.L5512
	testb	$9, 53(%r13)
	jne	.L5512
	cmpq	%r13, current_function_decl(%rip)
	je	.L11754
.L5516:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5517
	cmpq	$0, 8(%rax)
	jne	.L11755
.L5517:
	movq	-2440(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11123:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2440(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L5512
.L11755:
	movq	-2440(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11123
.L11754:
	movq	-2440(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L5516
	.p2align 6,,7
.L11305:
	cmpq	$0, 64(%rcx)
	jne	.L5505
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L5505
.L11304:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2440(%rbp)
	call	error_with_decl
	jmp	.L5503
.L5496:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L5498
	.p2align 4,,7
.L5502:
	cmpq	%r14, 56(%rdi)
	je	.L5498
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L5502
.L5498:
	movq	%rdi, -2440(%rbp)
	jmp	.L5495
.L11303:
	movq	40(%r14), %rbx
	movq	%rbx, -2440(%rbp)
	jmp	.L5495
.L11302:
	movq	56(%r13), %r14
	jmp	.L5492
.L11301:
	testb	$32, 53(%r13)
	jne	.L5490
	jmp	.L5491
.L10609:
	movzbl	16(%r13), %edx
	jmp	.L5490
.L10598:
	movzbl	16(%r13), %edx
.L5237:
	cmpb	$32, %dl
	je	.L10914
.L5245:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L5383
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5383
	testb	$1, 53(%rax)
	jne	.L5384
	testb	$8, 18(%rax)
	je	.L5383
.L5384:
	andb	$8, %dl
	je	.L11756
	.p2align 4,,7
.L5383:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10606
	testb	$1, 53(%r13)
	je	.L10606
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L5387
	movq	48(%r14), %r10
	testq	%r10, %r10
	movq	%r10, %rdx
	jne	.L5388
.L5387:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5389
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10123
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L5390
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11757
.L5390:
	testq	%rdx, %rdx
	jne	.L10123
.L10124:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10123
.L5389:
	movq	40(%r14), %rdx
.L5388:
	testq	%rdx, %rdx
	je	.L10252
.L10123:
	cmpb	$32, 16(%rdx)
	je	.L5392
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L5392
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L5400
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11120
	testl	%ebx, %ebx
	jle	.L11758
.L11120:
	movq	%rax, %rdx
.L5392:
	testq	%rdx, %rdx
	jne	.L10606
.L10252:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2432(%rbp)
.L5386:
	cmpq	%rax, -2432(%rbp)
	je	.L11759
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11760
.L11121:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L5431:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11761
.L5442:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L5444
	testq	%r12, %r12
	je	.L5445
	testb	$1, 53(%r13)
	jne	.L5445
	cmpb	$34, 16(%r12)
	je	.L11762
.L5445:
	movl	warn_shadow(%rip), %r10d
	testl	%r10d, %r10d
	je	.L5444
	testb	$1, 53(%r13)
	jne	.L5444
	movl	32(%r13), %r8d
	testl	%r8d, %r8d
	je	.L5444
	testq	%rax, %rax
	jne	.L5444
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L5450
	cmpb	$34, 16(%r12)
	je	.L11763
.L5450:
	cmpq	$0, 56(%r14)
	je	.L5452
	movl	$.LC41, %edi
.L5451:
	testq	%rdi, %rdi
	jne	.L11122
	.p2align 4,,7
.L5444:
	testq	%r12, %r12
	je	.L10607
	movq	-2432(%rbp), %r11
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r11), %rdx
	call	tree_cons
	movq	-2432(%rbp), %r12
	movq	%rax, 16(%r12)
.L10607:
	movzbl	16(%r13), %edx
.L5429:
	leal	-128(%rdx), %ebx
	cmpb	$1, %bl
	jbe	.L5217
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L5217
	cmpb	$18, 16(%rcx)
	je	.L11764
.L5461:
	testb	$64, 46(%rcx)
	je	.L5217
.L5460:
	movq	-2432(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11765
.L10608:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L5217:
	cmpb	$32, %dl
	je	.L11766
.L5463:
	movq	-2432(%rbp), %rdi
	cmpq	global_binding_level(%rip), %rdi
	movq	(%rdi), %rcx
	movq	%rcx, (%r13)
	movq	%r13, (%rdi)
	jne	.L5236
	testb	$4, 17(%r13)
	jne	.L5236
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5236
.L11766:
	testq	%r14, %r14
	je	.L5463
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5464
	cmpq	class_binding_level(%rip), %rax
	je	.L5465
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L5469
	cmpb	$32, 16(%rax)
	je	.L5467
.L5469:
	cmpq	$0, current_class_type(%rip)
	je	.L5464
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L5464
	cmpb	$32, 16(%rax)
	je	.L5467
.L5464:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5468
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5467
	cmpb	$-127, %dl
	je	.L11767
.L5468:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L5463
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11768
.L5475:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5479
	cmpq	class_binding_level(%rip), %rax
	je	.L5480
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L5484
	cmpb	$32, 16(%rax)
	je	.L5482
.L5484:
	cmpq	$0, current_class_type(%rip)
	je	.L5479
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L5479
	cmpb	$32, 16(%rax)
	je	.L5482
.L5479:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L5463
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5482
	cmpb	$-127, %dl
	jne	.L5463
	movq	$0, 8(%rbx)
	jmp	.L5463
.L5482:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L5463
.L5480:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5484
.L11768:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%r8b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L5475
.L11767:
	movq	$0, 8(%r14)
	jmp	.L5468
.L5467:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L5468
.L5465:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5469
.L11765:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10608
.L11764:
	movq	8(%rcx), %r9
	testb	$64, 46(%r9)
	jne	.L5460
	jmp	.L5461
.L11122:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L5444
.L5452:
	testq	%r12, %r12
	je	.L5454
	movl	$.LC42, %edi
	jmp	.L5451
.L5454:
	testq	%r15, %r15
	movl	$.LC43, %ecx
	cmovne	%rcx, %rdi
	jmp	.L5451
.L11763:
	movl	$.LC40, %edi
	jmp	.L5451
.L11762:
	cmpb	$34, 16(%r13)
	je	.L5445
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L5446
	movq	56(%rax), %rax
.L5446:
	movzbl	66(%rax), %edi
	andl	$15, %edi
	decl	%edi
	jne	.L5444
	movl	$.LC40, %edi
	jmp	.L11122
	.p2align 6,,7
.L11761:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11769
.L5434:
	testq	%r12, %r12
	jne	.L5442
	testq	%r15, %r15
	jne	.L5442
	testb	$1, 53(%r13)
	je	.L5442
	testb	$8, 18(%r13)
	je	.L5442
	orb	$8, 18(%r14)
	jmp	.L5442
	.p2align 6,,7
.L11769:
	testq	%r15, %r15
	je	.L5434
	cmpb	$29, 16(%r13)
	jne	.L5434
	cmpb	$29, 16(%r15)
	jne	.L5434
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11770
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L5437
	movzbl	53(%r13), %r9d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r9b
	orb	%sil, %r9b
	movb	%r9b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L5438
	movq	88(%r15), %rax
.L5439:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %r11
	movq	136(%r15), %rdx
	movzbl	17(%r13), %edi
	movq	%rbx, 72(%r13)
	movq	%r11, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %ecx
	movq	%r15, 96(%r13)
	andb	$127, %dil
	shrb	$7, %cl
	movzbl	%cl, %r8d
	movl	%r8d, %r10d
	salb	$7, %r10b
	orb	%r10b, %dil
	movb	%dil, 17(%r13)
	movzbl	53(%r15), %ecx
.L5437:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L5440
	movzbl	53(%r13), %r9d
	salb	$4, %al
	andb	$-17, %r9b
	orb	%al, %r9b
	movb	%r9b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L5440:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L5434
	cmpq	$0, 88(%r15)
	je	.L5434
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L5434
	movq	%rdx, 8(%r13)
	jmp	.L5434
.L5438:
	xorl	%eax, %eax
	jmp	.L5439
	.p2align 6,,7
.L11770:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L5434
	.p2align 6,,7
.L11760:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5431
	jmp	.L11121
.L11759:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11771
.L5406:
	cmpq	$0, 40(%r14)
	jne	.L5407
	testb	$8, 18(%r13)
	je	.L5407
	orb	$8, 18(%r14)
.L5407:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11772
.L5409:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L5408:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5420
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5410
	testb	$1, 18(%rcx)
	je	.L5410
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L5410:
	testq	%rax, %rax
	je	.L5420
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5415
	testb	$8, 17(%rcx)
	je	.L5415
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L5415:
	testq	%rax, %rax
	je	.L5420
	cmpq	$0, 8(%rax)
	je	.L5420
	cmpb	$29, %dl
	je	.L11773
.L5423:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L5420:
	testb	$8, 18(%r14)
	je	.L5429
	cmpb	$32, %dl
	je	.L5429
	testb	$8, 18(%r13)
	jne	.L5429
	testb	$1, 53(%r13)
	jne	.L5429
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5425
	cmpq	$0, 8(%rax)
	jne	.L11774
.L5425:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11240:
	xorl	%eax, %eax
	call	warning
	jmp	.L10607
.L11774:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11240
.L11773:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %r11
	cmpq	%r11, 8(%rbx)
	jne	.L5423
	jmp	.L5420
	.p2align 6,,7
.L11772:
	cmpq	$0, -2408(%rbp)
	je	.L5409
	movq	-2408(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L5408
	jmp	.L5409
.L11771:
	testb	$8, 54(%r13)
	jne	.L5406
	andb	$-9, 18(%r13)
	jmp	.L5406
	.p2align 6,,7
.L10606:
	movq	global_binding_level(%rip), %rax
	jmp	.L5386
.L11758:
	testl	%ecx, %ecx
	jg	.L11120
	testl	%ebx, %ebx
	je	.L5392
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11120
	.p2align 6,,7
.L5400:
	movq	8(%rdx), %r15
	cmpq	error_mark_node(%rip), %r15
	cmove	%r15, %rdx
	jmp	.L5392
.L11757:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L5392
	testq	%rax, %rax
	je	.L10124
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L5390
	.p2align 6,,7
.L11756:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L5383
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L5383
	.p2align 6,,7
.L10914:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L5247
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L5246
.L5247:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10599
	movq	%r13, 80(%rdx)
.L10599:
	movzbl	16(%r13), %eax
.L5250:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L5374
	cmpq	$0, 72(%r12)
	je	.L11775
.L5374:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L5245
	cmpq	$0, 56(%rax)
	je	.L5245
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11776
.L11119:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5380:
	movq	%r12, 8(%r15)
	jmp	.L5245
.L11776:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5380
	jmp	.L11119
.L11775:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%al
	movq	8(%r13), %r15
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L5375
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L5374
.L5375:
	movq	%rbx, 72(%r13)
	jmp	.L5374
.L5246:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L5250
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L5250
	cmpb	$95, 1(%rcx)
	jne	.L5250
	movq	class_binding_level(%rip), %r11
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r11, %r11
	movq	%r11, -2416(%rbp)
	movq	%rax, -1120(%rbp)
	jne	.L5254
	testb	$-128, 66(%rsi)
	movq	%rsi, -2416(%rbp)
	je	.L5254
.L5258:
	movq	-2416(%rbp), %rcx
	movq	56(%rcx), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2416(%rbp)
	jne	.L5258
.L5254:
	movq	-2416(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11777
	movq	-2416(%rbp), %r10
	movq	-1120(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r10), %rdx
	call	saveable_tree_cons
	movq	-2416(%rbp), %rbx
	movq	%rax, 8(%rbx)
.L5260:
	testq	%r15, %r15
	je	.L5261
	movq	-1120(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L5262
	movq	%r15, 80(%rcx)
.L5262:
	movq	-1120(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L5263
	cmpb	$21, 16(%rbx)
	je	.L11778
.L5264:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L5266
	cmpq	$0, 32(%rax)
	je	.L5265
.L5266:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11779
.L5267:
	xorl	%ecx, %ecx
.L5302:
	testq	%rcx, %rcx
	jne	.L5303
.L10251:
	movq	-1120(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1120(%rbp), %rdi
	movq	%rax, -2424(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2424(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11780
.L11115:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5305:
	movq	-1120(%rbp), %rcx
	movq	%rcx, 8(%r15)
.L5308:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11781
.L5310:
	movq	-1120(%rbp), %r9
	movq	80(%r9), %rdx
	testq	%rdx, %rdx
	je	.L5341
	cmpb	$32, 16(%rdx)
	je	.L11782
.L5311:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5330
	movq	-2424(%rbp), %rcx
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1136(%rbp)
	je	.L10603
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L5332
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5332:
	movq	-2424(%rbp), %rdx
	movq	-1136(%rbp), %r10
	movq	%rdx, 56(%r10)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %r9
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L11783
.L5334:
	movq	-2424(%rbp), %rsi
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r8)
	cmpb	$32, 16(%rsi)
	je	.L11784
.L10603:
	movq	32(%r15), %rax
.L5341:
	cmpb	$36, (%rax)
	je	.L11785
.L5355:
	movq	-2424(%rbp), %rdx
	movq	-1120(%rbp), %r11
	movq	%rdx, 80(%r11)
	movq	current_class_type(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L5358
	cmpq	$0, current_function_decl(%rip)
	je	.L5357
.L5358:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L5356
.L5357:
	movq	-2424(%rbp), %rax
	movq	%r15, 72(%rax)
.L5263:
	movq	-2416(%rbp), %rsi
	movzbl	66(%rsi), %r9d
	andl	$15, %r9d
	cmpl	$2, %r9d
	je	.L11786
.L5261:
	movq	-1120(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11787
	movq	-1120(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1120(%rbp), %rbx
	movq	%rax, (%rbx)
.L11118:
	movzbl	16(%r12), %eax
	jmp	.L5250
.L11787:
	movq	%rax, (%rsi)
	jmp	.L11118
.L11786:
	movq	-1120(%rbp), %r8
	orb	$64, 18(%r8)
	movq	80(%r8), %r10
	movq	current_class_type(%rip), %rax
	movq	%r10, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L5261
	movq	-2416(%rbp), %r11
	movq	144(%rax), %r15
	movq	8(%r11), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L5261
.L5356:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11788
	cmpq	$0, 32(%rdx)
	jne	.L5263
	movq	-2424(%rbp), %r9
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r9)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5366
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2424(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1120(%rbp), %r10
	movq	%r10, 8(%rax)
.L5368:
	movq	-2424(%rbp), %rax
	movq	current_class_type(%rip), %rdi
	movq	152(%rax), %rbx
	movq	%rdi, 64(%rax)
	movq	%rdi, 16(%rbx)
	jmp	.L5263
.L5366:
	movq	-2424(%rbp), %rdx
	movq	%r15, 72(%rdx)
	jmp	.L5368
.L11788:
	movq	-2424(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5361
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2424(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1120(%rbp), %r10
	movq	%r10, 8(%rax)
.L5363:
	movq	current_function_decl(%rip), %rdi
	movq	-2424(%rbp), %rbx
	movq	%rdi, 64(%rbx)
	jmp	.L5263
.L5361:
	movq	-2424(%rbp), %rdx
	movq	%r15, 72(%rdx)
	jmp	.L5363
.L11785:
	cmpb	$95, 1(%rax)
	jne	.L5355
	movq	-2424(%rbp), %r8
	orb	$64, 53(%r8)
	jmp	.L5355
.L11784:
	cmpq	$0, 72(%rsi)
	jne	.L10603
	movq	8(%rsi), %rdi
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	%rdi, -1144(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5336
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	-1136(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1136(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2424(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-1144(%rbp), %r11
	movq	%r11, 8(%rax)
	jmp	.L10603
.L5336:
	movq	-1136(%rbp), %r8
	movq	-2424(%rbp), %rdx
	movq	%r8, 72(%rdx)
	jmp	.L10603
.L11783:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5334
.L5330:
	movq	-2424(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2424(%rbp)
	jmp	.L10603
.L11782:
	movq	global_binding_level(%rip), %r10
	cmpq	%r10, current_binding_level(%rip)
	je	.L5312
	movq	48(%r15), %rdx
	testq	%rdx, %rdx
	movq	%rdx, %rcx
	jne	.L5313
.L5312:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5314
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10121
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L5315
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11789
.L5315:
	testq	%rcx, %rcx
	jne	.L10121
.L10122:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10121
.L5314:
	movq	40(%r15), %rcx
.L5313:
	testq	%rcx, %rcx
	je	.L5317
.L10121:
	cmpb	$32, 16(%rcx)
	je	.L5317
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5317
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L5325
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11116
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11790
.L11116:
	movq	%rax, %rcx
.L5317:
	movq	-1120(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L5311
	jmp	.L10603
.L11790:
	testl	%edx, %edx
	jg	.L11116
	movl	$1, %r11d
	testl	%r11d, %r11d
	je	.L5317
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11116
.L5325:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L5317
.L11789:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5317
	testq	%rax, %rax
	je	.L10122
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L5315
.L11781:
	cmpb	$95, 1(%rax)
	jne	.L5310
	jmp	.L5341
.L11780:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5305
	jmp	.L11115
	.p2align 6,,7
.L5303:
	movq	80(%rcx), %rsi
	movq	%rsi, -2424(%rbp)
	jmp	.L5308
.L11779:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L5269
	movq	80(%rax), %rbx
.L5269:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5302
.L5301:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L5278
	cmpl	$32, %eax
	je	.L11791
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L5272:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5302
	jmp	.L5301
.L11791:
	movq	8(%rbx), %r11
	movq	-1120(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r11), %r8
	movq	72(%r8), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10888
	movq	64(%rbx), %rbx
	jmp	.L5272
.L10888:
	movq	32(%rax), %rcx
	jmp	.L5302
.L5278:
	movq	-1120(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L5267
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L5281
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L5282
.L5281:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5283
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10119
	movq	32(%rdi), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1128(%rbp)
	jne	.L5284
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11792
.L5284:
	testq	%rcx, %rcx
	jne	.L10119
.L10120:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10119
.L5283:
	movq	40(%rbx), %rcx
.L5282:
	testq	%rcx, %rcx
	je	.L10251
.L10119:
	cmpb	$32, 16(%rcx)
	je	.L5302
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5302
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L5294
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11114
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11793
.L11114:
	movq	%rax, %rcx
	jmp	.L5302
.L11793:
	testl	%edx, %edx
	jg	.L11114
	movl	$1, %r11d
	testl	%r11d, %r11d
	je	.L5302
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11114
.L5294:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L5302
	jmp	.L11114
.L11792:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5302
	testq	%rax, %rax
	je	.L10120
	cmpb	$32, 16(%rax)
	cmovne	-1128(%rbp), %rcx
	jmp	.L5284
.L5265:
	movq	-1120(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1152(%rbp)
	je	.L11794
.L11117:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5343:
	movq	-1120(%rbp), %r11
	movq	%r11, 8(%r15)
	movq	-1152(%rbp), %rdi
	movq	56(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, -1160(%rbp)
	je	.L5346
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L5347
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5347:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1152(%rbp), %r9
	movq	-1160(%rbp), %rax
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	movq	%r9, 56(%rax)
	ja	.L11795
.L5349:
	movq	-1152(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11796
.L5346:
	movq	-1152(%rbp), %rbx
	movq	%rbx, -2424(%rbp)
	jmp	.L10603
.L11796:
	cmpq	$0, 72(%rbx)
	jne	.L5346
	movq	-1152(%rbp), %r10
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r10), %rcx
	movq	%rcx, -1168(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5351
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1160(%rbp), %r9
	cmpb	$1, 16(%r9)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1160(%rbp), %r11
	movq	32(%rbx), %rdx
	movq	-1160(%rbp), %rdi
	movl	$.LC35, %esi
	movq	32(%r11), %rcx
	movl	24(%rdi), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1152(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-1168(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L5346
.L5351:
	movq	-1160(%rbp), %rcx
	movq	-1152(%rbp), %r10
	movq	%rcx, 72(%r10)
	jmp	.L5346
.L11795:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5349
.L11794:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5343
	jmp	.L11117
	.p2align 6,,7
.L11778:
	cmpq	$0, class_binding_level(%rip)
	je	.L5264
	movq	144(%rbx), %rdi
	testb	$16, 3(%rdi)
	jne	.L5263
	jmp	.L5264
	.p2align 6,,7
.L11777:
	movq	-1120(%rbp), %rsi
	movq	8(%rcx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2416(%rbp), %r9
	movq	%rax, 8(%r9)
	jmp	.L5260
	.p2align 6,,7
.L5231:
	movq	-2408(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10598
	movl	flag_traditional(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L5236
	testb	$8, 18(%r14)
	je	.L5236
	testb	$8, 18(%r13)
	jne	.L5236
	testb	$9, 53(%r13)
	jne	.L5236
	cmpq	%r13, current_function_decl(%rip)
	je	.L11797
.L5240:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5241
	cmpq	$0, 8(%rax)
	jne	.L11798
.L5241:
	movq	-2408(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11113:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2408(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L5236
.L11798:
	movq	-2408(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11113
.L11797:
	movq	-2408(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L5240
	.p2align 6,,7
.L11300:
	cmpq	$0, 64(%rcx)
	jne	.L5229
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L5229
.L11299:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2408(%rbp)
	call	error_with_decl
	jmp	.L5227
.L5220:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L5222
	.p2align 4,,7
.L5226:
	cmpq	%r14, 56(%rdi)
	je	.L5222
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L5226
.L5222:
	movq	%rdi, -2408(%rbp)
	jmp	.L5219
.L11298:
	movq	40(%r14), %rdi
	movq	%rdi, -2408(%rbp)
	jmp	.L5219
.L11297:
	movq	56(%r13), %r14
	jmp	.L5216
.L11296:
	testb	$32, 53(%r13)
	jne	.L5214
	jmp	.L5215
.L10594:
	movzbl	16(%r13), %edx
	jmp	.L5214
.L10583:
	movzbl	16(%r13), %edx
.L4961:
	cmpb	$32, %dl
	je	.L10913
.L4969:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L5107
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5107
	testb	$1, 53(%rax)
	jne	.L5108
	testb	$8, 18(%rax)
	je	.L5107
.L5108:
	andb	$8, %dl
	je	.L11799
	.p2align 4,,7
.L5107:
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	je	.L10591
	testb	$1, 53(%r13)
	je	.L10591
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %r12
	cmpq	%r12, current_binding_level(%rip)
	je	.L5111
	movq	48(%r14), %r11
	testq	%r11, %r11
	movq	%r11, %rdx
	jne	.L5112
.L5111:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5113
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10117
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L5114
	movq	144(%rdi), %r15
	testb	$1, 3(%r15)
	jne	.L11800
.L5114:
	testq	%rdx, %rdx
	jne	.L10117
.L10118:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10117
.L5113:
	movq	40(%r14), %rdx
.L5112:
	testq	%rdx, %rdx
	je	.L10250
.L10117:
	cmpb	$32, 16(%rdx)
	je	.L5116
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L5116
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L5124
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11110
	testl	%ebx, %ebx
	jle	.L11801
.L11110:
	movq	%rax, %rdx
.L5116:
	testq	%rdx, %rdx
	jne	.L10591
.L10250:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2400(%rbp)
.L5110:
	cmpq	%rax, -2400(%rbp)
	je	.L11802
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11803
.L11111:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L5155:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11804
.L5166:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L5168
	testq	%r12, %r12
	je	.L5169
	testb	$1, 53(%r13)
	jne	.L5169
	cmpb	$34, 16(%r12)
	je	.L11805
.L5169:
	movl	warn_shadow(%rip), %r11d
	testl	%r11d, %r11d
	je	.L5168
	testb	$1, 53(%r13)
	jne	.L5168
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L5168
	testq	%rax, %rax
	jne	.L5168
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L5174
	cmpb	$34, 16(%r12)
	je	.L11806
.L5174:
	cmpq	$0, 56(%r14)
	je	.L5176
	movl	$.LC41, %edi
.L5175:
	testq	%rdi, %rdi
	jne	.L11112
	.p2align 4,,7
.L5168:
	testq	%r12, %r12
	je	.L10592
	movq	-2400(%rbp), %rbx
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%rbx), %rdx
	call	tree_cons
	movq	-2400(%rbp), %r12
	movq	%rax, 16(%r12)
.L10592:
	movzbl	16(%r13), %edx
.L5153:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L4941
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L4941
	cmpb	$18, 16(%rcx)
	je	.L11807
.L5185:
	testb	$64, 46(%rcx)
	je	.L4941
.L5184:
	movq	-2400(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11808
.L10593:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L4941:
	cmpb	$32, %dl
	je	.L11809
.L5187:
	movq	-2400(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	movq	(%rcx), %r9
	movq	%r9, (%r13)
	movq	%r13, (%rcx)
	jne	.L4960
	testb	$4, 17(%r13)
	jne	.L4960
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4960
.L11809:
	testq	%r14, %r14
	je	.L5187
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5188
	cmpq	class_binding_level(%rip), %rax
	je	.L5189
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L5193
	cmpb	$32, 16(%rax)
	je	.L5191
.L5193:
	cmpq	$0, current_class_type(%rip)
	je	.L5188
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L5188
	cmpb	$32, 16(%rax)
	je	.L5191
.L5188:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L5192
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5191
	cmpb	$-127, %dl
	je	.L11810
.L5192:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L5187
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11811
.L5199:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L5203
	cmpq	class_binding_level(%rip), %rax
	je	.L5204
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L5208
	cmpb	$32, 16(%rax)
	je	.L5206
.L5208:
	cmpq	$0, current_class_type(%rip)
	je	.L5203
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L5203
	cmpb	$32, 16(%rax)
	je	.L5206
.L5203:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L5187
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L5206
	cmpb	$-127, %dl
	jne	.L5187
	movq	$0, 8(%rbx)
	jmp	.L5187
.L5206:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L5187
.L5204:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5208
.L11811:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%r11b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	cmpb	$1, 16(%r14)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L5199
.L11810:
	movq	$0, 8(%r14)
	jmp	.L5192
.L5191:
	movq	8(%rax), %r8
	movq	%r8, 8(%r14)
	jmp	.L5192
.L5189:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L5193
.L11808:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10593
.L11807:
	movq	8(%rcx), %r10
	testb	$64, 46(%r10)
	jne	.L5184
	jmp	.L5185
.L11112:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L5168
.L5176:
	testq	%r12, %r12
	je	.L5178
	movl	$.LC42, %edi
	jmp	.L5175
.L5178:
	testq	%r15, %r15
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L5175
.L11806:
	movl	$.LC40, %edi
	jmp	.L5175
.L11805:
	cmpb	$34, 16(%r13)
	je	.L5169
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L5170
	movq	56(%rax), %rax
.L5170:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L5168
	movl	$.LC40, %edi
	jmp	.L11112
	.p2align 6,,7
.L11804:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11812
.L5158:
	testq	%r12, %r12
	jne	.L5166
	testq	%r15, %r15
	jne	.L5166
	testb	$1, 53(%r13)
	je	.L5166
	testb	$8, 18(%r13)
	je	.L5166
	orb	$8, 18(%r14)
	jmp	.L5166
	.p2align 6,,7
.L11812:
	testq	%r15, %r15
	je	.L5158
	cmpb	$29, 16(%r13)
	jne	.L5158
	cmpb	$29, 16(%r15)
	jne	.L5158
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11813
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L5161
	movzbl	53(%r13), %r10d
	leal	0(,%rax,8), %r8d
	leaq	88(%r13), %rdx
	andb	$-9, %r10b
	orb	%r8b, %r10b
	movb	%r10b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L5162
	movq	88(%r15), %rax
.L5163:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %rdi
	movq	136(%r15), %rdx
	movzbl	17(%r13), %esi
	movq	%rbx, 72(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %r9d
	movq	%r15, 96(%r13)
	andb	$127, %sil
	shrb	$7, %r9b
	movzbl	%r9b, %ecx
	movl	%ecx, %r11d
	salb	$7, %r11b
	orb	%r11b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r15), %ecx
.L5161:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L5164
	movzbl	53(%r13), %r10d
	salb	$4, %al
	andb	$-17, %r10b
	orb	%al, %r10b
	movb	%r10b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L5164:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L5158
	cmpq	$0, 88(%r15)
	je	.L5158
	movq	8(%r13), %r8
	cmpq	$0, 24(%r8)
	jne	.L5158
	movq	%rdx, 8(%r13)
	jmp	.L5158
.L5162:
	xorl	%eax, %eax
	jmp	.L5163
	.p2align 6,,7
.L11813:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L5158
	.p2align 6,,7
.L11803:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5155
	jmp	.L11111
.L11802:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11814
.L5130:
	cmpq	$0, 40(%r14)
	jne	.L5131
	testb	$8, 18(%r13)
	je	.L5131
	orb	$8, 18(%r14)
.L5131:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11815
.L5133:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L5132:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5144
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5134
	testb	$1, 18(%rcx)
	je	.L5134
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L5134:
	testq	%rax, %rax
	je	.L5144
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L5139
	testb	$8, 17(%rcx)
	je	.L5139
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L5139:
	testq	%rax, %rax
	je	.L5144
	cmpq	$0, 8(%rax)
	je	.L5144
	cmpb	$29, %dl
	je	.L11816
.L5147:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L5144:
	testb	$8, 18(%r14)
	je	.L5153
	cmpb	$32, %dl
	je	.L5153
	testb	$8, 18(%r13)
	jne	.L5153
	testb	$1, 53(%r13)
	jne	.L5153
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L5149
	cmpq	$0, 8(%rax)
	jne	.L11817
.L5149:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11239:
	xorl	%eax, %eax
	call	warning
	jmp	.L10592
.L11817:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11239
.L11816:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%rbx)
	jne	.L5147
	jmp	.L5144
	.p2align 6,,7
.L11815:
	cmpq	$0, -2376(%rbp)
	je	.L5133
	movq	-2376(%rbp), %r9
	cmpb	$32, 16(%r9)
	jne	.L5132
	jmp	.L5133
.L11814:
	testb	$8, 54(%r13)
	jne	.L5130
	andb	$-9, 18(%r13)
	jmp	.L5130
	.p2align 6,,7
.L10591:
	movq	global_binding_level(%rip), %rax
	jmp	.L5110
.L11801:
	testl	%ecx, %ecx
	jg	.L11110
	testl	%ebx, %ebx
	je	.L5116
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11110
	.p2align 6,,7
.L5124:
	movq	8(%rdx), %rcx
	cmpq	error_mark_node(%rip), %rcx
	cmove	%rcx, %rdx
	jmp	.L5116
.L11800:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L5116
	testq	%rax, %rax
	je	.L10118
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L5114
	.p2align 6,,7
.L11799:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L5107
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L5107
	.p2align 6,,7
.L10913:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L4971
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L4970
.L4971:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10584
	movq	%r13, 80(%rdx)
.L10584:
	movzbl	16(%r13), %eax
.L4974:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L5098
	cmpq	$0, 72(%r12)
	je	.L11818
.L5098:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L4969
	cmpq	$0, 56(%rax)
	je	.L4969
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11819
.L11109:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5104:
	movq	%r12, 8(%r15)
	jmp	.L4969
.L11819:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5104
	jmp	.L11109
.L11818:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r10b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L5099
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L5098
.L5099:
	movq	%rbx, 72(%r13)
	jmp	.L5098
.L4970:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L4974
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L4974
	cmpb	$95, 1(%rcx)
	jne	.L4974
	movq	class_binding_level(%rip), %r10
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%r10, %r10
	movq	%r10, -2384(%rbp)
	movq	%rax, -1064(%rbp)
	jne	.L4978
	testb	$-128, 66(%rsi)
	movq	%rsi, -2384(%rbp)
	je	.L4978
.L4982:
	movq	-2384(%rbp), %rdi
	movq	56(%rdi), %rcx
	testb	$-128, 66(%rcx)
	movq	%rcx, -2384(%rbp)
	jne	.L4982
.L4978:
	movq	-2384(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11820
	movq	-2384(%rbp), %rbx
	movq	-1064(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	saveable_tree_cons
	movq	-2384(%rbp), %r11
	movq	%rax, 8(%r11)
.L4984:
	testq	%r15, %r15
	je	.L4985
	movq	-1064(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L4986
	movq	%r15, 80(%rcx)
.L4986:
	movq	-1064(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L4987
	cmpb	$21, 16(%rbx)
	je	.L11821
.L4988:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L4990
	cmpq	$0, 32(%rax)
	je	.L4989
.L4990:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L11822
.L4991:
	xorl	%ecx, %ecx
.L5026:
	testq	%rcx, %rcx
	jne	.L5027
.L10249:
	movq	-1064(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1064(%rbp), %rdi
	movq	%rax, -2392(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2392(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11823
.L11105:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5029:
	movq	-1064(%rbp), %rdx
	movq	%rdx, 8(%r15)
.L5032:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11824
.L5034:
	movq	-1064(%rbp), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.L5065
	cmpb	$32, 16(%rdx)
	je	.L11825
.L5035:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5054
	movq	-2392(%rbp), %rax
	movq	56(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1080(%rbp)
	je	.L10588
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L5056
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5056:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2392(%rbp), %r8
	movq	-1080(%rbp), %rdi
	leaq	8(%rdx), %rcx
	cmpq	decl_obstack+32(%rip), %rcx
	movq	%r8, 56(%rdi)
	ja	.L11826
.L5058:
	movq	-2392(%rbp), %rsi
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r11)
	cmpb	$32, 16(%rsi)
	je	.L11827
.L10588:
	movq	32(%r15), %rax
.L5065:
	cmpb	$36, (%rax)
	je	.L11828
.L5079:
	movq	current_class_type(%rip), %rdx
	movq	-2392(%rbp), %r9
	movq	-1064(%rbp), %r11
	testq	%rdx, %rdx
	movq	%r9, 80(%r11)
	jne	.L5082
	cmpq	$0, current_function_decl(%rip)
	je	.L5081
.L5082:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L5080
.L5081:
	movq	-2392(%rbp), %rax
	movq	%r15, 72(%rax)
.L4987:
	movq	-2384(%rbp), %rsi
	movzbl	66(%rsi), %r8d
	andl	$15, %r8d
	cmpl	$2, %r8d
	je	.L11829
.L4985:
	movq	-1064(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11830
	movq	-1064(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1064(%rbp), %rdi
	movq	%rax, (%rdi)
.L11108:
	movzbl	16(%r12), %eax
	jmp	.L4974
.L11830:
	movq	%rax, (%rsi)
	jmp	.L11108
.L11829:
	movq	-1064(%rbp), %r11
	orb	$64, 18(%r11)
	movq	80(%r11), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L4985
	movq	-2384(%rbp), %r9
	movq	144(%rax), %r15
	movq	8(%r9), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L4985
.L5080:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11831
	cmpq	$0, 32(%rdx)
	jne	.L4987
	movq	-2392(%rbp), %r8
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r8)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5090
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2392(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1064(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L5092:
	movq	-2392(%rbp), %rax
	movq	current_class_type(%rip), %r10
	movq	152(%rax), %rbx
	movq	%r10, 64(%rax)
	movq	%r10, 16(%rbx)
	jmp	.L4987
.L5090:
	movq	-2392(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L5092
.L11831:
	movq	-2392(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5085
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2392(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1064(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L5087:
	movq	current_function_decl(%rip), %r10
	movq	-2392(%rbp), %rbx
	movq	%r10, 64(%rbx)
	jmp	.L4987
.L5085:
	movq	-2392(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L5087
.L11828:
	cmpb	$95, 1(%rax)
	jne	.L5079
	movq	-2392(%rbp), %rdi
	orb	$64, 53(%rdi)
	jmp	.L5079
.L11827:
	cmpq	$0, 72(%rsi)
	jne	.L10588
	movq	8(%rsi), %r9
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r9, -1088(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5060
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-1080(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1080(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2392(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-1088(%rbp), %r10
	movq	%r10, 8(%rax)
	jmp	.L10588
.L5060:
	movq	-1080(%rbp), %r11
	movq	-2392(%rbp), %rdi
	movq	%r11, 72(%rdi)
	jmp	.L10588
.L11826:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5058
.L5054:
	movq	-2392(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2392(%rbp)
	jmp	.L10588
.L11825:
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L5036
	movq	48(%r15), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L5037
.L5036:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5038
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10115
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L5039
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11832
.L5039:
	testq	%rcx, %rcx
	jne	.L10115
.L10116:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10115
.L5038:
	movq	40(%r15), %rcx
.L5037:
	testq	%rcx, %rcx
	je	.L5041
.L10115:
	cmpb	$32, 16(%rcx)
	je	.L5041
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5041
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L5049
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11106
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L11833
.L11106:
	movq	%rax, %rcx
.L5041:
	movq	-1064(%rbp), %rdx
	cmpq	80(%rdx), %rcx
	jne	.L5035
	jmp	.L10588
.L11833:
	testl	%edx, %edx
	jg	.L11106
	movl	$1, %ebx
	testl	%ebx, %ebx
	je	.L5041
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11106
.L5049:
	movq	8(%rcx), %rsi
	cmpq	error_mark_node(%rip), %rsi
	cmove	%rsi, %rcx
	jmp	.L5041
.L11832:
	movl	$1, %r9d
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %r9d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5041
	testq	%rax, %rax
	je	.L10116
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L5039
.L11824:
	cmpb	$95, 1(%rax)
	jne	.L5034
	jmp	.L5065
.L11823:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5029
	jmp	.L11105
	.p2align 6,,7
.L5027:
	movq	80(%rcx), %rax
	movq	%rax, -2392(%rbp)
	jmp	.L5032
.L11822:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L4993
	movq	80(%rax), %rbx
.L4993:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5026
.L5025:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L5002
	cmpl	$32, %eax
	je	.L11834
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L4996:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L5026
	jmp	.L5025
.L11834:
	movq	8(%rbx), %rsi
	movq	-1064(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %r10
	movq	72(%r10), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10887
	movq	64(%rbx), %rbx
	jmp	.L4996
.L10887:
	movq	32(%rax), %rcx
	jmp	.L5026
.L5002:
	movq	-1064(%rbp), %rax
	movq	80(%rax), %rcx
	movq	56(%rcx), %rbx
	testq	%rbx, %rbx
	je	.L4991
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L5005
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L5006
.L5005:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L5007
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10113
	movq	32(%rdi), %r11
	testq	%r11, %r11
	movq	%r11, -1072(%rbp)
	jne	.L5008
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L11835
.L5008:
	testq	%rcx, %rcx
	jne	.L10113
.L10114:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10113
.L5007:
	movq	40(%rbx), %rcx
.L5006:
	testq	%rcx, %rcx
	je	.L10249
.L10113:
	cmpb	$32, 16(%rcx)
	je	.L5026
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L5026
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L5018
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11104
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L11836
.L11104:
	movq	%rax, %rcx
	jmp	.L5026
.L11836:
	testl	%edx, %edx
	jg	.L11104
	movl	$1, %esi
	testl	%esi, %esi
	je	.L5026
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11104
.L5018:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L5026
	jmp	.L11104
.L11835:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L5026
	testq	%rax, %rax
	je	.L10114
	cmpb	$32, 16(%rax)
	cmovne	-1072(%rbp), %rcx
	jmp	.L5008
.L4989:
	movq	-1064(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1096(%rbp)
	je	.L11837
.L11107:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L5067:
	movq	-1064(%rbp), %r10
	movq	%r10, 8(%r15)
	movq	-1096(%rbp), %r9
	movq	56(%r9), %rax
	testq	%rax, %rax
	movq	%rax, -1104(%rbp)
	je	.L5070
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L5071
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L5071:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1096(%rbp), %r8
	movq	-1104(%rbp), %rax
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	movq	%r8, 56(%rax)
	ja	.L11838
.L5073:
	movq	-1096(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11839
.L5070:
	movq	-1096(%rbp), %rbx
	movq	%rbx, -2392(%rbp)
	jmp	.L10588
.L11839:
	cmpq	$0, 72(%rbx)
	jne	.L5070
	movq	-1096(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %rcx
	movq	%rcx, -1112(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L5075
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1104(%rbp), %r8
	cmpb	$1, 16(%r8)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1104(%rbp), %r9
	movq	32(%rbx), %rdx
	movq	-1104(%rbp), %r10
	movl	$.LC35, %esi
	movq	32(%r9), %rcx
	movl	24(%r10), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1096(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-1112(%rbp), %rdi
	movq	%rdi, 8(%rax)
	jmp	.L5070
.L5075:
	movq	-1104(%rbp), %rcx
	movq	-1096(%rbp), %rdx
	movq	%rcx, 72(%rdx)
	jmp	.L5070
.L11838:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L5073
.L11837:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L5067
	jmp	.L11107
	.p2align 6,,7
.L11821:
	cmpq	$0, class_binding_level(%rip)
	je	.L4988
	movq	144(%rbx), %r9
	testb	$16, 3(%r9)
	jne	.L4987
	jmp	.L4988
	.p2align 6,,7
.L11820:
	movq	-1064(%rbp), %rsi
	movq	8(%rcx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2384(%rbp), %r8
	movq	%rax, 8(%r8)
	jmp	.L4984
	.p2align 6,,7
.L4955:
	movq	-2376(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10583
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L4960
	testb	$8, 18(%r14)
	je	.L4960
	testb	$8, 18(%r13)
	jne	.L4960
	testb	$9, 53(%r13)
	jne	.L4960
	cmpq	%r13, current_function_decl(%rip)
	je	.L11840
.L4964:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4965
	cmpq	$0, 8(%rax)
	jne	.L11841
.L4965:
	movq	-2376(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11103:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2376(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L4960
.L11841:
	movq	-2376(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11103
.L11840:
	movq	-2376(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L4964
	.p2align 6,,7
.L11295:
	cmpq	$0, 64(%rcx)
	jne	.L4953
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L4953
.L11294:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2376(%rbp)
	call	error_with_decl
	jmp	.L4951
.L4944:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L4946
	.p2align 4,,7
.L4950:
	cmpq	%r14, 56(%rdi)
	je	.L4946
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L4950
.L4946:
	movq	%rdi, -2376(%rbp)
	jmp	.L4943
.L11293:
	movq	40(%r14), %rbx
	movq	%rbx, -2376(%rbp)
	jmp	.L4943
.L11292:
	movq	56(%r13), %r14
	jmp	.L4940
.L11291:
	testb	$32, 53(%r13)
	jne	.L4938
	jmp	.L4939
.L10579:
	movzbl	16(%r13), %edx
	jmp	.L4938
.L10568:
	movzbl	16(%r13), %edx
.L4685:
	cmpb	$32, %dl
	je	.L10912
.L4693:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L4831
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L4831
	testb	$1, 53(%rax)
	jne	.L4832
	testb	$8, 18(%rax)
	je	.L4831
.L4832:
	andb	$8, %dl
	je	.L11842
	.p2align 4,,7
.L4831:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10576
	testb	$1, 53(%r13)
	je	.L10576
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L4835
	movq	48(%r14), %r10
	testq	%r10, %r10
	movq	%r10, %rdx
	jne	.L4836
.L4835:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4837
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10111
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L4838
	movq	144(%rdi), %r15
	testb	$1, 3(%r15)
	jne	.L11843
.L4838:
	testq	%rdx, %rdx
	jne	.L10111
.L10112:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10111
.L4837:
	movq	40(%r14), %rdx
.L4836:
	testq	%rdx, %rdx
	je	.L10248
.L10111:
	cmpb	$32, 16(%rdx)
	je	.L4840
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L4840
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L4848
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11100
	testl	%ebx, %ebx
	jle	.L11844
.L11100:
	movq	%rax, %rdx
.L4840:
	testq	%rdx, %rdx
	jne	.L10576
.L10248:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2368(%rbp)
.L4834:
	cmpq	%rax, -2368(%rbp)
	je	.L11845
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11846
.L11101:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L4879:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11847
.L4890:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L4892
	testq	%r12, %r12
	je	.L4893
	testb	$1, 53(%r13)
	jne	.L4893
	cmpb	$34, 16(%r12)
	je	.L11848
.L4893:
	movl	warn_shadow(%rip), %r10d
	testl	%r10d, %r10d
	je	.L4892
	testb	$1, 53(%r13)
	jne	.L4892
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L4892
	testq	%rax, %rax
	jne	.L4892
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L4898
	cmpb	$34, 16(%r12)
	je	.L11849
.L4898:
	cmpq	$0, 56(%r14)
	je	.L4900
	movl	$.LC41, %edi
.L4899:
	testq	%rdi, %rdi
	jne	.L11102
	.p2align 4,,7
.L4892:
	testq	%r12, %r12
	je	.L10577
	movq	-2368(%rbp), %r11
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r11), %rdx
	call	tree_cons
	movq	-2368(%rbp), %r12
	movq	%rax, 16(%r12)
.L10577:
	movzbl	16(%r13), %edx
.L4877:
	leal	-128(%rdx), %ebx
	cmpb	$1, %bl
	jbe	.L4665
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L4665
	cmpb	$18, 16(%rcx)
	je	.L11850
.L4909:
	testb	$64, 46(%rcx)
	je	.L4665
.L4908:
	movq	-2368(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L11851
.L10578:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L4665:
	cmpb	$32, %dl
	je	.L11852
.L4911:
	movq	-2368(%rbp), %rdi
	cmpq	global_binding_level(%rip), %rdi
	movq	(%rdi), %r8
	movq	%r8, (%r13)
	movq	%r13, (%rdi)
	jne	.L4684
	testb	$4, 17(%r13)
	jne	.L4684
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4684
.L11852:
	testq	%r14, %r14
	je	.L4911
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4912
	cmpq	class_binding_level(%rip), %rax
	je	.L4913
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L4917
	cmpb	$32, 16(%rax)
	je	.L4915
.L4917:
	cmpq	$0, current_class_type(%rip)
	je	.L4912
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L4912
	cmpb	$32, 16(%rax)
	je	.L4915
.L4912:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L4916
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4915
	cmpb	$-127, %dl
	je	.L11853
.L4916:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L4911
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11854
.L4923:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4927
	cmpq	class_binding_level(%rip), %rax
	je	.L4928
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L4932
	cmpb	$32, 16(%rax)
	je	.L4930
.L4932:
	cmpq	$0, current_class_type(%rip)
	je	.L4927
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L4927
	cmpb	$32, 16(%rax)
	je	.L4930
.L4927:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L4911
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4930
	cmpb	$-127, %dl
	jne	.L4911
	movq	$0, 8(%rbx)
	jmp	.L4911
.L4930:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L4911
.L4928:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4932
.L11854:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%cl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L4923
.L11853:
	movq	$0, 8(%r14)
	jmp	.L4916
.L4915:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L4916
.L4913:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4917
.L11851:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10578
.L11850:
	movq	8(%rcx), %r9
	testb	$64, 46(%r9)
	jne	.L4908
	jmp	.L4909
.L11102:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L4892
.L4900:
	testq	%r12, %r12
	je	.L4902
	movl	$.LC42, %edi
	jmp	.L4899
.L4902:
	testq	%r15, %r15
	movl	$.LC43, %r8d
	cmovne	%r8, %rdi
	jmp	.L4899
.L11849:
	movl	$.LC40, %edi
	jmp	.L4899
.L11848:
	cmpb	$34, 16(%r13)
	je	.L4893
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L4894
	movq	56(%rax), %rax
.L4894:
	movzbl	66(%rax), %edi
	andl	$15, %edi
	decl	%edi
	jne	.L4892
	movl	$.LC40, %edi
	jmp	.L11102
	.p2align 6,,7
.L11847:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L11855
.L4882:
	testq	%r12, %r12
	jne	.L4890
	testq	%r15, %r15
	jne	.L4890
	testb	$1, 53(%r13)
	je	.L4890
	testb	$8, 18(%r13)
	je	.L4890
	orb	$8, 18(%r14)
	jmp	.L4890
	.p2align 6,,7
.L11855:
	testq	%r15, %r15
	je	.L4882
	cmpb	$29, 16(%r13)
	jne	.L4882
	cmpb	$29, 16(%r15)
	jne	.L4882
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11856
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L4885
	movzbl	53(%r13), %r9d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r9b
	orb	%sil, %r9b
	movb	%r9b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L4886
	movq	88(%r15), %rax
.L4887:
	movq	%rax, (%rdx)
	movq	72(%r15), %rbx
	movq	80(%r15), %r11
	movq	136(%r15), %rdx
	movzbl	17(%r13), %edi
	movq	%rbx, 72(%r13)
	movq	%r11, 80(%r13)
	movq	%rdx, 136(%r13)
	movzbl	17(%r15), %r8d
	movq	%r15, 96(%r13)
	andb	$127, %dil
	shrb	$7, %r8b
	movzbl	%r8b, %ecx
	movl	%ecx, %r10d
	salb	$7, %r10b
	orb	%r10b, %dil
	movb	%dil, 17(%r13)
	movzbl	53(%r15), %ecx
.L4885:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L4888
	movzbl	53(%r13), %r9d
	salb	$4, %al
	andb	$-17, %r9b
	orb	%al, %r9b
	movb	%r9b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L4888:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L4882
	cmpq	$0, 88(%r15)
	je	.L4882
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L4882
	movq	%rdx, 8(%r13)
	jmp	.L4882
.L4886:
	xorl	%eax, %eax
	jmp	.L4887
	.p2align 6,,7
.L11856:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L4882
	.p2align 6,,7
.L11846:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4879
	jmp	.L11101
.L11845:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11857
.L4854:
	cmpq	$0, 40(%r14)
	jne	.L4855
	testb	$8, 18(%r13)
	je	.L4855
	orb	$8, 18(%r14)
.L4855:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11858
.L4857:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L4856:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4868
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4858
	testb	$1, 18(%rcx)
	je	.L4858
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L4858:
	testq	%rax, %rax
	je	.L4868
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4863
	testb	$8, 17(%rcx)
	je	.L4863
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L4863:
	testq	%rax, %rax
	je	.L4868
	cmpq	$0, 8(%rax)
	je	.L4868
	cmpb	$29, %dl
	je	.L11859
.L4871:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L4868:
	testb	$8, 18(%r14)
	je	.L4877
	cmpb	$32, %dl
	je	.L4877
	testb	$8, 18(%r13)
	jne	.L4877
	testb	$1, 53(%r13)
	jne	.L4877
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4873
	cmpq	$0, 8(%rax)
	jne	.L11860
.L4873:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11238:
	xorl	%eax, %eax
	call	warning
	jmp	.L10577
.L11860:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11238
.L11859:
	movq	8(%r13), %rbx
	movq	integer_type_node(%rip), %r11
	cmpq	%r11, 8(%rbx)
	jne	.L4871
	jmp	.L4868
	.p2align 6,,7
.L11858:
	cmpq	$0, -2344(%rbp)
	je	.L4857
	movq	-2344(%rbp), %r8
	cmpb	$32, 16(%r8)
	jne	.L4856
	jmp	.L4857
.L11857:
	testb	$8, 54(%r13)
	jne	.L4854
	andb	$-9, 18(%r13)
	jmp	.L4854
	.p2align 6,,7
.L10576:
	movq	global_binding_level(%rip), %rax
	jmp	.L4834
.L11844:
	testl	%ecx, %ecx
	jg	.L11100
	testl	%ebx, %ebx
	je	.L4840
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11100
	.p2align 6,,7
.L4848:
	movq	8(%rdx), %rcx
	cmpq	error_mark_node(%rip), %rcx
	cmove	%rcx, %rdx
	jmp	.L4840
.L11843:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L4840
	testq	%rax, %rax
	je	.L10112
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L4838
	.p2align 6,,7
.L11842:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L4831
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L4831
	.p2align 6,,7
.L10912:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L4695
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L4694
.L4695:
	movq	global_binding_level(%rip), %r15
	movq	%r13, %r12
	cmpq	%r15, current_binding_level(%rip)
	jne	.L10569
	movq	%r13, 80(%rdx)
.L10569:
	movzbl	16(%r13), %eax
.L4698:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L4822
	cmpq	$0, 72(%r12)
	je	.L11861
.L4822:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L4693
	cmpq	$0, 56(%rax)
	je	.L4693
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11862
.L11099:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4828:
	movq	%r12, 8(%r15)
	jmp	.L4693
.L11862:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4828
	jmp	.L11099
.L11861:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%al
	movq	8(%r13), %r15
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L4823
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L4822
.L4823:
	movq	%rbx, 72(%r13)
	jmp	.L4822
.L4694:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L4698
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L4698
	cmpb	$95, 1(%rcx)
	jne	.L4698
	movq	class_binding_level(%rip), %rcx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rax
	testq	%rcx, %rcx
	movq	%rcx, -2352(%rbp)
	movq	%rax, -1008(%rbp)
	jne	.L4702
	testb	$-128, 66(%rsi)
	movq	%rsi, -2352(%rbp)
	je	.L4702
.L4706:
	movq	-2352(%rbp), %r11
	movq	56(%r11), %rbx
	testb	$-128, 66(%rbx)
	movq	%rbx, -2352(%rbp)
	jne	.L4706
.L4702:
	movq	-2352(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11863
	movq	-2352(%rbp), %r10
	movq	-1008(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r10), %rdx
	call	saveable_tree_cons
	movq	-2352(%rbp), %r9
	movq	%rax, 8(%r9)
.L4708:
	testq	%r15, %r15
	je	.L4709
	movq	-1008(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L4710
	movq	%r15, 80(%rcx)
.L4710:
	movq	-1008(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L4711
	cmpb	$21, 16(%rbx)
	je	.L11864
.L4712:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L4714
	cmpq	$0, 32(%rax)
	je	.L4713
.L4714:
	movq	lang_name_cplusplus(%rip), %r8
	cmpq	%r8, current_lang_name(%rip)
	je	.L11865
.L4715:
	xorl	%ecx, %ecx
.L4750:
	testq	%rcx, %rcx
	jne	.L4751
.L10247:
	movq	-1008(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-1008(%rbp), %rdi
	movq	%rax, -2360(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2360(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11866
.L11095:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4753:
	movq	-1008(%rbp), %rcx
	movq	%rcx, 8(%r15)
.L4756:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11867
.L4758:
	movq	-1008(%rbp), %r9
	movq	80(%r9), %rdx
	testq	%rdx, %rdx
	je	.L4789
	cmpb	$32, 16(%rdx)
	je	.L11868
.L4759:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4778
	movq	-2360(%rbp), %rcx
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -1024(%rbp)
	je	.L10573
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L4780
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4780:
	movq	-1024(%rbp), %rdx
	movq	-2360(%rbp), %r10
	movq	%r10, 56(%rdx)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %r9
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L11869
.L4782:
	movq	-2360(%rbp), %rsi
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r8)
	cmpb	$32, 16(%rsi)
	je	.L11870
.L10573:
	movq	32(%r15), %rax
.L4789:
	cmpb	$36, (%rax)
	je	.L11871
.L4803:
	movq	current_class_type(%rip), %rdx
	movq	-2360(%rbp), %r11
	movq	-1008(%rbp), %r8
	testq	%rdx, %rdx
	movq	%r11, 80(%r8)
	jne	.L4806
	cmpq	$0, current_function_decl(%rip)
	je	.L4805
.L4806:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L4804
.L4805:
	movq	-2360(%rbp), %rax
	movq	%r15, 72(%rax)
.L4711:
	movq	-2352(%rbp), %rsi
	movzbl	66(%rsi), %r9d
	andl	$15, %r9d
	cmpl	$2, %r9d
	je	.L11872
.L4709:
	movq	-1008(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11873
	movq	-1008(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-1008(%rbp), %r11
	movq	%rax, (%r11)
.L11098:
	movzbl	16(%r12), %eax
	jmp	.L4698
.L11873:
	movq	%rax, (%rsi)
	jmp	.L11098
.L11872:
	movq	-1008(%rbp), %r10
	orb	$64, 18(%r10)
	movq	80(%r10), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L4709
	movq	-2352(%rbp), %r8
	movq	144(%rax), %r15
	movq	8(%r8), %rcx
	movq	%rcx, 72(%r15)
	jmp	.L4709
.L4804:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11874
	cmpq	$0, 32(%rdx)
	jne	.L4711
	movq	-2360(%rbp), %r9
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r9)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4814
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2360(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1008(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L4816:
	movq	-2360(%rbp), %rax
	movq	current_class_type(%rip), %rdi
	movq	152(%rax), %rbx
	movq	%rdi, 64(%rax)
	movq	%rdi, 16(%rbx)
	jmp	.L4711
.L4814:
	movq	-2360(%rbp), %r11
	movq	%r15, 72(%r11)
	jmp	.L4816
.L11874:
	movq	-2360(%rbp), %rsi
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rsi)
	movl	$136, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4809
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2360(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-1008(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L4811:
	movq	current_function_decl(%rip), %rdi
	movq	-2360(%rbp), %rbx
	movq	%rdi, 64(%rbx)
	jmp	.L4711
.L4809:
	movq	-2360(%rbp), %r11
	movq	%r15, 72(%r11)
	jmp	.L4811
.L11871:
	cmpb	$95, 1(%rax)
	jne	.L4803
	movq	-2360(%rbp), %r10
	orb	$64, 53(%r10)
	jmp	.L4803
.L11870:
	cmpq	$0, 72(%rsi)
	jne	.L10573
	movq	8(%rsi), %rdi
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	%rdi, -1032(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4784
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-1024(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-1024(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2360(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-1032(%rbp), %rbx
	movq	%rbx, 8(%rax)
	jmp	.L10573
.L4784:
	movq	-1024(%rbp), %r8
	movq	-2360(%rbp), %r10
	movq	%r8, 72(%r10)
	jmp	.L10573
.L11869:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4782
.L4778:
	movq	-2360(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2360(%rbp)
	jmp	.L10573
.L11868:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L4760
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L4761
.L4760:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4762
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10109
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L4763
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11875
.L4763:
	testq	%rcx, %rcx
	jne	.L10109
.L10110:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10109
.L4762:
	movq	40(%r15), %rcx
.L4761:
	testq	%rcx, %rcx
	je	.L4765
.L10109:
	cmpb	$32, 16(%rcx)
	je	.L4765
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4765
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L4773
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11096
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11876
.L11096:
	movq	%rax, %rcx
.L4765:
	movq	-1008(%rbp), %r11
	cmpq	80(%r11), %rcx
	jne	.L4759
	jmp	.L10573
.L11876:
	testl	%edx, %edx
	jg	.L11096
	movl	$1, %ebx
	testl	%ebx, %ebx
	je	.L4765
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11096
.L4773:
	movq	8(%rcx), %rsi
	cmpq	error_mark_node(%rip), %rsi
	cmove	%rsi, %rcx
	jmp	.L4765
.L11875:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4765
	testq	%rax, %rax
	je	.L10110
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L4763
.L11867:
	cmpb	$95, 1(%rax)
	jne	.L4758
	jmp	.L4789
.L11866:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4753
	jmp	.L11095
	.p2align 6,,7
.L4751:
	movq	80(%rcx), %r11
	movq	%r11, -2360(%rbp)
	jmp	.L4756
.L11865:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L4717
	movq	80(%rax), %rbx
.L4717:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4750
.L4749:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L4726
	cmpl	$32, %eax
	je	.L11877
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L4720:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4750
	jmp	.L4749
.L11877:
	movq	8(%rbx), %rsi
	movq	-1008(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10886
	movq	64(%rbx), %rbx
	jmp	.L4720
.L10886:
	movq	32(%rax), %rcx
	jmp	.L4750
.L4726:
	movq	-1008(%rbp), %rax
	movq	80(%rax), %r11
	movq	56(%r11), %rbx
	testq	%rbx, %rbx
	je	.L4715
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L4729
	movq	48(%rbx), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L4730
.L4729:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4731
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10107
	movq	32(%rdi), %r10
	testq	%r10, %r10
	movq	%r10, -1016(%rbp)
	jne	.L4732
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L11878
.L4732:
	testq	%rcx, %rcx
	jne	.L10107
.L10108:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10107
.L4731:
	movq	40(%rbx), %rcx
.L4730:
	testq	%rcx, %rcx
	je	.L10247
.L10107:
	cmpb	$32, 16(%rcx)
	je	.L4750
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4750
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L4742
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11094
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11879
.L11094:
	movq	%rax, %rcx
	jmp	.L4750
.L11879:
	testl	%edx, %edx
	jg	.L11094
	movl	$1, %esi
	testl	%esi, %esi
	je	.L4750
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11094
.L4742:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L4750
	jmp	.L11094
.L11878:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4750
	testq	%rax, %rax
	je	.L10108
	cmpb	$32, 16(%rax)
	cmovne	-1016(%rbp), %rcx
	jmp	.L4732
.L4713:
	movq	-1008(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -1040(%rbp)
	je	.L11880
.L11097:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4791:
	movq	-1008(%rbp), %rbx
	movq	%rbx, 8(%r15)
	movq	-1040(%rbp), %rdi
	movq	56(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, -1048(%rbp)
	je	.L4794
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L4795
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4795:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1040(%rbp), %r9
	movq	-1048(%rbp), %rax
	leaq	8(%rdx), %r11
	cmpq	decl_obstack+32(%rip), %r11
	movq	%r9, 56(%rax)
	ja	.L11881
.L4797:
	movq	-1040(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L11882
.L4794:
	movq	-1040(%rbp), %rbx
	movq	%rbx, -2360(%rbp)
	jmp	.L10573
.L11882:
	cmpq	$0, 72(%rbx)
	jne	.L4794
	movq	-1040(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %rcx
	movq	%rcx, -1056(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4799
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-1048(%rbp), %r9
	cmpb	$1, 16(%r9)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-1048(%rbp), %r11
	movq	32(%rbx), %rdx
	movq	-1048(%rbp), %rdi
	movl	$.LC35, %esi
	movq	32(%r11), %rcx
	movl	24(%rdi), %eax
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1040(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-1056(%rbp), %r10
	movq	%r10, 8(%rax)
	jmp	.L4794
.L4799:
	movq	-1048(%rbp), %rcx
	movq	-1040(%rbp), %rdx
	movq	%rcx, 72(%rdx)
	jmp	.L4794
.L11881:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4797
.L11880:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4791
	jmp	.L11097
	.p2align 6,,7
.L11864:
	cmpq	$0, class_binding_level(%rip)
	je	.L4712
	movq	144(%rbx), %rdi
	testb	$16, 3(%rdi)
	jne	.L4711
	jmp	.L4712
	.p2align 6,,7
.L11863:
	movq	8(%rcx), %rdx
	movq	-1008(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2352(%rbp), %rdx
	movq	%rax, 8(%rdx)
	jmp	.L4708
	.p2align 6,,7
.L4679:
	movq	-2344(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10568
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L4684
	testb	$8, 18(%r14)
	je	.L4684
	testb	$8, 18(%r13)
	jne	.L4684
	testb	$9, 53(%r13)
	jne	.L4684
	cmpq	%r13, current_function_decl(%rip)
	je	.L11883
.L4688:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4689
	cmpq	$0, 8(%rax)
	jne	.L11884
.L4689:
	movq	-2344(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11093:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2344(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L4684
.L11884:
	movq	-2344(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11093
.L11883:
	movq	-2344(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L4688
	.p2align 6,,7
.L11290:
	cmpq	$0, 64(%rcx)
	jne	.L4677
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L4677
.L11289:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2344(%rbp)
	call	error_with_decl
	jmp	.L4675
.L4668:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L4670
	.p2align 4,,7
.L4674:
	cmpq	%r14, 56(%rdi)
	je	.L4670
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L4674
.L4670:
	movq	%rdi, -2344(%rbp)
	jmp	.L4667
.L11288:
	movq	40(%r14), %rdi
	movq	%rdi, -2344(%rbp)
	jmp	.L4667
.L11287:
	movq	56(%r13), %r14
	jmp	.L4664
.L11286:
	testb	$32, 53(%r13)
	jne	.L4662
	jmp	.L4663
.L10564:
	movzbl	16(%r13), %edx
	jmp	.L4662
.L10553:
	movzbl	16(%r13), %edx
.L4409:
	cmpb	$32, %dl
	je	.L10911
.L4417:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L4555
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L4555
	testb	$1, 53(%rax)
	jne	.L4556
	testb	$8, 18(%rax)
	je	.L4555
.L4556:
	andb	$8, %dl
	je	.L11885
	.p2align 4,,7
.L4555:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10561
	testb	$1, 53(%r13)
	je	.L10561
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L4559
	movq	48(%r14), %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	jne	.L4560
.L4559:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4561
	movq	56(%r14), %rdx
	testq	%rdx, %rdx
	jne	.L10105
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L4562
	movq	144(%rdi), %r15
	testb	$1, 3(%r15)
	jne	.L11886
.L4562:
	testq	%rdx, %rdx
	jne	.L10105
.L10106:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.L10105
.L4561:
	movq	40(%r14), %rdx
.L4560:
	testq	%rdx, %rdx
	je	.L10246
.L10105:
	cmpb	$32, 16(%rdx)
	je	.L4564
	movl	looking_for_typename(%rip), %ecx
	testl	%ecx, %ecx
	js	.L4564
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L4572
	movq	80(%rax), %rax
	cmpq	%rax, %rdx
	je	.L11090
	testl	%ebx, %ebx
	jle	.L11887
.L11090:
	movq	%rax, %rdx
.L4564:
	testq	%rdx, %rdx
	jne	.L10561
.L10246:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2336(%rbp)
.L4558:
	cmpq	%rax, -2336(%rbp)
	je	.L11888
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r15
	cmpq	%rax, %rbx
	je	.L11889
.L11091:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 16(%rbx)
.L4603:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L11890
.L4614:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L4616
	testq	%r12, %r12
	je	.L4617
	testb	$1, 53(%r13)
	jne	.L4617
	cmpb	$34, 16(%r12)
	je	.L11891
.L4617:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L4616
	testb	$1, 53(%r13)
	jne	.L4616
	movl	32(%r13), %edx
	testl	%edx, %edx
	je	.L4616
	testq	%rax, %rax
	jne	.L4616
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L4622
	cmpb	$34, 16(%r12)
	je	.L11892
.L4622:
	cmpq	$0, 56(%r14)
	je	.L4624
	movl	$.LC41, %edi
.L4623:
	testq	%rdi, %rdi
	jne	.L11092
	.p2align 4,,7
.L4616:
	testq	%r12, %r12
	je	.L10562
	movq	-2336(%rbp), %r9
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	16(%r9), %rdx
	call	tree_cons
	movq	-2336(%rbp), %r12
	movq	%rax, 16(%r12)
.L10562:
	movzbl	16(%r13), %edx
.L4601:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L4389
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L4389
	cmpb	$18, 16(%rcx)
	je	.L11893
.L4633:
	testb	$64, 46(%rcx)
	je	.L4389
.L4632:
	movq	-2336(%rbp), %r8
	movzwl	64(%r8), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%r8)
	je	.L11894
.L10563:
	movzbl	16(%r13), %edx
	.p2align 4,,7
.L4389:
	cmpb	$32, %dl
	je	.L11895
.L4635:
	movq	-2336(%rbp), %rdx
	cmpq	global_binding_level(%rip), %rdx
	movq	(%rdx), %r11
	movq	%r11, (%r13)
	movq	%r13, (%rdx)
	jne	.L4408
	testb	$4, 17(%r13)
	jne	.L4408
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4408
.L11895:
	testq	%r14, %r14
	je	.L4635
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4636
	cmpq	class_binding_level(%rip), %rax
	je	.L4637
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L4641
	cmpb	$32, 16(%rax)
	je	.L4639
.L4641:
	cmpq	$0, current_class_type(%rip)
	je	.L4636
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L4636
	cmpb	$32, 16(%rax)
	je	.L4639
.L4636:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L4640
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4639
	cmpb	$-127, %dl
	je	.L11896
.L4640:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L4635
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11897
.L4647:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4651
	cmpq	class_binding_level(%rip), %rax
	je	.L4652
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L4656
	cmpb	$32, 16(%rax)
	je	.L4654
.L4656:
	cmpq	$0, current_class_type(%rip)
	je	.L4651
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L4651
	cmpb	$32, 16(%rax)
	je	.L4654
.L4651:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L4635
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4654
	cmpb	$-127, %dl
	jne	.L4635
	movq	$0, 8(%rbx)
	jmp	.L4635
.L4654:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L4635
.L4652:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4656
.L11897:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %r12
	sete	%bl
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%r14), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L4647
.L11896:
	movq	$0, 8(%r14)
	jmp	.L4640
.L4639:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L4640
.L4637:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4641
.L11894:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10563
.L11893:
	movq	8(%rcx), %r10
	testb	$64, 46(%r10)
	jne	.L4632
	jmp	.L4633
.L11092:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L4616
.L4624:
	testq	%r12, %r12
	je	.L4626
	movl	$.LC42, %edi
	jmp	.L4623
.L4626:
	testq	%r15, %r15
	movl	$.LC43, %r11d
	cmovne	%r11, %rdi
	jmp	.L4623
.L11892:
	movl	$.LC40, %edi
	jmp	.L4623
.L11891:
	cmpb	$34, 16(%r13)
	je	.L4617
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r15
	movq	56(%r15), %rax
	je	.L4618
	movq	56(%rax), %rax
.L4618:
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	decl	%ecx
	jne	.L4616
	movl	$.LC40, %edi
	jmp	.L11092
	.p2align 6,,7
.L11890:
	movzbl	53(%r13), %r10d
	andb	$9, %r10b
	decb	%r10b
	je	.L11898
.L4606:
	testq	%r12, %r12
	jne	.L4614
	testq	%r15, %r15
	jne	.L4614
	testb	$1, 53(%r13)
	je	.L4614
	testb	$8, 18(%r13)
	je	.L4614
	orb	$8, 18(%r14)
	jmp	.L4614
	.p2align 6,,7
.L11898:
	testq	%r15, %r15
	je	.L4606
	cmpb	$29, 16(%r13)
	jne	.L4606
	cmpb	$29, 16(%r15)
	jne	.L4606
	movq	8(%r13), %rdi
	movq	8(%r15), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L11899
	movzbl	53(%r15), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L4609
	movzbl	53(%r13), %r8d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r8b
	orb	%sil, %r8b
	movb	%r8b, 53(%r13)
	cmpq	%r15, current_function_decl(%rip)
	je	.L4610
	movq	88(%r15), %rax
.L4611:
	movq	136(%r15), %r10
	movq	72(%r15), %r9
	movq	%rax, (%rdx)
	movq	80(%r15), %rdi
	movzbl	17(%r13), %ecx
	movq	%r10, 136(%r13)
	movq	%r9, 72(%r13)
	movq	%rdi, 80(%r13)
	movzbl	17(%r15), %r11d
	movq	%r15, 96(%r13)
	andb	$127, %cl
	shrb	$7, %r11b
	movzbl	%r11b, %edx
	movl	%edx, %ebx
	salb	$7, %bl
	orb	%bl, %cl
	movb	%cl, 17(%r13)
	movzbl	53(%r15), %ecx
.L4609:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L4612
	movzbl	53(%r13), %r8d
	salb	$4, %al
	andb	$-17, %r8b
	orb	%al, %r8b
	movb	%r8b, 53(%r13)
	movl	128(%r15), %eax
	movl	%eax, 128(%r13)
.L4612:
	movq	8(%r15), %rdx
	cmpq	$0, 24(%rdx)
	je	.L4606
	cmpq	$0, 88(%r15)
	je	.L4606
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L4606
	movq	%rdx, 8(%r13)
	jmp	.L4606
.L4610:
	xorl	%eax, %eax
	jmp	.L4611
	.p2align 6,,7
.L11899:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	jmp	.L4606
	.p2align 6,,7
.L11889:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4603
	jmp	.L11091
.L11888:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11900
.L4578:
	cmpq	$0, 40(%r14)
	jne	.L4579
	testb	$8, 18(%r13)
	je	.L4579
	orb	$8, 18(%r14)
.L4579:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11901
.L4581:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L4580:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4592
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4582
	testb	$1, 18(%rcx)
	je	.L4582
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L4582:
	testq	%rax, %rax
	je	.L4592
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4587
	testb	$8, 17(%rcx)
	je	.L4587
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L4587:
	testq	%rax, %rax
	je	.L4592
	cmpq	$0, 8(%rax)
	je	.L4592
	cmpb	$29, %dl
	je	.L11902
.L4595:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L4592:
	testb	$8, 18(%r14)
	je	.L4601
	cmpb	$32, %dl
	je	.L4601
	testb	$8, 18(%r13)
	jne	.L4601
	testb	$1, 53(%r13)
	jne	.L4601
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4597
	cmpq	$0, 8(%rax)
	jne	.L11903
.L4597:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11237:
	xorl	%eax, %eax
	call	warning
	jmp	.L10562
.L11903:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11237
.L11902:
	movq	8(%r13), %r9
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r9)
	jne	.L4595
	jmp	.L4592
	.p2align 6,,7
.L11901:
	cmpq	$0, -2312(%rbp)
	je	.L4581
	movq	-2312(%rbp), %r11
	cmpb	$32, 16(%r11)
	jne	.L4580
	jmp	.L4581
.L11900:
	testb	$8, 54(%r13)
	jne	.L4578
	andb	$-9, 18(%r13)
	jmp	.L4578
	.p2align 6,,7
.L10561:
	movq	global_binding_level(%rip), %rax
	jmp	.L4558
.L11887:
	testl	%ecx, %ecx
	jg	.L11090
	testl	%ebx, %ebx
	je	.L4564
	movq	%rdx, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11090
	.p2align 6,,7
.L4572:
	movq	8(%rdx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rdx
	jmp	.L4564
.L11886:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rdx
	je	.L4564
	testq	%rax, %rax
	je	.L10106
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rdx
	jmp	.L4562
	.p2align 6,,7
.L11885:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L4555
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L4555
	.p2align 6,,7
.L10911:
	movq	8(%r13), %rdx
	movq	80(%rdx), %r12
	testq	%r12, %r12
	je	.L4419
	movzbl	16(%r12), %eax
	cmpb	$32, %al
	je	.L4418
.L4419:
	movq	global_binding_level(%rip), %r11
	movq	%r13, %r12
	cmpq	%r11, current_binding_level(%rip)
	jne	.L10554
	movq	%r13, 80(%rdx)
.L10554:
	movzbl	16(%r13), %eax
.L4422:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L4546
	cmpq	$0, 72(%r12)
	je	.L11904
.L4546:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L4417
	cmpq	$0, 56(%rax)
	je	.L4417
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r15
	cmpq	global_binding_level(%rip), %rbx
	je	.L11905
.L11089:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4552:
	movq	%r12, 8(%r15)
	jmp	.L4417
.L11905:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4552
	jmp	.L11089
.L11904:
	cmpb	$32, 16(%r13)
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	sete	%r10b
	movq	8(%r13), %r15
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L4547
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r15, 8(%rax)
	jmp	.L4546
.L4547:
	movq	%rbx, 72(%r13)
	jmp	.L4546
.L4418:
	movq	current_binding_level(%rip), %rsi
	movq	56(%r12), %r15
	cmpq	global_binding_level(%rip), %rsi
	jne	.L4422
	movq	32(%r15), %rcx
	cmpb	$36, (%rcx)
	jne	.L4422
	cmpb	$95, 1(%rcx)
	jne	.L4422
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %r10
	testq	%r9, %r9
	movq	%r9, -2320(%rbp)
	movq	%r10, -952(%rbp)
	jne	.L4426
	testb	$-128, 66(%rsi)
	movq	%rsi, -2320(%rbp)
	je	.L4426
.L4430:
	movq	-2320(%rbp), %rcx
	movq	56(%rcx), %rbx
	testb	$-128, 66(%rbx)
	movq	%rbx, -2320(%rbp)
	jne	.L4430
.L4426:
	movq	-2320(%rbp), %rcx
	cmpq	global_binding_level(%rip), %rcx
	je	.L11906
	movq	-2320(%rbp), %r11
	movq	-952(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	8(%r11), %rdx
	call	saveable_tree_cons
	movq	-2320(%rbp), %r8
	movq	%rax, 8(%r8)
.L4432:
	testq	%r15, %r15
	je	.L4433
	movq	-952(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L4434
	movq	%r15, 80(%rcx)
.L4434:
	movq	-952(%rbp), %rbx
	cmpq	%rbx, 8(%r15)
	je	.L4435
	cmpb	$21, 16(%rbx)
	je	.L11907
.L4436:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L4438
	cmpq	$0, 32(%rax)
	je	.L4437
.L4438:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L11908
.L4439:
	xorl	%ecx, %ecx
.L4474:
	testq	%rcx, %rcx
	jne	.L4475
.L10245:
	movq	-952(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-952(%rbp), %rdi
	movq	%rax, -2328(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2328(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11909
.L11085:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4477:
	movq	-952(%rbp), %r10
	movq	%r10, 8(%r15)
.L4480:
	movq	32(%r15), %rax
	cmpb	$36, (%rax)
	je	.L11910
.L4482:
	movq	-952(%rbp), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.L4513
	cmpb	$32, 16(%rdx)
	je	.L11911
.L4483:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4502
	movq	-2328(%rbp), %rax
	movq	56(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, -968(%rbp)
	je	.L10558
	movq	56(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L4504
	movq	%rdx, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4504:
	movq	-968(%rbp), %rdx
	movq	-2328(%rbp), %r8
	movq	%r8, 56(%rdx)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rcx
	cmpq	decl_obstack+32(%rip), %rcx
	ja	.L11912
.L4506:
	movq	-2328(%rbp), %rsi
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rsi, (%r11)
	cmpb	$32, 16(%rsi)
	je	.L11913
.L10558:
	movq	32(%r15), %rax
.L4513:
	cmpb	$36, (%rax)
	je	.L11914
.L4527:
	movq	current_class_type(%rip), %rdx
	movq	-2328(%rbp), %r9
	movq	-952(%rbp), %rdi
	testq	%rdx, %rdx
	movq	%r9, 80(%rdi)
	jne	.L4530
	cmpq	$0, current_function_decl(%rip)
	je	.L4529
.L4530:
	movq	lang_name_cplusplus(%rip), %rsi
	cmpq	%rsi, current_lang_name(%rip)
	je	.L4528
.L4529:
	movq	-2328(%rbp), %r10
	movq	%r15, 72(%r10)
.L4435:
	movq	-2320(%rbp), %rcx
	movzbl	66(%rcx), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L11915
.L4433:
	movq	-952(%rbp), %rsi
	movq	80(%rsi), %rax
	cmpb	$32, 16(%rax)
	je	.L11916
	movq	-952(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-952(%rbp), %rdi
	movq	%rax, (%rdi)
.L11088:
	movzbl	16(%r12), %eax
	jmp	.L4422
.L11916:
	movq	%rax, (%rsi)
	jmp	.L11088
.L11915:
	movq	-952(%rbp), %rbx
	orb	$64, 18(%rbx)
	movq	80(%rbx), %rdx
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r15)
	cmpq	$0, 32(%rax)
	jne	.L4433
	movq	144(%rax), %r15
	movq	-2320(%rbp), %rax
	movq	8(%rax), %r11
	movq	%r11, 72(%r15)
	jmp	.L4433
.L4528:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11917
	cmpq	$0, 32(%rdx)
	jne	.L4435
	movq	-2328(%rbp), %r8
	movq	80(%rdx), %rcx
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	movq	72(%rcx), %rbx
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4538
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2328(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-952(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L4540:
	movq	-2328(%rbp), %r8
	movq	current_class_type(%rip), %r10
	movq	152(%r8), %r9
	movq	%r10, 64(%r8)
	movq	%r10, 16(%r9)
	jmp	.L4435
.L4538:
	movq	-2328(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L4540
.L11917:
	movq	-2328(%rbp), %rcx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rcx)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4533
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r15)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r15), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r15), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2328(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-952(%rbp), %rdx
	movq	%rdx, 8(%rax)
.L4535:
	movq	current_function_decl(%rip), %rsi
	movq	-2328(%rbp), %r9
	movq	%rsi, 64(%r9)
	jmp	.L4435
.L4533:
	movq	-2328(%rbp), %rdi
	movq	%r15, 72(%rdi)
	jmp	.L4535
.L11914:
	cmpb	$95, 1(%rax)
	jne	.L4527
	movq	-2328(%rbp), %r11
	orb	$64, 53(%r11)
	jmp	.L4527
.L11913:
	cmpq	$0, 72(%rsi)
	jne	.L10558
	movq	8(%rsi), %rdi
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	%rdi, -976(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4508
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-968(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-968(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2328(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-976(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L10558
.L4508:
	movq	-968(%rbp), %rdi
	movq	-2328(%rbp), %r11
	movq	%rdi, 72(%r11)
	jmp	.L10558
.L11912:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4506
.L4502:
	movq	-2328(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2328(%rbp)
	jmp	.L10558
.L11911:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L4484
	movq	48(%r15), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L4485
.L4484:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4486
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10103
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L4487
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L11918
.L4487:
	testq	%rcx, %rcx
	jne	.L10103
.L10104:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10103
.L4486:
	movq	40(%r15), %rcx
.L4485:
	testq	%rcx, %rcx
	je	.L4489
.L10103:
	cmpb	$32, 16(%rcx)
	je	.L4489
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4489
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L4497
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11086
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11919
.L11086:
	movq	%rax, %rcx
.L4489:
	movq	-952(%rbp), %r10
	cmpq	80(%r10), %rcx
	jne	.L4483
	jmp	.L10558
.L11919:
	testl	%edx, %edx
	jg	.L11086
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L4489
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11086
.L4497:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L4489
.L11918:
	movl	$1, %esi
	xorl	%ecx, %ecx
	cmpl	$-1, %esi
	movq	%r15, %rsi
	sete	%cl
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4489
	testq	%rax, %rax
	je	.L10104
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L4487
.L11910:
	cmpb	$95, 1(%rax)
	jne	.L4482
	jmp	.L4513
.L11909:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4477
	jmp	.L11085
	.p2align 6,,7
.L4475:
	movq	80(%rcx), %rax
	movq	%rax, -2328(%rbp)
	jmp	.L4480
.L11908:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L4441
	movq	80(%rax), %rbx
.L4441:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4474
.L4473:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L4450
	cmpl	$32, %eax
	je	.L11920
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L4444:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4474
	jmp	.L4473
.L11920:
	movq	8(%rbx), %r10
	movq	-952(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r10), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10885
	movq	64(%rbx), %rbx
	jmp	.L4444
.L10885:
	movq	32(%rax), %rcx
	jmp	.L4474
.L4450:
	movq	-952(%rbp), %rax
	movq	80(%rax), %rcx
	movq	56(%rcx), %rbx
	testq	%rbx, %rbx
	je	.L4439
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L4453
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L4454
.L4453:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4455
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10101
	movq	32(%rdi), %r11
	testq	%r11, %r11
	movq	%r11, -960(%rbp)
	jne	.L4456
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L11921
.L4456:
	testq	%rcx, %rcx
	jne	.L10101
.L10102:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10101
.L4455:
	movq	40(%rbx), %rcx
.L4454:
	testq	%rcx, %rcx
	je	.L10245
.L10101:
	cmpb	$32, 16(%rcx)
	je	.L4474
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4474
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L4466
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11084
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L11922
.L11084:
	movq	%rax, %rcx
	jmp	.L4474
.L11922:
	testl	%edx, %edx
	jg	.L11084
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L4474
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11084
.L4466:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L4474
	jmp	.L11084
.L11921:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4474
	testq	%rax, %rax
	je	.L10102
	cmpb	$32, 16(%rax)
	cmovne	-960(%rbp), %rcx
	jmp	.L4456
.L4437:
	movq	-952(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rsi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -984(%rbp)
	je	.L11923
.L11087:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4515:
	movq	-952(%rbp), %r9
	movq	%r9, 8(%r15)
	movq	-984(%rbp), %rbx
	movq	56(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, -992(%rbp)
	je	.L4518
	movq	56(%rax), %rsi
	testq	%rsi, %rsi
	je	.L4519
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4519:
	movq	decl_obstack+24(%rip), %rdx
	movq	-984(%rbp), %r8
	movq	-992(%rbp), %r10
	leaq	8(%rdx), %rsi
	cmpq	decl_obstack+32(%rip), %rsi
	movq	%r8, 56(%r10)
	ja	.L11924
.L4521:
	movq	-984(%rbp), %rbx
	movq	%rdx, %rcx
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rcx)
	cmpb	$32, 16(%rbx)
	je	.L11925
.L4518:
	movq	-984(%rbp), %rbx
	movq	%rbx, -2328(%rbp)
	jmp	.L10558
.L11925:
	cmpq	$0, 72(%rbx)
	jne	.L4518
	movq	-984(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %rax
	movq	%rax, -1000(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4523
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-992(%rbp), %r8
	cmpb	$1, 16(%r8)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	-992(%rbp), %r9
	movq	32(%rbx), %rdx
	movq	-992(%rbp), %rsi
	movq	32(%r9), %rcx
	movl	24(%rsi), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-984(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-1000(%rbp), %r11
	movq	%r11, 8(%rax)
	jmp	.L4518
.L4523:
	movq	-992(%rbp), %rdx
	movq	-984(%rbp), %rax
	movq	%rdx, 72(%rax)
	jmp	.L4518
.L11924:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4521
.L11923:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4515
	jmp	.L11087
	.p2align 6,,7
.L11907:
	cmpq	$0, class_binding_level(%rip)
	je	.L4436
	movq	144(%rbx), %rsi
	testb	$16, 3(%rsi)
	jne	.L4435
	jmp	.L4436
	.p2align 6,,7
.L11906:
	movq	8(%rcx), %rdx
	movq	-952(%rbp), %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	-2320(%rbp), %rdx
	movq	%rax, 8(%rdx)
	jmp	.L4432
	.p2align 6,,7
.L4403:
	movq	-2312(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10553
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L4408
	testb	$8, 18(%r14)
	je	.L4408
	testb	$8, 18(%r13)
	jne	.L4408
	testb	$9, 53(%r13)
	jne	.L4408
	cmpq	%r13, current_function_decl(%rip)
	je	.L11926
.L4412:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L4413
	cmpq	$0, 8(%rax)
	jne	.L11927
.L4413:
	movq	-2312(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11083:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2312(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L4408
.L11927:
	movq	-2312(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11083
.L11926:
	movq	-2312(%rbp), %rdi
	movq	%rdi, current_function_decl(%rip)
	jmp	.L4412
	.p2align 6,,7
.L11285:
	cmpq	$0, 64(%rcx)
	jne	.L4401
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L4401
.L11284:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2312(%rbp)
	call	error_with_decl
	jmp	.L4399
.L4392:
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.L4394
	.p2align 4,,7
.L4398:
	cmpq	%r14, 56(%rdi)
	je	.L4394
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.L4398
.L4394:
	movq	%rdi, -2312(%rbp)
	jmp	.L4391
.L11283:
	movq	40(%r14), %r8
	movq	%r8, -2312(%rbp)
	jmp	.L4391
.L11282:
	movq	56(%r13), %r14
	jmp	.L4388
.L11281:
	testb	$32, 53(%r13)
	jne	.L4386
	jmp	.L4387
.L10549:
	movzbl	16(%r13), %edx
	jmp	.L4386
.L11280:
	leal	(%rcx,%rcx), %r10d
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%r10d,%rsi
	movl	%r10d, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L4382
.L4372:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4373
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11081
.L4371:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11928
.L11082:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4378:
	movq	$0, 8
	jmp	.L4370
.L11928:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4378
	jmp	.L11082
	.p2align 6,,7
.L11279:
	movq	-2264(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_binding_level(%rip), %rax
	movq	%rax, -2296(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10530
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11929
.L4090:
	movq	%rax, 64(%r13)
.L4089:
	cmpb	$32, %dl
	je	.L11930
.L4091:
	testq	%r15, %r15
	je	.L4092
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11931
	cmpq	$0, 48(%r15)
	jne	.L4095
	movq	$0, -2304(%rbp)
.L4094:
	cmpq	$0, -2304(%rbp)
	je	.L4112
	movq	-2304(%rbp), %r8
	cmpq	error_mark_node(%rip), %r8
	je	.L11932
.L4102:
	cmpq	$0, -2304(%rbp)
	je	.L10534
	movq	-2304(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11933
.L4104:
	movq	-2304(%rbp), %r11
	testq	%r11, %r11
	movq	24(%r11), %r12
	movq	%r11, %rsi
	movl	32(%r11), %ebx
	je	.L10534
	movzbl	16(%r11), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L4106
	cmpb	$32, %al
	je	.L4112
	cmpb	$32, %dl
	je	.L10910
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10534
.L10538:
	movq	global_binding_level(%rip), %rax
.L4111:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L4365
	movq	-2264(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11080:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4366:
	movq	-2264(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L4088
	movq	-2304(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L4088
.L4365:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4366
	movq	-2264(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L11080
.L10534:
	movzbl	16(%r13), %edx
.L4112:
	cmpb	$32, %dl
	je	.L10910
.L4120:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L4258
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L4258
	testb	$1, 53(%rax)
	jne	.L4259
	testb	$8, 18(%rax)
	je	.L4258
.L4259:
	andb	$8, %dl
	je	.L11934
.L4258:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10546
	testb	$1, 53(%r13)
	je	.L10546
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L4262
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L4263
.L4262:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4264
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10099
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L4265
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L11935
.L4265:
	testq	%rcx, %rcx
	jne	.L10099
.L10100:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10099
.L4264:
	movq	40(%r15), %rcx
.L4263:
	testq	%rcx, %rcx
	je	.L10244
.L10099:
	cmpb	$32, 16(%rcx)
	je	.L4267
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L4267
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L4275
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11077
	testl	%ebx, %ebx
	jle	.L11936
.L11077:
	movq	%rax, %rcx
.L4267:
	testq	%rcx, %rcx
	jne	.L10546
.L10244:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2296(%rbp)
.L4261:
	cmpq	%rax, -2296(%rbp)
	je	.L11937
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L11938
.L11078:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L4306:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L11939
.L4317:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L4319
	testq	%r12, %r12
	je	.L4320
	testb	$1, 53(%r13)
	jne	.L4320
	cmpb	$34, 16(%r12)
	je	.L11940
.L4320:
	movl	warn_shadow(%rip), %edx
	testl	%edx, %edx
	je	.L4319
	testb	$1, 53(%r13)
	jne	.L4319
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L4319
	testq	%rax, %rax
	jne	.L4319
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L4325
	cmpb	$34, 16(%r12)
	je	.L11941
.L4325:
	cmpq	$0, 56(%r15)
	je	.L4327
	movl	$.LC41, %edi
.L4326:
	testq	%rdi, %rdi
	jne	.L11079
.L4319:
	testq	%r12, %r12
	je	.L10547
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2296(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10547:
	movzbl	16(%r13), %edx
.L4304:
	leal	-128(%rdx), %r9d
	cmpb	$1, %r9b
	jbe	.L4092
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L4092
	cmpb	$18, 16(%rcx)
	je	.L11942
.L4336:
	testb	$64, 46(%rcx)
	je	.L4092
.L4335:
	movq	-2296(%rbp), %rcx
	movzwl	64(%rcx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rcx)
	je	.L11943
.L10548:
	movzbl	16(%r13), %edx
.L4092:
	cmpb	$32, %dl
	je	.L11944
.L4338:
	movq	-2296(%rbp), %rdi
	movq	global_binding_level(%rip), %rax
	movq	(%rdi), %rdx
	cmpq	%rax, %rdi
	movq	%rdx, (%r13)
	movq	%r13, (%rdi)
	je	.L11945
.L4364:
	movq	%r13, -2304(%rbp)
	jmp	.L4111
.L11945:
	testb	$4, 17(%r13)
	jne	.L4364
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L4364
.L11944:
	testq	%r15, %r15
	je	.L4338
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4339
	cmpq	class_binding_level(%rip), %rax
	je	.L4340
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L4344
	cmpb	$32, 16(%rax)
	je	.L4342
.L4344:
	cmpq	$0, current_class_type(%rip)
	je	.L4339
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L4339
	cmpb	$32, 16(%rax)
	je	.L4342
.L4339:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L4343
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4342
	cmpb	$-127, %dl
	je	.L11946
.L4343:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L4338
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11947
.L4350:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4354
	cmpq	class_binding_level(%rip), %rax
	je	.L4355
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L4359
	cmpb	$32, 16(%rax)
	je	.L4357
.L4359:
	cmpq	$0, current_class_type(%rip)
	je	.L4354
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L4354
	cmpb	$32, 16(%rax)
	je	.L4357
.L4354:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L4338
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4357
	cmpb	$-127, %dl
	jne	.L4338
	movq	$0, 8(%rbx)
	jmp	.L4338
.L4357:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L4338
.L4355:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4359
.L11947:
	cmpb	$32, 16(%r13)
	movq	56(%r13), %rbx
	sete	%sil
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L4350
.L11946:
	movq	$0, 8(%r15)
	jmp	.L4343
.L4342:
	movq	8(%rax), %r10
	movq	%r10, 8(%r15)
	jmp	.L4343
.L4340:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4344
.L11943:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10548
.L11942:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L4335
	jmp	.L4336
.L11079:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L4319
.L4327:
	testq	%r12, %r12
	je	.L4329
	movl	$.LC42, %edi
	jmp	.L4326
.L4329:
	testq	%r8, %r8
	movl	$.LC43, %r11d
	cmovne	%r11, %rdi
	jmp	.L4326
.L11941:
	movl	$.LC40, %edi
	jmp	.L4326
.L11940:
	cmpb	$34, 16(%r13)
	je	.L4320
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L4321
	movq	56(%rax), %rax
.L4321:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L4319
	movl	$.LC40, %edi
	jmp	.L11079
	.p2align 6,,7
.L11939:
	movzbl	53(%r13), %r9d
	andb	$9, %r9b
	decb	%r9b
	je	.L11948
.L4309:
	testq	%r12, %r12
	jne	.L4317
	testq	%r8, %r8
	jne	.L4317
	testb	$1, 53(%r13)
	je	.L4317
	testb	$8, 18(%r13)
	je	.L4317
	orb	$8, 18(%r15)
	jmp	.L4317
	.p2align 6,,7
.L11948:
	testq	%r8, %r8
	je	.L4309
	cmpb	$29, 16(%r13)
	jne	.L4309
	cmpb	$29, 16(%r8)
	jne	.L4309
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L11949
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L4312
	movzbl	53(%r13), %ebx
	leal	0(,%rax,8), %ecx
	leaq	88(%r13), %rdx
	andb	$-9, %bl
	orb	%cl, %bl
	movb	%bl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L4313
	movq	88(%r8), %rax
.L4314:
	movq	136(%r8), %rbx
	movq	72(%r8), %r9
	movq	%rax, (%rdx)
	movq	80(%r8), %r11
	movzbl	17(%r13), %r10d
	movq	%rbx, 136(%r13)
	movq	%r9, 72(%r13)
	movq	%r11, 80(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %r10b
	shrb	$7, %dil
	movzbl	%dil, %edx
	movl	%edx, %esi
	salb	$7, %sil
	orb	%sil, %r10b
	movb	%r10b, 17(%r13)
	movzbl	53(%r8), %ecx
.L4312:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L4315
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L4315:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L4309
	cmpq	$0, 88(%r8)
	je	.L4309
	movq	8(%r13), %r10
	cmpq	$0, 24(%r10)
	jne	.L4309
	movq	%rdx, 8(%r13)
	jmp	.L4309
.L4313:
	xorl	%eax, %eax
	jmp	.L4314
.L11949:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L4309
	.p2align 6,,7
.L11938:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4306
	jmp	.L11078
.L11937:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11950
.L4281:
	cmpq	$0, 40(%r15)
	jne	.L4282
	testb	$8, 18(%r13)
	je	.L4282
	orb	$8, 18(%r15)
.L4282:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11951
.L4284:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L4283:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L4295
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4285
	testb	$1, 18(%rcx)
	je	.L4285
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L4285:
	testq	%rax, %rax
	je	.L4295
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L4290
	testb	$8, 17(%rcx)
	je	.L4290
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L4290:
	testq	%rax, %rax
	je	.L4295
	cmpq	$0, 8(%rax)
	je	.L4295
	cmpb	$29, %dl
	je	.L11952
.L4298:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L4295:
	testb	$8, 18(%r15)
	je	.L4304
	cmpb	$32, %dl
	je	.L4304
	testb	$8, 18(%r13)
	jne	.L4304
	testb	$1, 53(%r13)
	jne	.L4304
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L4300
	cmpq	$0, 8(%rax)
	jne	.L11953
.L4300:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11236:
	xorl	%eax, %eax
	call	warning
	jmp	.L10547
.L11953:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11236
.L11952:
	movq	8(%r13), %r11
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r11)
	jne	.L4298
	jmp	.L4295
	.p2align 6,,7
.L11951:
	cmpq	$0, -2304(%rbp)
	je	.L4284
	movq	-2304(%rbp), %r8
	cmpb	$32, 16(%r8)
	jne	.L4283
	jmp	.L4284
.L11950:
	testb	$8, 54(%r13)
	jne	.L4281
	andb	$-9, 18(%r13)
	jmp	.L4281
	.p2align 6,,7
.L10546:
	movq	global_binding_level(%rip), %rax
	jmp	.L4261
.L11936:
	testl	%esi, %esi
	jg	.L11077
	testl	%ebx, %ebx
	je	.L4267
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11077
	.p2align 6,,7
.L4275:
	movq	8(%rcx), %rdx
	cmpq	error_mark_node(%rip), %rdx
	cmove	%rdx, %rcx
	jmp	.L4267
.L11935:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4267
	testq	%rax, %rax
	je	.L10100
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L4265
	.p2align 6,,7
.L11934:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L4258
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L4258
	.p2align 6,,7
.L10910:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2288(%rbp)
	je	.L4122
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L4121
.L4122:
	movq	global_binding_level(%rip), %r9
	movq	%r13, -2288(%rbp)
	cmpq	%r9, current_binding_level(%rip)
	jne	.L10539
	movq	%r13, 80(%rdx)
.L10539:
	movzbl	16(%r13), %eax
.L4125:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2288(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L4249
	cmpq	$0, 72(%rax)
	je	.L11954
.L4249:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L4120
	cmpq	$0, 56(%rax)
	je	.L4120
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -944(%rbp)
	je	.L4254
	movq	-944(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
.L11076:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4255:
	movq	-944(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L4120
.L4254:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4255
	movq	-944(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11076
.L11954:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -936(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L4250
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-936(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L4249
.L4250:
	movq	%rbx, 72(%r13)
	jmp	.L4249
.L4121:
	movq	-2288(%rbp), %rdi
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rdi), %r12
	movq	%r12, -896(%rbp)
	jne	.L4125
	movq	-896(%rbp), %r10
	movq	32(%r10), %rcx
	cmpb	$36, (%rcx)
	jne	.L4125
	cmpb	$95, 1(%rcx)
	jne	.L4125
	movq	class_binding_level(%rip), %rbx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rcx
	testq	%rbx, %rbx
	movq	%rbx, -2272(%rbp)
	movq	%rcx, -904(%rbp)
	jne	.L4129
	testb	$-128, 66(%rsi)
	movq	%rsi, -2272(%rbp)
	je	.L4129
.L4133:
	movq	-2272(%rbp), %rax
	movq	56(%rax), %rdx
	testb	$-128, 66(%rdx)
	movq	%rdx, -2272(%rbp)
	jne	.L4133
.L4129:
	movq	-2272(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11955
	movq	-2272(%rbp), %r11
	movq	-896(%rbp), %rdi
	xorl	%eax, %eax
	movq	-904(%rbp), %rsi
	movq	8(%r11), %rdx
	call	saveable_tree_cons
	movq	-2272(%rbp), %r8
	movq	%rax, 8(%r8)
.L4135:
	cmpq	$0, -896(%rbp)
	je	.L4136
	movq	-904(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L4137
	movq	-896(%rbp), %rsi
	movq	%rsi, 80(%rcx)
.L4137:
	movq	-896(%rbp), %r9
	movq	-904(%rbp), %rax
	cmpq	%rax, 8(%r9)
	je	.L4138
	cmpb	$21, 16(%rax)
	je	.L11956
.L4139:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L4141
	cmpq	$0, 32(%rax)
	je	.L4140
.L4141:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L11957
.L4142:
	xorl	%ecx, %ecx
.L4177:
	testq	%rcx, %rcx
	jne	.L4178
.L10243:
	movq	-896(%rbp), %rsi
	movq	-904(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-904(%rbp), %rdi
	movq	%rax, -2280(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2280(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L4179
	movq	-896(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L11073:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4180:
	movq	-904(%rbp), %r8
	movq	-896(%rbp), %rdx
	movq	%r8, 8(%rdx)
.L4183:
	movq	-896(%rbp), %r11
	movq	32(%r11), %rax
	cmpb	$36, (%rax)
	je	.L11958
.L4185:
	movq	-904(%rbp), %rsi
	movq	80(%rsi), %rdx
	testq	%rdx, %rdx
	je	.L4216
	cmpb	$32, 16(%rdx)
	je	.L11959
.L4186:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4205
	movq	-2280(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10541
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L4207
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4207:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2280(%rbp), %rdi
	leaq	8(%rdx), %r9
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L11960
.L4209:
	movq	-2280(%rbp), %rbx
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r10)
	cmpb	$32, 16(%rbx)
	je	.L11961
.L10542:
	movq	-896(%rbp), %rbx
	movq	32(%rbx), %rax
.L4216:
	cmpb	$36, (%rax)
	je	.L11962
.L4230:
	movq	current_class_type(%rip), %rdx
	movq	-2280(%rbp), %rsi
	movq	-904(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rsi, 80(%rcx)
	jne	.L4233
	cmpq	$0, current_function_decl(%rip)
	je	.L4232
.L4233:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L4231
.L4232:
	movq	-896(%rbp), %rdi
	movq	-2280(%rbp), %r10
	movq	%rdi, 72(%r10)
.L4138:
	movq	-2272(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L11963
.L4136:
	movq	-904(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L11964
	movq	-904(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-904(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2288(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L4125
.L11964:
	movq	%rax, (%rdx)
	movq	-2288(%rbp), %r11
	movzbl	16(%r11), %eax
	jmp	.L4125
.L11963:
	movq	-904(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-896(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L4136
	movq	-2272(%rbp), %r8
	movq	144(%rax), %r12
	movq	8(%r8), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L4136
.L4231:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L11965
	cmpq	$0, 32(%rdx)
	jne	.L4138
	movq	-2280(%rbp), %r11
	movq	80(%rdx), %r9
	movl	$136, %esi
	cmpb	$32, 16(%r11)
	movq	72(%r9), %rbx
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4241
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	-896(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-896(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2280(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-904(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L4243:
	movq	-2280(%rbp), %rbx
	movq	current_class_type(%rip), %r9
	movq	152(%rbx), %r11
	movq	%r9, 64(%rbx)
	movq	%r9, 16(%r11)
	jmp	.L4138
.L4241:
	movq	-896(%rbp), %r8
	movq	-2280(%rbp), %rdx
	movq	%r8, 72(%rdx)
	jmp	.L4243
.L11965:
	movq	-2280(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4236
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-896(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-896(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2280(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-904(%rbp), %r8
	movq	%r8, 8(%rax)
.L4238:
	movq	current_function_decl(%rip), %rdx
	movq	-2280(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L4138
.L4236:
	movq	-896(%rbp), %rdi
	movq	-2280(%rbp), %r10
	movq	%rdi, 72(%r10)
	jmp	.L4238
.L11962:
	cmpb	$95, 1(%rax)
	jne	.L4230
	movq	-2280(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L4230
.L11961:
	cmpq	$0, 72(%rbx)
	je	.L11966
.L10543:
	movq	-896(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L4216
.L11966:
	movq	-2280(%rbp), %r8
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r8), %rdx
	movq	%rdx, -912(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4211
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2280(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-912(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10540:
	movq	-896(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L4216
.L4211:
	movq	-2280(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10542
.L11960:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4209
.L10541:
	movq	-896(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L4216
.L4205:
	movq	-2280(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2280(%rbp)
	jmp	.L10543
.L11959:
	movq	global_binding_level(%rip), %r9
	movl	$1, %r12d
	cmpq	%r9, current_binding_level(%rip)
	je	.L4187
	movq	-896(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L4188
.L4187:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4189
	movq	-896(%rbp), %r10
	movq	56(%r10), %rcx
	testq	%rcx, %rcx
	jne	.L10097
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L4190
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L11967
.L4190:
	testq	%rcx, %rcx
	jne	.L10097
.L10098:
	movq	-896(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10097
	movq	-896(%rbp), %r8
	movq	40(%r8), %rcx
.L4188:
	testq	%rcx, %rcx
	je	.L4192
.L10097:
	cmpb	$32, 16(%rcx)
	je	.L4192
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4192
	movq	-896(%rbp), %r11
	movq	8(%r11), %rax
	testq	%rax, %rax
	je	.L4200
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11074
	testl	%r12d, %r12d
	jle	.L11968
.L11074:
	movq	%rax, %rcx
.L4192:
	movq	-904(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L4186
	jmp	.L10540
.L11968:
	testl	%edx, %edx
	jg	.L11074
	testl	%r12d, %r12d
	je	.L4192
	movq	-896(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11074
.L4200:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L4192
.L11967:
	xorl	%ecx, %ecx
	movq	-896(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4192
	testq	%rax, %rax
	je	.L10098
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L4190
.L4189:
	movq	-896(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L4188
.L11958:
	cmpb	$95, 1(%rax)
	jne	.L4185
	jmp	.L4216
.L4179:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4180
	movq	-896(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11073
.L4178:
	movq	80(%rcx), %rax
	movq	%rax, -2280(%rbp)
	jmp	.L4183
.L11957:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L4144
	movq	80(%rax), %rbx
.L4144:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4177
.L4176:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L4153
	cmpl	$32, %eax
	je	.L11969
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L4147:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L4177
	jmp	.L4176
.L11969:
	movq	8(%rbx), %rdx
	movq	-904(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10884
	movq	64(%rbx), %rbx
	jmp	.L4147
.L10884:
	movq	32(%rax), %rcx
	jmp	.L4177
.L4153:
	movq	-904(%rbp), %rax
	movq	80(%rax), %r8
	movq	56(%r8), %rbx
	testq	%rbx, %rbx
	je	.L4142
	movq	global_binding_level(%rip), %r11
	cmpq	%r11, current_binding_level(%rip)
	je	.L4156
	movq	48(%rbx), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L4157
.L4156:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L4158
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10095
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L4159
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L11970
.L4159:
	testq	%rcx, %rcx
	jne	.L10095
.L10096:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10095
.L4158:
	movq	40(%rbx), %rcx
.L4157:
	testq	%rcx, %rcx
	je	.L10243
.L10095:
	cmpb	$32, 16(%rcx)
	je	.L4177
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L4177
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L4169
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11072
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L11971
.L11072:
	movq	%rax, %rcx
	jmp	.L4177
.L11971:
	testl	%edx, %edx
	jg	.L11072
	movl	$1, %edi
	testl	%edi, %edi
	je	.L4177
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11072
.L4169:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L4177
	jmp	.L11072
.L11970:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L4177
	testq	%rax, %rax
	je	.L10096
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L4159
.L4140:
	movq	-896(%rbp), %rsi
	movq	-904(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -920(%rbp)
	je	.L4217
	movq	-896(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11075:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4218:
	movq	-904(%rbp), %rdx
	movq	-896(%rbp), %r10
	movq	%rdx, 8(%r10)
	movq	-920(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L4221
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L4222
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L4222:
	movq	decl_obstack+24(%rip), %rdx
	movq	-920(%rbp), %rbx
	leaq	8(%rdx), %r8
	movq	%rbx, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r8
	ja	.L11972
.L4224:
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-920(%rbp), %rdx
	movq	%rdx, (%r11)
	cmpb	$32, 16(%rdx)
	je	.L11973
.L4221:
	movq	-920(%rbp), %r12
	movq	%r12, -2280(%rbp)
	jmp	.L10543
.L11973:
	cmpq	$0, 72(%rdx)
	jne	.L4221
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -928(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L4226
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-920(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-928(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L4221
.L4226:
	movq	-920(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L4221
.L11972:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L4224
.L4217:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4218
	movq	-896(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11075
.L11956:
	cmpq	$0, class_binding_level(%rip)
	je	.L4139
	movq	144(%rax), %r12
	testb	$16, 3(%r12)
	jne	.L4138
	jmp	.L4139
.L11955:
	movq	-896(%rbp), %rdi
	movq	-904(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L4135
	.p2align 6,,7
.L4106:
	movq	-2304(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10534
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L10538
	testb	$8, 18(%r15)
	je	.L10538
	testb	$8, 18(%r13)
	jne	.L10538
	testb	$9, 53(%r13)
	jne	.L10538
	cmpq	%r13, current_function_decl(%rip)
	je	.L11974
.L4115:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L4116
	cmpq	$0, 8(%rax)
	jne	.L11975
.L4116:
	movq	-2304(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11071:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2304(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10538
.L11975:
	movq	-2304(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11071
.L11974:
	movq	-2304(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L4115
	.p2align 6,,7
.L11933:
	cmpq	$0, 64(%rcx)
	jne	.L4104
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L4104
.L11932:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2304(%rbp)
	call	error_with_decl
	jmp	.L4102
.L4095:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L4097
.L4101:
	cmpq	%r15, 56(%rax)
	je	.L4097
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L4101
.L4097:
	movq	%rax, -2304(%rbp)
	jmp	.L4094
.L11931:
	movq	40(%r15), %rax
	jmp	.L4097
.L11930:
	movq	56(%r13), %r15
	jmp	.L4091
.L11929:
	testb	$32, 53(%r13)
	jne	.L4089
	jmp	.L4090
.L10530:
	movzbl	16(%r13), %edx
	jmp	.L4089
.L11278:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L4082
.L4072:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4073
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11069
.L4071:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L11976
.L11070:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4078:
	movq	$0, 8
	jmp	.L4070
.L11976:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4078
	jmp	.L11070
	.p2align 6,,7
.L11277:
	movq	-2216(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r11
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r11, -2248(%rbp)
	cmpq	%rax, %r13
	je	.L10511
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L11977
.L3790:
	movq	%rax, 64(%r13)
.L3789:
	cmpb	$32, %dl
	je	.L11978
.L3791:
	testq	%r15, %r15
	je	.L3792
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L11979
	cmpq	$0, 48(%r15)
	jne	.L3795
	movq	$0, -2256(%rbp)
.L3794:
	cmpq	$0, -2256(%rbp)
	je	.L3812
	movq	-2256(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L11980
.L3802:
	cmpq	$0, -2256(%rbp)
	je	.L10515
	movq	-2256(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L11981
.L3804:
	movq	-2256(%rbp), %r8
	testq	%r8, %r8
	movq	24(%r8), %r12
	movq	%r8, %rsi
	movl	32(%r8), %ebx
	je	.L10515
	movzbl	16(%r8), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L3806
	cmpb	$32, %al
	je	.L3812
	cmpb	$32, %dl
	je	.L10909
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10515
.L10519:
	movq	global_binding_level(%rip), %rax
.L3811:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L4065
	movq	-2216(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11068:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L4066:
	movq	-2216(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L3788
	movq	-2256(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L3788
.L4065:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4066
	movq	-2216(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11068
.L10515:
	movzbl	16(%r13), %edx
.L3812:
	cmpb	$32, %dl
	je	.L10909
.L3820:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L3958
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L3958
	testb	$1, 53(%rax)
	jne	.L3959
	testb	$8, 18(%rax)
	je	.L3958
.L3959:
	andb	$8, %dl
	je	.L11982
.L3958:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10527
	testb	$1, 53(%r13)
	je	.L10527
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L3962
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L3963
.L3962:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3964
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10093
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3965
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L11983
.L3965:
	testq	%rcx, %rcx
	jne	.L10093
.L10094:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10093
.L3964:
	movq	40(%r15), %rcx
.L3963:
	testq	%rcx, %rcx
	je	.L10242
.L10093:
	cmpb	$32, 16(%rcx)
	je	.L3967
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L3967
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L3975
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11065
	testl	%ebx, %ebx
	jle	.L11984
.L11065:
	movq	%rax, %rcx
.L3967:
	testq	%rcx, %rcx
	jne	.L10527
.L10242:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2248(%rbp)
.L3961:
	cmpq	%rax, -2248(%rbp)
	je	.L11985
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L11986
.L11066:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L4006:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L11987
.L4017:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L4019
	testq	%r12, %r12
	je	.L4020
	testb	$1, 53(%r13)
	jne	.L4020
	cmpb	$34, 16(%r12)
	je	.L11988
.L4020:
	movl	warn_shadow(%rip), %edx
	testl	%edx, %edx
	je	.L4019
	testb	$1, 53(%r13)
	jne	.L4019
	movl	32(%r13), %r11d
	testl	%r11d, %r11d
	je	.L4019
	testq	%rax, %rax
	jne	.L4019
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L4025
	cmpb	$34, 16(%r12)
	je	.L11989
.L4025:
	cmpq	$0, 56(%r15)
	je	.L4027
	movl	$.LC41, %edi
.L4026:
	testq	%rdi, %rdi
	jne	.L11067
.L4019:
	testq	%r12, %r12
	je	.L10528
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2248(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10528:
	movzbl	16(%r13), %edx
.L4004:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L3792
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L3792
	cmpb	$18, 16(%rcx)
	je	.L11990
.L4036:
	testb	$64, 46(%rcx)
	je	.L3792
.L4035:
	movq	-2248(%rbp), %rcx
	movzwl	64(%rcx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rcx)
	je	.L11991
.L10529:
	movzbl	16(%r13), %edx
.L3792:
	cmpb	$32, %dl
	je	.L11992
.L4038:
	movq	-2248(%rbp), %r11
	movq	global_binding_level(%rip), %rax
	movq	(%r11), %rdx
	cmpq	%rax, %r11
	movq	%rdx, (%r13)
	movq	%r13, (%r11)
	je	.L11993
.L4064:
	movq	%r13, -2256(%rbp)
	jmp	.L3811
.L11993:
	testb	$4, 17(%r13)
	jne	.L4064
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L4064
.L11992:
	testq	%r15, %r15
	je	.L4038
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4039
	cmpq	class_binding_level(%rip), %rax
	je	.L4040
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L4044
	cmpb	$32, 16(%rax)
	je	.L4042
.L4044:
	cmpq	$0, current_class_type(%rip)
	je	.L4039
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L4039
	cmpb	$32, 16(%rax)
	je	.L4042
.L4039:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L4043
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4042
	cmpb	$-127, %dl
	je	.L11994
.L4043:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L4038
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L11995
.L4050:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L4054
	cmpq	class_binding_level(%rip), %rax
	je	.L4055
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L4059
	cmpb	$32, 16(%rax)
	je	.L4057
.L4059:
	cmpq	$0, current_class_type(%rip)
	je	.L4054
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L4054
	cmpb	$32, 16(%rax)
	je	.L4057
.L4054:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L4038
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L4057
	cmpb	$-127, %dl
	jne	.L4038
	movq	$0, 8(%rbx)
	jmp	.L4038
.L4057:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L4038
.L4055:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4059
.L11995:
	cmpb	$32, 16(%r13)
	movq	56(%r13), %rbx
	sete	%sil
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L4050
.L11994:
	movq	$0, 8(%r15)
	jmp	.L4043
.L4042:
	movq	8(%rax), %r10
	movq	%r10, 8(%r15)
	jmp	.L4043
.L4040:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L4044
.L11991:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10529
.L11990:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L4035
	jmp	.L4036
.L11067:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L4019
.L4027:
	testq	%r12, %r12
	je	.L4029
	movl	$.LC42, %edi
	jmp	.L4026
.L4029:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L4026
.L11989:
	movl	$.LC40, %edi
	jmp	.L4026
.L11988:
	cmpb	$34, 16(%r13)
	je	.L4020
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L4021
	movq	56(%rax), %rax
.L4021:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L4019
	movl	$.LC40, %edi
	jmp	.L11067
	.p2align 6,,7
.L11987:
	movzbl	53(%r13), %r9d
	andb	$9, %r9b
	decb	%r9b
	je	.L11996
.L4009:
	testq	%r12, %r12
	jne	.L4017
	testq	%r8, %r8
	jne	.L4017
	testb	$1, 53(%r13)
	je	.L4017
	testb	$8, 18(%r13)
	je	.L4017
	orb	$8, 18(%r15)
	jmp	.L4017
	.p2align 6,,7
.L11996:
	testq	%r8, %r8
	je	.L4009
	cmpb	$29, 16(%r13)
	jne	.L4009
	cmpb	$29, 16(%r8)
	jne	.L4009
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L11997
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L4012
	movzbl	53(%r13), %ebx
	leal	0(,%rax,8), %ecx
	leaq	88(%r13), %rdx
	andb	$-9, %bl
	orb	%cl, %bl
	movb	%bl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L4013
	movq	88(%r8), %rax
.L4014:
	movq	136(%r8), %rbx
	movq	72(%r8), %r9
	movq	%rax, (%rdx)
	movq	80(%r8), %rdi
	movzbl	17(%r13), %r10d
	movq	%rbx, 136(%r13)
	movq	%r9, 72(%r13)
	movq	%rdi, 80(%r13)
	movzbl	17(%r8), %r11d
	movq	%r8, 96(%r13)
	andb	$127, %r10b
	shrb	$7, %r11b
	movzbl	%r11b, %edx
	movl	%edx, %esi
	salb	$7, %sil
	orb	%sil, %r10b
	movb	%r10b, 17(%r13)
	movzbl	53(%r8), %ecx
.L4012:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L4015
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L4015:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L4009
	cmpq	$0, 88(%r8)
	je	.L4009
	movq	8(%r13), %r10
	cmpq	$0, 24(%r10)
	jne	.L4009
	movq	%rdx, 8(%r13)
	jmp	.L4009
.L4013:
	xorl	%eax, %eax
	jmp	.L4014
.L11997:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L4009
	.p2align 6,,7
.L11986:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L4006
	jmp	.L11066
.L11985:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L11998
.L3981:
	cmpq	$0, 40(%r15)
	jne	.L3982
	testb	$8, 18(%r13)
	je	.L3982
	orb	$8, 18(%r15)
.L3982:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L11999
.L3984:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L3983:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L3995
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3985
	testb	$1, 18(%rcx)
	je	.L3985
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L3985:
	testq	%rax, %rax
	je	.L3995
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3990
	testb	$8, 17(%rcx)
	je	.L3990
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L3990:
	testq	%rax, %rax
	je	.L3995
	cmpq	$0, 8(%rax)
	je	.L3995
	cmpb	$29, %dl
	je	.L12000
.L3998:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L3995:
	testb	$8, 18(%r15)
	je	.L4004
	cmpb	$32, %dl
	je	.L4004
	testb	$8, 18(%r13)
	jne	.L4004
	testb	$1, 53(%r13)
	jne	.L4004
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L4000
	cmpq	$0, 8(%rax)
	jne	.L12001
.L4000:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11235:
	xorl	%eax, %eax
	call	warning
	jmp	.L10528
.L12001:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11235
.L12000:
	movq	8(%r13), %r8
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r8)
	jne	.L3998
	jmp	.L3995
	.p2align 6,,7
.L11999:
	cmpq	$0, -2256(%rbp)
	je	.L3984
	movq	-2256(%rbp), %r11
	cmpb	$32, 16(%r11)
	jne	.L3983
	jmp	.L3984
.L11998:
	testb	$8, 54(%r13)
	jne	.L3981
	andb	$-9, 18(%r13)
	jmp	.L3981
	.p2align 6,,7
.L10527:
	movq	global_binding_level(%rip), %rax
	jmp	.L3961
.L11984:
	testl	%esi, %esi
	jg	.L11065
	testl	%ebx, %ebx
	je	.L3967
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11065
	.p2align 6,,7
.L3975:
	movq	8(%rcx), %rdx
	cmpq	error_mark_node(%rip), %rdx
	cmove	%rdx, %rcx
	jmp	.L3967
.L11983:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3967
	testq	%rax, %rax
	je	.L10094
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3965
	.p2align 6,,7
.L11982:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L3958
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L3958
	.p2align 6,,7
.L10909:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2240(%rbp)
	je	.L3822
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L3821
.L3822:
	movq	global_binding_level(%rip), %rcx
	movq	%r13, -2240(%rbp)
	cmpq	%rcx, current_binding_level(%rip)
	jne	.L10520
	movq	%r13, 80(%rdx)
.L10520:
	movzbl	16(%r13), %eax
.L3825:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2240(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L3949
	cmpq	$0, 72(%rax)
	je	.L12002
.L3949:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L3820
	cmpq	$0, 56(%rax)
	je	.L3820
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -888(%rbp)
	je	.L3954
	movq	-888(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L11064:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3955:
	movq	-888(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L3820
.L3954:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3955
	movq	-888(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11064
.L12002:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -880(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L3950
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-880(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L3949
.L3950:
	movq	%rbx, 72(%r13)
	jmp	.L3949
.L3821:
	movq	-2240(%rbp), %r12
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r12), %rbx
	movq	%rbx, -840(%rbp)
	jne	.L3825
	movq	-840(%rbp), %rdi
	movq	32(%rdi), %rcx
	cmpb	$36, (%rcx)
	jne	.L3825
	cmpb	$95, 1(%rcx)
	jne	.L3825
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %r10
	testq	%r9, %r9
	movq	%r9, -2224(%rbp)
	movq	%r10, -848(%rbp)
	jne	.L3829
	testb	$-128, 66(%rsi)
	movq	%rsi, -2224(%rbp)
	je	.L3829
.L3833:
	movq	-2224(%rbp), %r11
	movq	56(%r11), %rdx
	testb	$-128, 66(%rdx)
	movq	%rdx, -2224(%rbp)
	jne	.L3833
.L3829:
	movq	-2224(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12003
	movq	-2224(%rbp), %rcx
	movq	-840(%rbp), %rdi
	xorl	%eax, %eax
	movq	-848(%rbp), %rsi
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-2224(%rbp), %r8
	movq	%rax, 8(%r8)
.L3835:
	cmpq	$0, -840(%rbp)
	je	.L3836
	movq	-848(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L3837
	movq	-840(%rbp), %rax
	movq	%rax, 80(%rcx)
.L3837:
	movq	-840(%rbp), %rbx
	movq	-848(%rbp), %rax
	cmpq	%rax, 8(%rbx)
	je	.L3838
	cmpb	$21, 16(%rax)
	je	.L12004
.L3839:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L3841
	cmpq	$0, 32(%rax)
	je	.L3840
.L3841:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L12005
.L3842:
	xorl	%ecx, %ecx
.L3877:
	testq	%rcx, %rcx
	jne	.L3878
.L10241:
	movq	-840(%rbp), %rsi
	movq	-848(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-848(%rbp), %rdi
	movq	%rax, -2232(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2232(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3879
	movq	-840(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L11061:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3880:
	movq	-848(%rbp), %rdx
	movq	-840(%rbp), %r10
	movq	%rdx, 8(%r10)
.L3883:
	movq	-840(%rbp), %r8
	movq	32(%r8), %rax
	cmpb	$36, (%rax)
	je	.L12006
.L3885:
	movq	-848(%rbp), %rsi
	movq	80(%rsi), %rdx
	testq	%rdx, %rdx
	je	.L3916
	cmpb	$32, 16(%rdx)
	je	.L12007
.L3886:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3905
	movq	-2232(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10522
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3907
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3907:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2232(%rbp), %rdi
	leaq	8(%rdx), %r9
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L12008
.L3909:
	movq	-2232(%rbp), %rbx
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r10)
	cmpb	$32, 16(%rbx)
	je	.L12009
.L10523:
	movq	-840(%rbp), %r11
	movq	32(%r11), %rax
.L3916:
	cmpb	$36, (%rax)
	je	.L12010
.L3930:
	movq	current_class_type(%rip), %rdx
	movq	-2232(%rbp), %rsi
	movq	-848(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rsi, 80(%rcx)
	jne	.L3933
	cmpq	$0, current_function_decl(%rip)
	je	.L3932
.L3933:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L3931
.L3932:
	movq	-840(%rbp), %rdi
	movq	-2232(%rbp), %r10
	movq	%rdi, 72(%r10)
.L3838:
	movq	-2224(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12011
.L3836:
	movq	-848(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12012
	movq	-848(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-848(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2240(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L3825
.L12012:
	movq	%rax, (%rdx)
	movq	-2240(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L3825
.L12011:
	movq	-848(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-840(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L3836
	movq	-2224(%rbp), %r11
	movq	144(%rax), %r12
	movq	8(%r11), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L3836
.L3931:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12013
	cmpq	$0, 32(%rdx)
	jne	.L3838
	movq	-2232(%rbp), %r8
	movq	80(%rdx), %r9
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	movq	72(%r9), %rbx
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3941
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	-840(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-840(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2232(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-848(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3943:
	movq	-2232(%rbp), %rbx
	movq	current_class_type(%rip), %r9
	movq	152(%rbx), %r8
	movq	%r9, 64(%rbx)
	movq	%r9, 16(%r8)
	jmp	.L3838
.L3941:
	movq	-840(%rbp), %r11
	movq	-2232(%rbp), %rdx
	movq	%r11, 72(%rdx)
	jmp	.L3943
.L12013:
	movq	-2232(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3936
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-840(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-840(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2232(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-848(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3938:
	movq	current_function_decl(%rip), %rdx
	movq	-2232(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L3838
.L3936:
	movq	-840(%rbp), %rdi
	movq	-2232(%rbp), %r10
	movq	%rdi, 72(%r10)
	jmp	.L3938
.L12010:
	cmpb	$95, 1(%rax)
	jne	.L3930
	movq	-2232(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L3930
.L12009:
	cmpq	$0, 72(%rbx)
	je	.L12014
.L10524:
	movq	-840(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L3916
.L12014:
	movq	-2232(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -856(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3911
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2232(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-856(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10521:
	movq	-840(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L3916
.L3911:
	movq	-2232(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10523
.L12008:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3909
.L10522:
	movq	-840(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L3916
.L3905:
	movq	-2232(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2232(%rbp)
	jmp	.L10524
.L12007:
	movq	global_binding_level(%rip), %r9
	movl	$1, %r12d
	cmpq	%r9, current_binding_level(%rip)
	je	.L3887
	movq	-840(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L3888
.L3887:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3889
	movq	-840(%rbp), %r10
	movq	56(%r10), %rcx
	testq	%rcx, %rcx
	jne	.L10091
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L3890
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12015
.L3890:
	testq	%rcx, %rcx
	jne	.L10091
.L10092:
	movq	-840(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10091
	movq	-840(%rbp), %rax
	movq	40(%rax), %rcx
.L3888:
	testq	%rcx, %rcx
	je	.L3892
.L10091:
	cmpb	$32, 16(%rcx)
	je	.L3892
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3892
	movq	-840(%rbp), %r8
	movq	8(%r8), %rax
	testq	%rax, %rax
	je	.L3900
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11062
	testl	%r12d, %r12d
	jle	.L12016
.L11062:
	movq	%rax, %rcx
.L3892:
	movq	-848(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L3886
	jmp	.L10521
.L12016:
	testl	%edx, %edx
	jg	.L11062
	testl	%r12d, %r12d
	je	.L3892
	movq	-840(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11062
.L3900:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L3892
.L12015:
	xorl	%ecx, %ecx
	movq	-840(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3892
	testq	%rax, %rax
	je	.L10092
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L3890
.L3889:
	movq	-840(%rbp), %r11
	movq	40(%r11), %rcx
	jmp	.L3888
.L12006:
	cmpb	$95, 1(%rax)
	jne	.L3885
	jmp	.L3916
.L3879:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3880
	movq	-840(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L11061
.L3878:
	movq	80(%rcx), %r11
	movq	%r11, -2232(%rbp)
	jmp	.L3883
.L12005:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L3844
	movq	80(%rax), %rbx
.L3844:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3877
.L3876:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L3853
	cmpl	$32, %eax
	je	.L12017
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L3847:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3877
	jmp	.L3876
.L12017:
	movq	8(%rbx), %r10
	movq	-848(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r10), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10883
	movq	64(%rbx), %rbx
	jmp	.L3847
.L10883:
	movq	32(%rax), %rcx
	jmp	.L3877
.L3853:
	movq	-848(%rbp), %r11
	movq	80(%r11), %rdx
	movq	56(%rdx), %rbx
	testq	%rbx, %rbx
	je	.L3842
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	je	.L3856
	movq	48(%rbx), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L3857
.L3856:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3858
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10089
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3859
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12018
.L3859:
	testq	%rcx, %rcx
	jne	.L10089
.L10090:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10089
.L3858:
	movq	40(%rbx), %rcx
.L3857:
	testq	%rcx, %rcx
	je	.L10241
.L10089:
	cmpb	$32, 16(%rcx)
	je	.L3877
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3877
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L3869
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11060
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L12019
.L11060:
	movq	%rax, %rcx
	jmp	.L3877
.L12019:
	testl	%edx, %edx
	jg	.L11060
	movl	$1, %edi
	testl	%edi, %edi
	je	.L3877
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11060
.L3869:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L3877
	jmp	.L11060
.L12018:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3877
	testq	%rax, %rax
	je	.L10090
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3859
.L3840:
	movq	-840(%rbp), %rsi
	movq	-848(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -864(%rbp)
	je	.L3917
	movq	-840(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11063:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3918:
	movq	-848(%rbp), %rdx
	movq	-840(%rbp), %r10
	movq	%rdx, 8(%r10)
	movq	-864(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L3921
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3922
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3922:
	movq	decl_obstack+24(%rip), %rdx
	movq	-864(%rbp), %r11
	leaq	8(%rdx), %rbx
	movq	%r11, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12020
.L3924:
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-864(%rbp), %rdx
	movq	%rdx, (%r8)
	cmpb	$32, 16(%rdx)
	je	.L12021
.L3921:
	movq	-864(%rbp), %r12
	movq	%r12, -2232(%rbp)
	jmp	.L10524
.L12021:
	cmpq	$0, 72(%rdx)
	jne	.L3921
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -872(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3926
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-864(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-872(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L3921
.L3926:
	movq	-864(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L3921
.L12020:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3924
.L3917:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3918
	movq	-840(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11063
.L12004:
	cmpq	$0, class_binding_level(%rip)
	je	.L3839
	movq	144(%rax), %rsi
	testb	$16, 3(%rsi)
	jne	.L3838
	jmp	.L3839
.L12003:
	movq	-840(%rbp), %rdi
	movq	-848(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L3835
	.p2align 6,,7
.L3806:
	movq	-2256(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10515
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10519
	testb	$8, 18(%r15)
	je	.L10519
	testb	$8, 18(%r13)
	jne	.L10519
	testb	$9, 53(%r13)
	jne	.L10519
	cmpq	%r13, current_function_decl(%rip)
	je	.L12022
.L3815:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L3816
	cmpq	$0, 8(%rax)
	jne	.L12023
.L3816:
	movq	-2256(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11059:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2256(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10519
.L12023:
	movq	-2256(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11059
.L12022:
	movq	-2256(%rbp), %rsi
	movq	%rsi, current_function_decl(%rip)
	jmp	.L3815
	.p2align 6,,7
.L11981:
	cmpq	$0, 64(%rcx)
	jne	.L3804
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L3804
.L11980:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2256(%rbp)
	call	error_with_decl
	jmp	.L3802
.L3795:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L3797
.L3801:
	cmpq	%r15, 56(%rax)
	je	.L3797
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3801
.L3797:
	movq	%rax, -2256(%rbp)
	jmp	.L3794
.L11979:
	movq	40(%r15), %rax
	jmp	.L3797
.L11978:
	movq	56(%r13), %r15
	jmp	.L3791
.L11977:
	testb	$32, 53(%r13)
	jne	.L3789
	jmp	.L3790
.L10511:
	movzbl	16(%r13), %edx
	jmp	.L3789
.L11276:
	leal	(%rcx,%rcx), %r10d
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%r10d,%rsi
	movl	%r10d, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L3782
.L3772:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3773
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11057
.L3771:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r15, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12024
.L11058:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3778:
	movq	$0, 8
	jmp	.L3770
.L12024:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3778
	jmp	.L11058
	.p2align 6,,7
.L11275:
	movq	-2168(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r8
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r8, -2200(%rbp)
	cmpq	%rax, %r13
	je	.L10492
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12025
.L3490:
	movq	%rax, 64(%r13)
.L3489:
	cmpb	$32, %dl
	je	.L12026
.L3491:
	testq	%r14, %r14
	je	.L3492
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12027
	cmpq	$0, 48(%r14)
	jne	.L3495
	movq	$0, -2208(%rbp)
.L3494:
	cmpq	$0, -2208(%rbp)
	je	.L3512
	movq	-2208(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L12028
.L3502:
	cmpq	$0, -2208(%rbp)
	je	.L10496
	movq	-2208(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12029
.L3504:
	movq	-2208(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movq	%rcx, %rsi
	movl	32(%rcx), %ebx
	je	.L10496
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L3506
	cmpb	$32, %al
	je	.L3512
	cmpb	$32, %dl
	je	.L10908
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10496
.L10500:
	movq	global_binding_level(%rip), %rax
.L3511:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L3765
	movq	-2168(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11056:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3766:
	movq	-2168(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L3488
	movq	-2208(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L3488
.L3765:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3766
	movq	-2168(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
	jmp	.L11056
.L10496:
	movzbl	16(%r13), %edx
.L3512:
	cmpb	$32, %dl
	je	.L10908
.L3520:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L3658
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L3658
	testb	$1, 53(%rax)
	jne	.L3659
	testb	$8, 18(%rax)
	je	.L3658
.L3659:
	andb	$8, %dl
	je	.L12030
.L3658:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10508
	testb	$1, 53(%r13)
	je	.L10508
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L3662
	movq	48(%r14), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L3663
.L3662:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3664
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10087
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3665
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12031
.L3665:
	testq	%rcx, %rcx
	jne	.L10087
.L10088:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10087
.L3664:
	movq	40(%r14), %rcx
.L3663:
	testq	%rcx, %rcx
	je	.L10240
.L10087:
	cmpb	$32, 16(%rcx)
	je	.L3667
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L3667
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L3675
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11053
	testl	%ebx, %ebx
	jle	.L12032
.L11053:
	movq	%rax, %rcx
.L3667:
	testq	%rcx, %rcx
	jne	.L10508
.L10240:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2200(%rbp)
.L3661:
	cmpq	%rax, -2200(%rbp)
	je	.L12033
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L12034
.L11054:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L3706:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L12035
.L3717:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L3719
	testq	%r12, %r12
	je	.L3720
	testb	$1, 53(%r13)
	jne	.L3720
	cmpb	$34, 16(%r12)
	je	.L12036
.L3720:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L3719
	testb	$1, 53(%r13)
	jne	.L3719
	movl	32(%r13), %ecx
	testl	%ecx, %ecx
	je	.L3719
	testq	%rax, %rax
	jne	.L3719
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L3725
	cmpb	$34, 16(%r12)
	je	.L12037
.L3725:
	cmpq	$0, 56(%r14)
	je	.L3727
	movl	$.LC41, %edi
.L3726:
	testq	%rdi, %rdi
	jne	.L11055
.L3719:
	testq	%r12, %r12
	je	.L10509
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-2200(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10509:
	movzbl	16(%r13), %edx
.L3704:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L3492
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L3492
	cmpb	$18, 16(%rcx)
	je	.L12038
.L3736:
	testb	$64, 46(%rcx)
	je	.L3492
.L3735:
	movq	-2200(%rbp), %r10
	movzwl	64(%r10), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%r10)
	je	.L12039
.L10510:
	movzbl	16(%r13), %edx
.L3492:
	cmpb	$32, %dl
	je	.L12040
.L3738:
	movq	-2200(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rcx
	cmpq	%rax, %rbx
	movq	%rcx, (%r13)
	movq	%r13, (%rbx)
	je	.L12041
.L3764:
	movq	%r13, -2208(%rbp)
	jmp	.L3511
.L12041:
	testb	$4, 17(%r13)
	jne	.L3764
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L3764
.L12040:
	testq	%r14, %r14
	je	.L3738
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3739
	cmpq	class_binding_level(%rip), %rax
	je	.L3740
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L3744
	cmpb	$32, 16(%rax)
	je	.L3742
.L3744:
	cmpq	$0, current_class_type(%rip)
	je	.L3739
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L3739
	cmpb	$32, 16(%rax)
	je	.L3742
.L3739:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L3743
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3742
	cmpb	$-127, %dl
	je	.L12042
.L3743:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L3738
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12043
.L3750:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3754
	cmpq	class_binding_level(%rip), %rax
	je	.L3755
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L3759
	cmpb	$32, 16(%rax)
	je	.L3757
.L3759:
	cmpq	$0, current_class_type(%rip)
	je	.L3754
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L3754
	cmpb	$32, 16(%rax)
	je	.L3757
.L3754:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L3738
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3757
	cmpb	$-127, %dl
	jne	.L3738
	movq	$0, 8(%rbx)
	jmp	.L3738
.L3757:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L3738
.L3755:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3759
.L12043:
	cmpb	$32, 16(%r13)
	movq	56(%r13), %rbx
	sete	%sil
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L3750
.L12042:
	movq	$0, 8(%r14)
	jmp	.L3743
.L3742:
	movq	8(%rax), %r11
	movq	%r11, 8(%r14)
	jmp	.L3743
.L3740:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3744
.L12039:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10510
.L12038:
	movq	8(%rcx), %r9
	testb	$64, 46(%r9)
	jne	.L3735
	jmp	.L3736
.L11055:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L3719
.L3727:
	testq	%r12, %r12
	je	.L3729
	movl	$.LC42, %edi
	jmp	.L3726
.L3729:
	testq	%r8, %r8
	movl	$.LC43, %edx
	cmovne	%rdx, %rdi
	jmp	.L3726
.L12037:
	movl	$.LC40, %edi
	jmp	.L3726
.L12036:
	cmpb	$34, 16(%r13)
	je	.L3720
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L3721
	movq	56(%rax), %rax
.L3721:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L3719
	movl	$.LC40, %edi
	jmp	.L11055
	.p2align 6,,7
.L12035:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12044
.L3709:
	testq	%r12, %r12
	jne	.L3717
	testq	%r8, %r8
	jne	.L3717
	testb	$1, 53(%r13)
	je	.L3717
	testb	$8, 18(%r13)
	je	.L3717
	orb	$8, 18(%r14)
	jmp	.L3717
	.p2align 6,,7
.L12044:
	testq	%r8, %r8
	je	.L3709
	cmpb	$29, 16(%r13)
	jne	.L3709
	cmpb	$29, 16(%r8)
	jne	.L3709
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12045
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L3712
	movzbl	53(%r13), %r9d
	leal	0(,%rax,8), %r10d
	leaq	88(%r13), %rdx
	andb	$-9, %r9b
	orb	%r10b, %r9b
	movb	%r9b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L3713
	movq	88(%r8), %rax
.L3714:
	movq	%rax, (%rdx)
	movq	136(%r8), %r9
	movq	80(%r8), %rdi
	movq	72(%r8), %rdx
	movzbl	17(%r13), %r11d
	movq	%r9, 136(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %ecx
	movq	%r8, 96(%r13)
	andb	$127, %r11b
	shrb	$7, %cl
	movzbl	%cl, %ebx
	movl	%ebx, %esi
	salb	$7, %sil
	orb	%sil, %r11b
	movb	%r11b, 17(%r13)
	movzbl	53(%r8), %ecx
.L3712:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L3715
	movzbl	53(%r13), %r10d
	salb	$4, %al
	andb	$-17, %r10b
	orb	%al, %r10b
	movb	%r10b, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L3715:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L3709
	cmpq	$0, 88(%r8)
	je	.L3709
	movq	8(%r13), %r11
	cmpq	$0, 24(%r11)
	jne	.L3709
	movq	%rdx, 8(%r13)
	jmp	.L3709
.L3713:
	xorl	%eax, %eax
	jmp	.L3714
.L12045:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L3709
	.p2align 6,,7
.L12034:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3706
	jmp	.L11054
.L12033:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12046
.L3681:
	cmpq	$0, 40(%r14)
	jne	.L3682
	testb	$8, 18(%r13)
	je	.L3682
	orb	$8, 18(%r14)
.L3682:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12047
.L3684:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L3683:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L3695
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3685
	testb	$1, 18(%rcx)
	je	.L3685
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L3685:
	testq	%rax, %rax
	je	.L3695
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3690
	testb	$8, 17(%rcx)
	je	.L3690
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L3690:
	testq	%rax, %rax
	je	.L3695
	cmpq	$0, 8(%rax)
	je	.L3695
	cmpb	$29, %dl
	je	.L12048
.L3698:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L3695:
	testb	$8, 18(%r14)
	je	.L3704
	cmpb	$32, %dl
	je	.L3704
	testb	$8, 18(%r13)
	jne	.L3704
	testb	$1, 53(%r13)
	jne	.L3704
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L3700
	cmpq	$0, 8(%rax)
	jne	.L12049
.L3700:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11234:
	xorl	%eax, %eax
	call	warning
	jmp	.L10509
.L12049:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11234
.L12048:
	movq	8(%r13), %rdi
	movq	integer_type_node(%rip), %r8
	cmpq	%r8, 8(%rdi)
	jne	.L3698
	jmp	.L3695
	.p2align 6,,7
.L12047:
	cmpq	$0, -2208(%rbp)
	je	.L3684
	movq	-2208(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L3683
	jmp	.L3684
.L12046:
	testb	$8, 54(%r13)
	jne	.L3681
	andb	$-9, 18(%r13)
	jmp	.L3681
	.p2align 6,,7
.L10508:
	movq	global_binding_level(%rip), %rax
	jmp	.L3661
.L12032:
	testl	%esi, %esi
	jg	.L11053
	testl	%ebx, %ebx
	je	.L3667
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11053
	.p2align 6,,7
.L3675:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L3667
.L12031:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3667
	testq	%rax, %rax
	je	.L10088
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3665
	.p2align 6,,7
.L12030:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L3658
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L3658
	.p2align 6,,7
.L10908:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2192(%rbp)
	je	.L3522
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L3521
.L3522:
	movq	global_binding_level(%rip), %r12
	movq	%r13, -2192(%rbp)
	cmpq	%r12, current_binding_level(%rip)
	jne	.L10501
	movq	%r13, 80(%rdx)
.L10501:
	movzbl	16(%r13), %eax
.L3525:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2192(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L3649
	cmpq	$0, 72(%rax)
	je	.L12050
.L3649:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L3520
	cmpq	$0, 56(%rax)
	je	.L3520
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -832(%rbp)
	je	.L3654
	movq	-832(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
.L11052:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3655:
	movq	-832(%rbp), %r10
	movq	%r12, 8(%r10)
	jmp	.L3520
.L3654:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3655
	movq	-832(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11052
.L12050:
	movq	8(%r13), %r11
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r11, -824(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L3650
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-824(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L3649
.L3650:
	movq	%rbx, 72(%r13)
	jmp	.L3649
.L3521:
	movq	-2192(%rbp), %r10
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r10), %rbx
	movq	%rbx, -784(%rbp)
	jne	.L3525
	movq	-784(%rbp), %r11
	movq	32(%r11), %rcx
	cmpb	$36, (%rcx)
	jne	.L3525
	cmpb	$95, 1(%rcx)
	jne	.L3525
	movq	class_binding_level(%rip), %r8
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r8, %r8
	movq	%r8, -2176(%rbp)
	movq	%rdx, -792(%rbp)
	jne	.L3529
	testb	$-128, 66(%rsi)
	movq	%rsi, -2176(%rbp)
	je	.L3529
.L3533:
	movq	-2176(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2176(%rbp)
	jne	.L3533
.L3529:
	movq	-2176(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12051
	movq	-2176(%rbp), %r9
	movq	-784(%rbp), %rdi
	xorl	%eax, %eax
	movq	-792(%rbp), %rsi
	movq	8(%r9), %rdx
	call	saveable_tree_cons
	movq	-2176(%rbp), %rcx
	movq	%rax, 8(%rcx)
.L3535:
	cmpq	$0, -784(%rbp)
	je	.L3536
	movq	-792(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L3537
	movq	-784(%rbp), %rdi
	movq	%rdi, 80(%rcx)
.L3537:
	movq	-784(%rbp), %r12
	movq	-792(%rbp), %rax
	cmpq	%rax, 8(%r12)
	je	.L3538
	cmpb	$21, 16(%rax)
	je	.L12052
.L3539:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L3541
	cmpq	$0, 32(%rax)
	je	.L3540
.L3541:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L12053
.L3542:
	xorl	%ecx, %ecx
.L3577:
	testq	%rcx, %rcx
	jne	.L3578
.L10239:
	movq	-784(%rbp), %rsi
	movq	-792(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-792(%rbp), %rdi
	movq	%rax, -2184(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2184(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3579
	movq	-784(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
.L11049:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3580:
	movq	-792(%rbp), %rsi
	movq	-784(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L3583:
	movq	-784(%rbp), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	je	.L12054
.L3585:
	movq	-792(%rbp), %r9
	movq	80(%r9), %rdx
	testq	%rdx, %rdx
	je	.L3616
	cmpb	$32, 16(%rdx)
	je	.L12055
.L3586:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3605
	movq	-2184(%rbp), %rax
	movq	56(%rax), %r12
	testq	%r12, %r12
	je	.L10503
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3607
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3607:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2184(%rbp), %rdi
	leaq	8(%rdx), %rcx
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rcx
	ja	.L12056
.L3609:
	movq	-2184(%rbp), %rbx
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r11)
	cmpb	$32, 16(%rbx)
	je	.L12057
.L10504:
	movq	-784(%rbp), %rsi
	movq	32(%rsi), %rax
.L3616:
	cmpb	$36, (%rax)
	je	.L12058
.L3630:
	movq	current_class_type(%rip), %rdx
	movq	-2184(%rbp), %r12
	movq	-792(%rbp), %r10
	testq	%rdx, %rdx
	movq	%r12, 80(%r10)
	jne	.L3633
	cmpq	$0, current_function_decl(%rip)
	je	.L3632
.L3633:
	movq	lang_name_cplusplus(%rip), %rax
	cmpq	%rax, current_lang_name(%rip)
	je	.L3631
.L3632:
	movq	-784(%rbp), %rdi
	movq	-2184(%rbp), %rcx
	movq	%rdi, 72(%rcx)
.L3538:
	movq	-2176(%rbp), %rax
	movzbl	66(%rax), %r12d
	andl	$15, %r12d
	cmpl	$2, %r12d
	je	.L12059
.L3536:
	movq	-792(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12060
	movq	-792(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-792(%rbp), %r12
	movq	%rax, (%r12)
	movq	-2192(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L3525
.L12060:
	movq	%rax, (%rdx)
	movq	-2192(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L3525
.L12059:
	movq	-792(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-784(%rbp), %r11
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r11)
	cmpq	$0, 32(%rax)
	jne	.L3536
	movq	-2176(%rbp), %rdx
	movq	144(%rax), %rcx
	movq	8(%rdx), %r8
	movq	%r8, 72(%rcx)
	jmp	.L3536
.L3631:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12061
	cmpq	$0, 32(%rdx)
	jne	.L3538
	movq	-2184(%rbp), %r9
	movq	80(%rdx), %r10
	movl	$136, %esi
	cmpb	$32, 16(%r9)
	movq	72(%r10), %rbx
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3641
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	-784(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-784(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2184(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-792(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3643:
	movq	-2184(%rbp), %rbx
	movq	current_class_type(%rip), %r10
	movq	152(%rbx), %r9
	movq	%r10, 64(%rbx)
	movq	%r10, 16(%r9)
	jmp	.L3538
.L3641:
	movq	-784(%rbp), %rdx
	movq	-2184(%rbp), %r8
	movq	%rdx, 72(%r8)
	jmp	.L3643
.L12061:
	movq	-2184(%rbp), %r8
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3636
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-784(%rbp), %r12
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-784(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2184(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-792(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3638:
	movq	current_function_decl(%rip), %r8
	movq	-2184(%rbp), %r11
	movq	%r8, 64(%r11)
	jmp	.L3538
.L3636:
	movq	-784(%rbp), %rcx
	movq	-2184(%rbp), %rdi
	movq	%rcx, 72(%rdi)
	jmp	.L3638
.L12058:
	cmpb	$95, 1(%rax)
	jne	.L3630
	movq	-2184(%rbp), %r9
	orb	$64, 53(%r9)
	jmp	.L3630
.L12057:
	cmpq	$0, 72(%rbx)
	je	.L12062
.L10505:
	movq	-784(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L3616
.L12062:
	movq	-2184(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %r8
	movq	%r8, -800(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3611
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2184(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-800(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10502:
	movq	-784(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L3616
.L3611:
	movq	-2184(%rbp), %rax
	movq	%r12, 72(%rax)
	jmp	.L10504
.L12056:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3609
.L10503:
	movq	-784(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L3616
.L3605:
	movq	-2184(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2184(%rbp)
	jmp	.L10505
.L12055:
	movq	global_binding_level(%rip), %r10
	movl	$1, %r12d
	cmpq	%r10, current_binding_level(%rip)
	je	.L3587
	movq	-784(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L3588
.L3587:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3589
	movq	-784(%rbp), %r11
	movq	56(%r11), %rcx
	testq	%rcx, %rcx
	jne	.L10085
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L3590
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L12063
.L3590:
	testq	%rcx, %rcx
	jne	.L10085
.L10086:
	movq	-784(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10085
	movq	-784(%rbp), %rsi
	movq	40(%rsi), %rcx
.L3588:
	testq	%rcx, %rcx
	je	.L3592
.L10085:
	cmpb	$32, 16(%rcx)
	je	.L3592
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3592
	movq	-784(%rbp), %r9
	movq	8(%r9), %rax
	testq	%rax, %rax
	je	.L3600
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11050
	testl	%r12d, %r12d
	jle	.L12064
.L11050:
	movq	%rax, %rcx
.L3592:
	movq	-792(%rbp), %r10
	cmpq	80(%r10), %rcx
	jne	.L3586
	jmp	.L10502
.L12064:
	testl	%edx, %edx
	jg	.L11050
	testl	%r12d, %r12d
	je	.L3592
	movq	-784(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11050
.L3600:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L3592
.L12063:
	xorl	%ecx, %ecx
	movq	-784(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3592
	testq	%rax, %rax
	je	.L10086
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L3590
.L3589:
	movq	-784(%rbp), %rdx
	movq	40(%rdx), %rcx
	jmp	.L3588
.L12054:
	cmpb	$95, 1(%rax)
	jne	.L3585
	jmp	.L3616
.L3579:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3580
	movq	-784(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
	jmp	.L11049
.L3578:
	movq	80(%rcx), %rax
	movq	%rax, -2184(%rbp)
	jmp	.L3583
.L12053:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L3544
	movq	80(%rax), %rbx
.L3544:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3577
.L3576:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L3553
	cmpl	$32, %eax
	je	.L12065
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L3547:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3577
	jmp	.L3576
.L12065:
	movq	8(%rbx), %rdx
	movq	-792(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r8
	movq	72(%r8), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10882
	movq	64(%rbx), %rbx
	jmp	.L3547
.L10882:
	movq	32(%rax), %rcx
	jmp	.L3577
.L3553:
	movq	-792(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L3542
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L3556
	movq	48(%rbx), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L3557
.L3556:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3558
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10083
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3559
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L12066
.L3559:
	testq	%rcx, %rcx
	jne	.L10083
.L10084:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10083
.L3558:
	movq	40(%rbx), %rcx
.L3557:
	testq	%rcx, %rcx
	je	.L10239
.L10083:
	cmpb	$32, 16(%rcx)
	je	.L3577
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3577
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L3569
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11048
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L12067
.L11048:
	movq	%rax, %rcx
	jmp	.L3577
.L12067:
	testl	%edx, %edx
	jg	.L11048
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L3577
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11048
.L3569:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L3577
	jmp	.L11048
.L12066:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3577
	testq	%rax, %rax
	je	.L10084
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3559
.L3540:
	movq	-784(%rbp), %rsi
	movq	-792(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -808(%rbp)
	je	.L3617
	movq	-784(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11051:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3618:
	movq	-792(%rbp), %r8
	movq	-784(%rbp), %r11
	movq	%r8, 8(%r11)
	movq	-808(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L3621
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3622
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3622:
	movq	-808(%rbp), %rdx
	movq	%rdx, 56(%r12)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12068
.L3624:
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-808(%rbp), %rdx
	movq	%rdx, (%rsi)
	cmpb	$32, 16(%rdx)
	je	.L12069
.L3621:
	movq	-808(%rbp), %r11
	movq	%r11, -2184(%rbp)
	jmp	.L10505
.L12069:
	cmpq	$0, 72(%rdx)
	jne	.L3621
	movq	current_class_name(%rip), %rbx
	movq	8(%rdx), %r9
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r9, -816(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3626
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-808(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-816(%rbp), %r10
	movq	%r10, 8(%rax)
	jmp	.L3621
.L3626:
	movq	-808(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L3621
.L12068:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3624
.L3617:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3618
	movq	-784(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11051
.L12052:
	cmpq	$0, class_binding_level(%rip)
	je	.L3539
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L3538
	jmp	.L3539
.L12051:
	movq	-784(%rbp), %rdi
	movq	-792(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L3535
	.p2align 6,,7
.L3506:
	movq	-2208(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10496
	movl	flag_traditional(%rip), %r9d
	testl	%r9d, %r9d
	jne	.L10500
	testb	$8, 18(%r14)
	je	.L10500
	testb	$8, 18(%r13)
	jne	.L10500
	testb	$9, 53(%r13)
	jne	.L10500
	cmpq	%r13, current_function_decl(%rip)
	je	.L12070
.L3515:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L3516
	cmpq	$0, 8(%rax)
	jne	.L12071
.L3516:
	movq	-2208(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11047:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2208(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10500
.L12071:
	movq	-2208(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11047
.L12070:
	movq	-2208(%rbp), %rdi
	movq	%rdi, current_function_decl(%rip)
	jmp	.L3515
	.p2align 6,,7
.L12029:
	cmpq	$0, 64(%rcx)
	jne	.L3504
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L3504
.L12028:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2208(%rbp)
	call	error_with_decl
	jmp	.L3502
.L3495:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L3497
.L3501:
	cmpq	%r14, 56(%rax)
	je	.L3497
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3501
.L3497:
	movq	%rax, -2208(%rbp)
	jmp	.L3494
.L12027:
	movq	40(%r14), %rax
	jmp	.L3497
.L12026:
	movq	56(%r13), %r14
	jmp	.L3491
.L12025:
	testb	$32, 53(%r13)
	jne	.L3489
	jmp	.L3490
.L10492:
	movzbl	16(%r13), %edx
	jmp	.L3489
.L11274:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L3482
.L3472:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3473
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11045
.L3471:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12072
.L11046:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3478:
	movq	$0, 8
	jmp	.L3470
.L12072:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3478
	jmp	.L11046
	.p2align 6,,7
.L11273:
	movq	-2120(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %rcx
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%rcx, -2152(%rbp)
	cmpq	%rax, %r13
	je	.L10473
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12073
.L3190:
	movq	%rax, 64(%r13)
.L3189:
	cmpb	$32, %dl
	je	.L12074
.L3191:
	testq	%r15, %r15
	je	.L3192
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12075
	cmpq	$0, 48(%r15)
	jne	.L3195
	movq	$0, -2160(%rbp)
.L3194:
	cmpq	$0, -2160(%rbp)
	je	.L3212
	movq	-2160(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L12076
.L3202:
	cmpq	$0, -2160(%rbp)
	je	.L10477
	movq	-2160(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12077
.L3204:
	movq	-2160(%rbp), %r8
	testq	%r8, %r8
	movq	24(%r8), %r12
	movq	%r8, %rsi
	movl	32(%r8), %ebx
	je	.L10477
	movzbl	16(%r8), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L3206
	cmpb	$32, %al
	je	.L3212
	cmpb	$32, %dl
	je	.L10907
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10477
.L10481:
	movq	global_binding_level(%rip), %rax
.L3211:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L3465
	movq	-2120(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11044:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3466:
	movq	-2120(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L3188
	movq	-2160(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L3188
.L3465:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3466
	movq	-2120(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11044
.L10477:
	movzbl	16(%r13), %edx
.L3212:
	cmpb	$32, %dl
	je	.L10907
.L3220:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L3358
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L3358
	testb	$1, 53(%rax)
	jne	.L3359
	testb	$8, 18(%rax)
	je	.L3358
.L3359:
	andb	$8, %dl
	je	.L12078
.L3358:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10489
	testb	$1, 53(%r13)
	je	.L10489
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L3362
	movq	48(%r15), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L3363
.L3362:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3364
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10081
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3365
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12079
.L3365:
	testq	%rcx, %rcx
	jne	.L10081
.L10082:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10081
.L3364:
	movq	40(%r15), %rcx
.L3363:
	testq	%rcx, %rcx
	je	.L10238
.L10081:
	cmpb	$32, 16(%rcx)
	je	.L3367
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L3367
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L3375
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11041
	testl	%ebx, %ebx
	jle	.L12080
.L11041:
	movq	%rax, %rcx
.L3367:
	testq	%rcx, %rcx
	jne	.L10489
.L10238:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2152(%rbp)
.L3361:
	cmpq	%rax, -2152(%rbp)
	je	.L12081
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12082
.L11042:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L3406:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12083
.L3417:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L3419
	testq	%r12, %r12
	je	.L3420
	testb	$1, 53(%r13)
	jne	.L3420
	cmpb	$34, 16(%r12)
	je	.L12084
.L3420:
	movl	warn_shadow(%rip), %ecx
	testl	%ecx, %ecx
	je	.L3419
	testb	$1, 53(%r13)
	jne	.L3419
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L3419
	testq	%rax, %rax
	jne	.L3419
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L3425
	cmpb	$34, 16(%r12)
	je	.L12085
.L3425:
	cmpq	$0, 56(%r15)
	je	.L3427
	movl	$.LC41, %edi
.L3426:
	testq	%rdi, %rdi
	jne	.L11043
.L3419:
	testq	%r12, %r12
	je	.L10490
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2152(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10490:
	movzbl	16(%r13), %edx
.L3404:
	leal	-128(%rdx), %r10d
	cmpb	$1, %r10b
	jbe	.L3192
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L3192
	cmpb	$18, 16(%rcx)
	je	.L12086
.L3436:
	testb	$64, 46(%rcx)
	je	.L3192
.L3435:
	movq	-2152(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12087
.L10491:
	movzbl	16(%r13), %edx
.L3192:
	cmpb	$32, %dl
	je	.L12088
.L3438:
	movq	-2152(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rdi
	cmpq	%rax, %rbx
	movq	%rdi, (%r13)
	movq	%r13, (%rbx)
	je	.L12089
.L3464:
	movq	%r13, -2160(%rbp)
	jmp	.L3211
.L12089:
	testb	$4, 17(%r13)
	jne	.L3464
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L3464
.L12088:
	testq	%r15, %r15
	je	.L3438
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3439
	cmpq	class_binding_level(%rip), %rax
	je	.L3440
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L3444
	cmpb	$32, 16(%rax)
	je	.L3442
.L3444:
	cmpq	$0, current_class_type(%rip)
	je	.L3439
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L3439
	cmpb	$32, 16(%rax)
	je	.L3442
.L3439:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L3443
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3442
	cmpb	$-127, %dl
	je	.L12090
.L3443:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L3438
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12091
.L3450:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3454
	cmpq	class_binding_level(%rip), %rax
	je	.L3455
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L3459
	cmpb	$32, 16(%rax)
	je	.L3457
.L3459:
	cmpq	$0, current_class_type(%rip)
	je	.L3454
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L3454
	cmpb	$32, 16(%rax)
	je	.L3457
.L3454:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L3438
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3457
	cmpb	$-127, %dl
	jne	.L3438
	movq	$0, 8(%rbx)
	jmp	.L3438
.L3457:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L3438
.L3455:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3459
.L12091:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%cl
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L3450
.L12090:
	movq	$0, 8(%r15)
	jmp	.L3443
.L3442:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r15)
	jmp	.L3443
.L3440:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3444
.L12087:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10491
.L12086:
	movq	8(%rcx), %r11
	testb	$64, 46(%r11)
	jne	.L3435
	jmp	.L3436
.L11043:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L3419
.L3427:
	testq	%r12, %r12
	je	.L3429
	movl	$.LC42, %edi
	jmp	.L3426
.L3429:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L3426
.L12085:
	movl	$.LC40, %edi
	jmp	.L3426
.L12084:
	cmpb	$34, 16(%r13)
	je	.L3420
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L3421
	movq	56(%rax), %rax
.L3421:
	movzbl	66(%rax), %ebx
	andl	$15, %ebx
	decl	%ebx
	jne	.L3419
	movl	$.LC40, %edi
	jmp	.L11043
	.p2align 6,,7
.L12083:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12092
.L3409:
	testq	%r12, %r12
	jne	.L3417
	testq	%r8, %r8
	jne	.L3417
	testb	$1, 53(%r13)
	je	.L3417
	testb	$8, 18(%r13)
	je	.L3417
	orb	$8, 18(%r15)
	jmp	.L3417
	.p2align 6,,7
.L12092:
	testq	%r8, %r8
	je	.L3409
	cmpb	$29, 16(%r13)
	jne	.L3409
	cmpb	$29, 16(%r8)
	jne	.L3409
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12093
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L3412
	movzbl	53(%r13), %r10d
	leal	0(,%rax,8), %r11d
	leaq	88(%r13), %rdx
	andb	$-9, %r10b
	orb	%r11b, %r10b
	movb	%r10b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L3413
	movq	88(%r8), %rax
.L3414:
	movq	%rax, (%rdx)
	movq	136(%r8), %r10
	movq	80(%r8), %rdi
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%r10, 136(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %r9d
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %r9b
	movzbl	%r9b, %ecx
	movl	%ecx, %ebx
	salb	$7, %bl
	orb	%bl, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L3412:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L3415
	movzbl	53(%r13), %r11d
	salb	$4, %al
	andb	$-17, %r11b
	orb	%al, %r11b
	movb	%r11b, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L3415:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L3409
	cmpq	$0, 88(%r8)
	je	.L3409
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L3409
	movq	%rdx, 8(%r13)
	jmp	.L3409
.L3413:
	xorl	%eax, %eax
	jmp	.L3414
.L12093:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L3409
	.p2align 6,,7
.L12082:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3406
	jmp	.L11042
.L12081:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12094
.L3381:
	cmpq	$0, 40(%r15)
	jne	.L3382
	testb	$8, 18(%r13)
	je	.L3382
	orb	$8, 18(%r15)
.L3382:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12095
.L3384:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L3383:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L3395
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3385
	testb	$1, 18(%rcx)
	je	.L3385
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L3385:
	testq	%rax, %rax
	je	.L3395
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3390
	testb	$8, 17(%rcx)
	je	.L3390
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L3390:
	testq	%rax, %rax
	je	.L3395
	cmpq	$0, 8(%rax)
	je	.L3395
	cmpb	$29, %dl
	je	.L12096
.L3398:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L3395:
	testb	$8, 18(%r15)
	je	.L3404
	cmpb	$32, %dl
	je	.L3404
	testb	$8, 18(%r13)
	jne	.L3404
	testb	$1, 53(%r13)
	jne	.L3404
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L3400
	cmpq	$0, 8(%rax)
	jne	.L12097
.L3400:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11233:
	xorl	%eax, %eax
	call	warning
	jmp	.L10490
.L12097:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11233
.L12096:
	movq	8(%r13), %rdi
	movq	integer_type_node(%rip), %r9
	cmpq	%r9, 8(%rdi)
	jne	.L3398
	jmp	.L3395
	.p2align 6,,7
.L12095:
	cmpq	$0, -2160(%rbp)
	je	.L3384
	movq	-2160(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L3383
	jmp	.L3384
.L12094:
	testb	$8, 54(%r13)
	jne	.L3381
	andb	$-9, 18(%r13)
	jmp	.L3381
	.p2align 6,,7
.L10489:
	movq	global_binding_level(%rip), %rax
	jmp	.L3361
.L12080:
	testl	%esi, %esi
	jg	.L11041
	testl	%ebx, %ebx
	je	.L3367
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11041
	.p2align 6,,7
.L3375:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L3367
.L12079:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3367
	testq	%rax, %rax
	je	.L10082
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3365
	.p2align 6,,7
.L12078:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L3358
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L3358
	.p2align 6,,7
.L10907:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2144(%rbp)
	je	.L3222
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L3221
.L3222:
	movq	global_binding_level(%rip), %rdi
	movq	%r13, -2144(%rbp)
	cmpq	%rdi, current_binding_level(%rip)
	jne	.L10482
	movq	%r13, 80(%rdx)
.L10482:
	movzbl	16(%r13), %eax
.L3225:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2144(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L3349
	cmpq	$0, 72(%rax)
	je	.L12098
.L3349:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L3220
	cmpq	$0, 56(%rax)
	je	.L3220
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -776(%rbp)
	je	.L3354
	movq	-776(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
.L11040:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3355:
	movq	-776(%rbp), %r10
	movq	%r12, 8(%r10)
	jmp	.L3220
.L3354:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3355
	movq	-776(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
	jmp	.L11040
.L12098:
	movq	8(%r13), %r11
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r11, -768(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L3350
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-768(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L3349
.L3350:
	movq	%rbx, 72(%r13)
	jmp	.L3349
.L3221:
	movq	-2144(%rbp), %r11
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r11), %rbx
	movq	%rbx, -728(%rbp)
	jne	.L3225
	movq	-728(%rbp), %r12
	movq	32(%r12), %rcx
	cmpb	$36, (%rcx)
	jne	.L3225
	cmpb	$95, 1(%rcx)
	jne	.L3225
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r9, %r9
	movq	%r9, -2128(%rbp)
	movq	%rdx, -736(%rbp)
	jne	.L3229
	testb	$-128, 66(%rsi)
	movq	%rsi, -2128(%rbp)
	je	.L3229
.L3233:
	movq	-2128(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2128(%rbp)
	jne	.L3233
.L3229:
	movq	-2128(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12099
	movq	-2128(%rbp), %r8
	movq	-728(%rbp), %rdi
	xorl	%eax, %eax
	movq	-736(%rbp), %rsi
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-2128(%rbp), %rcx
	movq	%rax, 8(%rcx)
.L3235:
	cmpq	$0, -728(%rbp)
	je	.L3236
	movq	-736(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L3237
	movq	-728(%rbp), %r10
	movq	%r10, 80(%rcx)
.L3237:
	movq	-728(%rbp), %rdi
	movq	-736(%rbp), %rax
	cmpq	%rax, 8(%rdi)
	je	.L3238
	cmpb	$21, 16(%rax)
	je	.L12100
.L3239:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L3241
	cmpq	$0, 32(%rax)
	je	.L3240
.L3241:
	movq	lang_name_cplusplus(%rip), %r12
	cmpq	%r12, current_lang_name(%rip)
	je	.L12101
.L3242:
	xorl	%ecx, %ecx
.L3277:
	testq	%rcx, %rcx
	jne	.L3278
.L10237:
	movq	-728(%rbp), %rsi
	movq	-736(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-736(%rbp), %rdi
	movq	%rax, -2136(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2136(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L3279
	movq	-728(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11037:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3280:
	movq	-736(%rbp), %rsi
	movq	-728(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L3283:
	movq	-728(%rbp), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	je	.L12102
.L3285:
	movq	-736(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L3316
	cmpb	$32, 16(%rdx)
	je	.L12103
.L3286:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3305
	movq	-2136(%rbp), %rax
	movq	56(%rax), %r12
	testq	%r12, %r12
	je	.L10484
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3307
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3307:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2136(%rbp), %rdi
	leaq	8(%rdx), %rcx
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rcx
	ja	.L12104
.L3309:
	movq	-2136(%rbp), %rbx
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r11)
	cmpb	$32, 16(%rbx)
	je	.L12105
.L10485:
	movq	-728(%rbp), %rsi
	movq	32(%rsi), %rax
.L3316:
	cmpb	$36, (%rax)
	je	.L12106
.L3330:
	movq	current_class_type(%rip), %rdx
	movq	-2136(%rbp), %r12
	movq	-736(%rbp), %r10
	testq	%rdx, %rdx
	movq	%r12, 80(%r10)
	jne	.L3333
	cmpq	$0, current_function_decl(%rip)
	je	.L3332
.L3333:
	movq	lang_name_cplusplus(%rip), %rax
	cmpq	%rax, current_lang_name(%rip)
	je	.L3331
.L3332:
	movq	-728(%rbp), %rdi
	movq	-2136(%rbp), %rcx
	movq	%rdi, 72(%rcx)
.L3238:
	movq	-2128(%rbp), %rax
	movzbl	66(%rax), %r12d
	andl	$15, %r12d
	cmpl	$2, %r12d
	je	.L12107
.L3236:
	movq	-736(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12108
	movq	-736(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-736(%rbp), %r12
	movq	%rax, (%r12)
	movq	-2144(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L3225
.L12108:
	movq	%rax, (%rdx)
	movq	-2144(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L3225
.L12107:
	movq	-736(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-728(%rbp), %r11
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r11)
	cmpq	$0, 32(%rax)
	jne	.L3236
	movq	-2128(%rbp), %rdx
	movq	144(%rax), %rcx
	movq	8(%rdx), %r9
	movq	%r9, 72(%rcx)
	jmp	.L3236
.L3331:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12109
	cmpq	$0, 32(%rdx)
	jne	.L3238
	movq	-2136(%rbp), %r8
	movq	80(%rdx), %r10
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	movq	72(%r10), %rbx
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3341
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	-728(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-728(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2136(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-736(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3343:
	movq	-2136(%rbp), %rbx
	movq	current_class_type(%rip), %r10
	movq	152(%rbx), %r8
	movq	%r10, 64(%rbx)
	movq	%r10, 16(%r8)
	jmp	.L3238
.L3341:
	movq	-728(%rbp), %rdx
	movq	-2136(%rbp), %r9
	movq	%rdx, 72(%r9)
	jmp	.L3343
.L12109:
	movq	-2136(%rbp), %r9
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%r9)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3336
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-728(%rbp), %r12
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-728(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2136(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-736(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3338:
	movq	current_function_decl(%rip), %r9
	movq	-2136(%rbp), %r11
	movq	%r9, 64(%r11)
	jmp	.L3238
.L3336:
	movq	-728(%rbp), %rcx
	movq	-2136(%rbp), %rdi
	movq	%rcx, 72(%rdi)
	jmp	.L3338
.L12106:
	cmpb	$95, 1(%rax)
	jne	.L3330
	movq	-2136(%rbp), %r8
	orb	$64, 53(%r8)
	jmp	.L3330
.L12105:
	cmpq	$0, 72(%rbx)
	je	.L12110
.L10486:
	movq	-728(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L3316
.L12110:
	movq	-2136(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %r9
	movq	%r9, -744(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3311
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2136(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-744(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10483:
	movq	-728(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L3316
.L3311:
	movq	-2136(%rbp), %rax
	movq	%r12, 72(%rax)
	jmp	.L10485
.L12104:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3309
.L10484:
	movq	-728(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L3316
.L3305:
	movq	-2136(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2136(%rbp)
	jmp	.L10486
.L12103:
	movq	global_binding_level(%rip), %r10
	movl	$1, %r12d
	cmpq	%r10, current_binding_level(%rip)
	je	.L3287
	movq	-728(%rbp), %r11
	movq	48(%r11), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L3288
.L3287:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3289
	movq	-728(%rbp), %rbx
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10079
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L3290
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12111
.L3290:
	testq	%rcx, %rcx
	jne	.L10079
.L10080:
	movq	-728(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10079
	movq	-728(%rbp), %rsi
	movq	40(%rsi), %rcx
.L3288:
	testq	%rcx, %rcx
	je	.L3292
.L10079:
	cmpb	$32, 16(%rcx)
	je	.L3292
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3292
	movq	-728(%rbp), %r8
	movq	8(%r8), %rax
	testq	%rax, %rax
	je	.L3300
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11038
	testl	%r12d, %r12d
	jle	.L12112
.L11038:
	movq	%rax, %rcx
.L3292:
	movq	-736(%rbp), %r10
	cmpq	80(%r10), %rcx
	jne	.L3286
	jmp	.L10483
.L12112:
	testl	%edx, %edx
	jg	.L11038
	testl	%r12d, %r12d
	je	.L3292
	movq	-728(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11038
.L3300:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L3292
.L12111:
	xorl	%ecx, %ecx
	movq	-728(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3292
	testq	%rax, %rax
	je	.L10080
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L3290
.L3289:
	movq	-728(%rbp), %rdx
	movq	40(%rdx), %rcx
	jmp	.L3288
.L12102:
	cmpb	$95, 1(%rax)
	jne	.L3285
	jmp	.L3316
.L3279:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3280
	movq	-728(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11037
.L3278:
	movq	80(%rcx), %rax
	movq	%rax, -2136(%rbp)
	jmp	.L3283
.L12101:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L3244
	movq	80(%rax), %rbx
.L3244:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3277
.L3276:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L3253
	cmpl	$32, %eax
	je	.L12113
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L3247:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L3277
	jmp	.L3276
.L12113:
	movq	8(%rbx), %rdx
	movq	-736(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10881
	movq	64(%rbx), %rbx
	jmp	.L3247
.L10881:
	movq	32(%rax), %rcx
	jmp	.L3277
.L3253:
	movq	-736(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L3242
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L3256
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L3257
.L3256:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3258
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10077
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3259
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L12114
.L3259:
	testq	%rcx, %rcx
	jne	.L10077
.L10078:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10077
.L3258:
	movq	40(%rbx), %rcx
.L3257:
	testq	%rcx, %rcx
	je	.L10237
.L10077:
	cmpb	$32, 16(%rcx)
	je	.L3277
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L3277
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L3269
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11036
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L12115
.L11036:
	movq	%rax, %rcx
	jmp	.L3277
.L12115:
	testl	%edx, %edx
	jg	.L11036
	movl	$1, %r11d
	testl	%r11d, %r11d
	je	.L3277
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11036
.L3269:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L3277
	jmp	.L11036
.L12114:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3277
	testq	%rax, %rax
	je	.L10078
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3259
.L3240:
	movq	-728(%rbp), %rsi
	movq	-736(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -752(%rbp)
	je	.L3317
	movq	-728(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11039:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3318:
	movq	-736(%rbp), %r9
	movq	-728(%rbp), %r11
	movq	%r9, 8(%r11)
	movq	-752(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L3321
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3322
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3322:
	movq	-752(%rbp), %rdx
	movq	%rdx, 56(%r12)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12116
.L3324:
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-752(%rbp), %rdx
	movq	%rdx, (%rsi)
	cmpb	$32, 16(%rdx)
	je	.L12117
.L3321:
	movq	-752(%rbp), %r11
	movq	%r11, -2136(%rbp)
	jmp	.L10486
.L12117:
	cmpq	$0, 72(%rdx)
	jne	.L3321
	movq	current_class_name(%rip), %rbx
	movq	8(%rdx), %r8
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r8, -760(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3326
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-752(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-760(%rbp), %r10
	movq	%r10, 8(%rax)
	jmp	.L3321
.L3326:
	movq	-752(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L3321
.L12116:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3324
.L3317:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3318
	movq	-728(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11039
.L12100:
	cmpq	$0, class_binding_level(%rip)
	je	.L3239
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L3238
	jmp	.L3239
.L12099:
	movq	-728(%rbp), %rdi
	movq	-736(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L3235
	.p2align 6,,7
.L3206:
	movq	-2160(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10477
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10481
	testb	$8, 18(%r15)
	je	.L10481
	testb	$8, 18(%r13)
	jne	.L10481
	testb	$9, 53(%r13)
	jne	.L10481
	cmpq	%r13, current_function_decl(%rip)
	je	.L12118
.L3215:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L3216
	cmpq	$0, 8(%rax)
	jne	.L12119
.L3216:
	movq	-2160(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11035:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2160(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10481
.L12119:
	movq	-2160(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11035
.L12118:
	movq	-2160(%rbp), %r10
	movq	%r10, current_function_decl(%rip)
	jmp	.L3215
	.p2align 6,,7
.L12077:
	cmpq	$0, 64(%rcx)
	jne	.L3204
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L3204
.L12076:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2160(%rbp)
	call	error_with_decl
	jmp	.L3202
.L3195:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L3197
.L3201:
	cmpq	%r15, 56(%rax)
	je	.L3197
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3201
.L3197:
	movq	%rax, -2160(%rbp)
	jmp	.L3194
.L12075:
	movq	40(%r15), %rax
	jmp	.L3197
.L12074:
	movq	56(%r13), %r15
	jmp	.L3191
.L12073:
	testb	$32, 53(%r13)
	jne	.L3189
	jmp	.L3190
.L10473:
	movzbl	16(%r13), %edx
	jmp	.L3189
.L11272:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L3182
.L3172:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3173
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11033
.L3171:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r15, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12120
.L11034:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3178:
	movq	$0, 8
	jmp	.L3170
.L12120:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3178
	jmp	.L11034
	.p2align 6,,7
.L11271:
	movq	-2072(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r8
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_function_decl(%rip), %rax
	movq	%r8, -2104(%rbp)
	cmpq	%rax, %r13
	je	.L10454
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12121
.L2890:
	movq	%rax, 64(%r13)
.L2889:
	cmpb	$32, %dl
	je	.L12122
.L2891:
	testq	%r14, %r14
	je	.L2892
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12123
	cmpq	$0, 48(%r14)
	jne	.L2895
	movq	$0, -2112(%rbp)
.L2894:
	cmpq	$0, -2112(%rbp)
	je	.L2912
	movq	-2112(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L12124
.L2902:
	cmpq	$0, -2112(%rbp)
	je	.L10458
	movq	-2112(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12125
.L2904:
	movq	-2112(%rbp), %r11
	testq	%r11, %r11
	movq	24(%r11), %r12
	movq	%r11, %rsi
	movl	32(%r11), %ebx
	je	.L10458
	movzbl	16(%r11), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L2906
	cmpb	$32, %al
	je	.L2912
	cmpb	$32, %dl
	je	.L10906
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10458
.L10462:
	movq	global_binding_level(%rip), %rax
.L2911:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L3165
	movq	-2072(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11032:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3166:
	movq	-2072(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L2888
	movq	-2112(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L2888
.L3165:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3166
	movq	-2072(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L11032
.L10458:
	movzbl	16(%r13), %edx
.L2912:
	cmpb	$32, %dl
	je	.L10906
.L2920:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L3058
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L3058
	testb	$1, 53(%rax)
	jne	.L3059
	testb	$8, 18(%rax)
	je	.L3058
.L3059:
	andb	$8, %dl
	je	.L12126
.L3058:
	movl	flag_traditional(%rip), %ecx
	testl	%ecx, %ecx
	je	.L10470
	testb	$1, 53(%r13)
	je	.L10470
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L3062
	movq	48(%r14), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L3063
.L3062:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L3064
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10075
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L3065
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L12127
.L3065:
	testq	%rcx, %rcx
	jne	.L10075
.L10076:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10075
.L3064:
	movq	40(%r14), %rcx
.L3063:
	testq	%rcx, %rcx
	je	.L10236
.L10075:
	cmpb	$32, 16(%rcx)
	je	.L3067
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L3067
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L3075
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11029
	testl	%ebx, %ebx
	jle	.L12128
.L11029:
	movq	%rax, %rcx
.L3067:
	testq	%rcx, %rcx
	jne	.L10470
.L10236:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2104(%rbp)
.L3061:
	cmpq	%rax, -2104(%rbp)
	je	.L12129
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L12130
.L11030:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L3106:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L12131
.L3117:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L3119
	testq	%r12, %r12
	je	.L3120
	testb	$1, 53(%r13)
	jne	.L3120
	cmpb	$34, 16(%r12)
	je	.L12132
.L3120:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L3119
	testb	$1, 53(%r13)
	jne	.L3119
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L3119
	testq	%rax, %rax
	jne	.L3119
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L3125
	cmpb	$34, 16(%r12)
	je	.L12133
.L3125:
	cmpq	$0, 56(%r14)
	je	.L3127
	movl	$.LC41, %edi
.L3126:
	testq	%rdi, %rdi
	jne	.L11031
.L3119:
	testq	%r12, %r12
	je	.L10471
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-2104(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10471:
	movzbl	16(%r13), %edx
.L3104:
	leal	-128(%rdx), %r9d
	cmpb	$1, %r9b
	jbe	.L2892
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L2892
	cmpb	$18, 16(%rcx)
	je	.L12134
.L3136:
	testb	$64, 46(%rcx)
	je	.L2892
.L3135:
	movq	-2104(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12135
.L10472:
	movzbl	16(%r13), %edx
.L2892:
	cmpb	$32, %dl
	je	.L12136
.L3138:
	movq	-2104(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rdi
	cmpq	%rax, %rbx
	movq	%rdi, (%r13)
	movq	%r13, (%rbx)
	je	.L12137
.L3164:
	movq	%r13, -2112(%rbp)
	jmp	.L2911
.L12137:
	testb	$4, 17(%r13)
	jne	.L3164
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L3164
.L12136:
	testq	%r14, %r14
	je	.L3138
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3139
	cmpq	class_binding_level(%rip), %rax
	je	.L3140
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L3144
	cmpb	$32, 16(%rax)
	je	.L3142
.L3144:
	cmpq	$0, current_class_type(%rip)
	je	.L3139
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L3139
	cmpb	$32, 16(%rax)
	je	.L3142
.L3139:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L3143
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3142
	cmpb	$-127, %dl
	je	.L12138
.L3143:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L3138
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12139
.L3150:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L3154
	cmpq	class_binding_level(%rip), %rax
	je	.L3155
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L3159
	cmpb	$32, 16(%rax)
	je	.L3157
.L3159:
	cmpq	$0, current_class_type(%rip)
	je	.L3154
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L3154
	cmpb	$32, 16(%rax)
	je	.L3157
.L3154:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L3138
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L3157
	cmpb	$-127, %dl
	jne	.L3138
	movq	$0, 8(%rbx)
	jmp	.L3138
.L3157:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L3138
.L3155:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3159
.L12139:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r10b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L3150
.L12138:
	movq	$0, 8(%r14)
	jmp	.L3143
.L3142:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r14)
	jmp	.L3143
.L3140:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L3144
.L12135:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10472
.L12134:
	movq	8(%rcx), %rsi
	testb	$64, 46(%rsi)
	jne	.L3135
	jmp	.L3136
.L11031:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L3119
.L3127:
	testq	%r12, %r12
	je	.L3129
	movl	$.LC42, %edi
	jmp	.L3126
.L3129:
	testq	%r8, %r8
	movl	$.LC43, %r11d
	cmovne	%r11, %rdi
	jmp	.L3126
.L12133:
	movl	$.LC40, %edi
	jmp	.L3126
.L12132:
	cmpb	$34, 16(%r13)
	je	.L3120
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L3121
	movq	56(%rax), %rax
.L3121:
	movzbl	66(%rax), %r10d
	andl	$15, %r10d
	decl	%r10d
	jne	.L3119
	movl	$.LC40, %edi
	jmp	.L11031
	.p2align 6,,7
.L12131:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12140
.L3109:
	testq	%r12, %r12
	jne	.L3117
	testq	%r8, %r8
	jne	.L3117
	testb	$1, 53(%r13)
	je	.L3117
	testb	$8, 18(%r13)
	je	.L3117
	orb	$8, 18(%r14)
	jmp	.L3117
	.p2align 6,,7
.L12140:
	testq	%r8, %r8
	je	.L3109
	cmpb	$29, 16(%r13)
	jne	.L3109
	cmpb	$29, 16(%r8)
	jne	.L3109
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12141
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L3112
	movzbl	53(%r13), %r9d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r9b
	orb	%sil, %r9b
	movb	%r9b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L3113
	movq	88(%r8), %rax
.L3114:
	movq	%rax, (%rdx)
	movq	136(%r8), %r9
	movq	80(%r8), %r11
	movq	72(%r8), %rdx
	movzbl	17(%r13), %ecx
	movq	%r9, 136(%r13)
	movq	%r11, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %cl
	shrb	$7, %dil
	movzbl	%dil, %ebx
	movl	%ebx, %r10d
	salb	$7, %r10b
	orb	%r10b, %cl
	movb	%cl, 17(%r13)
	movzbl	53(%r8), %ecx
.L3112:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L3115
	movzbl	53(%r13), %esi
	salb	$4, %al
	andb	$-17, %sil
	orb	%al, %sil
	movb	%sil, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L3115:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L3109
	cmpq	$0, 88(%r8)
	je	.L3109
	movq	8(%r13), %rcx
	cmpq	$0, 24(%rcx)
	jne	.L3109
	movq	%rdx, 8(%r13)
	jmp	.L3109
.L3113:
	xorl	%eax, %eax
	jmp	.L3114
.L12141:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L3109
	.p2align 6,,7
.L12130:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3106
	jmp	.L11030
.L12129:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12142
.L3081:
	cmpq	$0, 40(%r14)
	jne	.L3082
	testb	$8, 18(%r13)
	je	.L3082
	orb	$8, 18(%r14)
.L3082:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12143
.L3084:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L3083:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L3095
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3085
	testb	$1, 18(%rcx)
	je	.L3085
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L3085:
	testq	%rax, %rax
	je	.L3095
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L3090
	testb	$8, 17(%rcx)
	je	.L3090
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L3090:
	testq	%rax, %rax
	je	.L3095
	cmpq	$0, 8(%rax)
	je	.L3095
	cmpb	$29, %dl
	je	.L12144
.L3098:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L3095:
	testb	$8, 18(%r14)
	je	.L3104
	cmpb	$32, %dl
	je	.L3104
	testb	$8, 18(%r13)
	jne	.L3104
	testb	$1, 53(%r13)
	jne	.L3104
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L3100
	cmpq	$0, 8(%rax)
	jne	.L12145
.L3100:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11232:
	xorl	%eax, %eax
	call	warning
	jmp	.L10471
.L12145:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11232
.L12144:
	movq	8(%r13), %r11
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r11)
	jne	.L3098
	jmp	.L3095
	.p2align 6,,7
.L12143:
	cmpq	$0, -2112(%rbp)
	je	.L3084
	movq	-2112(%rbp), %r12
	cmpb	$32, 16(%r12)
	jne	.L3083
	jmp	.L3084
.L12142:
	testb	$8, 54(%r13)
	jne	.L3081
	andb	$-9, 18(%r13)
	jmp	.L3081
	.p2align 6,,7
.L10470:
	movq	global_binding_level(%rip), %rax
	jmp	.L3061
.L12128:
	testl	%esi, %esi
	jg	.L11029
	testl	%ebx, %ebx
	je	.L3067
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11029
	.p2align 6,,7
.L3075:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L3067
.L12127:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L3067
	testq	%rax, %rax
	je	.L10076
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L3065
	.p2align 6,,7
.L12126:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L3058
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L3058
	.p2align 6,,7
.L10906:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2096(%rbp)
	je	.L2922
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L2921
.L2922:
	movq	global_binding_level(%rip), %r9
	movq	%r13, -2096(%rbp)
	cmpq	%r9, current_binding_level(%rip)
	jne	.L10463
	movq	%r13, 80(%rdx)
.L10463:
	movzbl	16(%r13), %eax
.L2925:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2096(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L3049
	cmpq	$0, 72(%rax)
	je	.L12146
.L3049:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L2920
	cmpq	$0, 56(%rax)
	je	.L2920
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -720(%rbp)
	je	.L3054
	movq	-720(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
.L11028:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3055:
	movq	-720(%rbp), %rsi
	movq	%r12, 8(%rsi)
	jmp	.L2920
.L3054:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3055
	movq	-720(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11028
.L12146:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -712(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L3050
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-712(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L3049
.L3050:
	movq	%rbx, 72(%r13)
	jmp	.L3049
.L2921:
	movq	-2096(%rbp), %r12
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r12), %rbx
	movq	%rbx, -672(%rbp)
	jne	.L2925
	movq	-672(%rbp), %rdi
	movq	32(%rdi), %rcx
	cmpb	$36, (%rcx)
	jne	.L2925
	cmpb	$95, 1(%rcx)
	jne	.L2925
	movq	class_binding_level(%rip), %r10
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rcx
	testq	%r10, %r10
	movq	%r10, -2080(%rbp)
	movq	%rcx, -680(%rbp)
	jne	.L2929
	testb	$-128, 66(%rsi)
	movq	%rsi, -2080(%rbp)
	je	.L2929
.L2933:
	movq	-2080(%rbp), %r8
	movq	56(%r8), %rdx
	testb	$-128, 66(%rdx)
	movq	%rdx, -2080(%rbp)
	jne	.L2933
.L2929:
	movq	-2080(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12147
	movq	-2080(%rbp), %r9
	movq	-672(%rbp), %rdi
	xorl	%eax, %eax
	movq	-680(%rbp), %rsi
	movq	8(%r9), %rdx
	call	saveable_tree_cons
	movq	-2080(%rbp), %r11
	movq	%rax, 8(%r11)
.L2935:
	cmpq	$0, -672(%rbp)
	je	.L2936
	movq	-680(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L2937
	movq	-672(%rbp), %rax
	movq	%rax, 80(%rcx)
.L2937:
	movq	-672(%rbp), %rbx
	movq	-680(%rbp), %rax
	cmpq	%rax, 8(%rbx)
	je	.L2938
	cmpb	$21, 16(%rax)
	je	.L12148
.L2939:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L2941
	cmpq	$0, 32(%rax)
	je	.L2940
.L2941:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L12149
.L2942:
	xorl	%ecx, %ecx
.L2977:
	testq	%rcx, %rcx
	jne	.L2978
.L10235:
	movq	-672(%rbp), %rsi
	movq	-680(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-680(%rbp), %rdi
	movq	%rax, -2088(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2088(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2979
	movq	-672(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L11025:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2980:
	movq	-680(%rbp), %rcx
	movq	-672(%rbp), %rdx
	movq	%rcx, 8(%rdx)
.L2983:
	movq	-672(%rbp), %r11
	movq	32(%r11), %rax
	cmpb	$36, (%rax)
	je	.L12150
.L2985:
	movq	-680(%rbp), %r9
	movq	80(%r9), %rdx
	testq	%rdx, %rdx
	je	.L3016
	cmpb	$32, 16(%rdx)
	je	.L12151
.L2986:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3005
	movq	-2088(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10465
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3007
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3007:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2088(%rbp), %rdi
	leaq	8(%rdx), %rsi
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rsi
	ja	.L12152
.L3009:
	movq	-2088(%rbp), %rbx
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r10)
	cmpb	$32, 16(%rbx)
	je	.L12153
.L10466:
	movq	-672(%rbp), %r8
	movq	32(%r8), %rax
.L3016:
	cmpb	$36, (%rax)
	je	.L12154
.L3030:
	movq	current_class_type(%rip), %rdx
	movq	-2088(%rbp), %rcx
	movq	-680(%rbp), %r9
	testq	%rdx, %rdx
	movq	%rcx, 80(%r9)
	jne	.L3033
	cmpq	$0, current_function_decl(%rip)
	je	.L3032
.L3033:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L3031
.L3032:
	movq	-672(%rbp), %r12
	movq	-2088(%rbp), %rdi
	movq	%r12, 72(%rdi)
.L2938:
	movq	-2080(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12155
.L2936:
	movq	-680(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12156
	movq	-680(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-680(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2096(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L2925
.L12156:
	movq	%rax, (%rdx)
	movq	-2096(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L2925
.L12155:
	movq	-680(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r12
	movq	-672(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%r12, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L2936
	movq	-2080(%rbp), %r11
	movq	144(%rax), %r8
	movq	8(%r11), %rdx
	movq	%rdx, 72(%r8)
	jmp	.L2936
.L3031:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12157
	cmpq	$0, 32(%rdx)
	jne	.L2938
	movq	-2088(%rbp), %r11
	movq	80(%rdx), %r9
	movl	$136, %esi
	cmpb	$32, 16(%r11)
	movq	72(%r9), %rbx
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3041
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-672(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-672(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2088(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-680(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3043:
	movq	-2088(%rbp), %rbx
	movq	current_class_type(%rip), %r9
	movq	152(%rbx), %r11
	movq	%r9, 64(%rbx)
	movq	%r9, 16(%r11)
	jmp	.L2938
.L3041:
	movq	-672(%rbp), %r8
	movq	-2088(%rbp), %rsi
	movq	%r8, 72(%rsi)
	jmp	.L3043
.L12157:
	movq	-2088(%rbp), %rdx
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rdx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3036
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-672(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-672(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2088(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-680(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L3038:
	movq	current_function_decl(%rip), %rsi
	movq	-2088(%rbp), %rdx
	movq	%rsi, 64(%rdx)
	jmp	.L2938
.L3036:
	movq	-672(%rbp), %r12
	movq	-2088(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L3038
.L12154:
	cmpb	$95, 1(%rax)
	jne	.L3030
	movq	-2088(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L3030
.L12153:
	cmpq	$0, 72(%rbx)
	je	.L12158
.L10467:
	movq	-672(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L3016
.L12158:
	movq	-2088(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -688(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3011
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2088(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-688(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10464:
	movq	-672(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L3016
.L3011:
	movq	-2088(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10466
.L12152:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3009
.L10465:
	movq	-672(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L3016
.L3005:
	movq	-2088(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2088(%rbp)
	jmp	.L10467
.L12151:
	movq	global_binding_level(%rip), %rsi
	movl	$1, %r12d
	cmpq	%rsi, current_binding_level(%rip)
	je	.L2987
	movq	-672(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L2988
.L2987:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2989
	movq	-672(%rbp), %r10
	movq	56(%r10), %rcx
	testq	%rcx, %rcx
	jne	.L10073
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L2990
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12159
.L2990:
	testq	%rcx, %rcx
	jne	.L10073
.L10074:
	movq	-672(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10073
	movq	-672(%rbp), %rax
	movq	40(%rax), %rcx
.L2988:
	testq	%rcx, %rcx
	je	.L2992
.L10073:
	cmpb	$32, 16(%rcx)
	je	.L2992
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2992
	movq	-672(%rbp), %r11
	movq	8(%r11), %rax
	testq	%rax, %rax
	je	.L3000
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11026
	testl	%r12d, %r12d
	jle	.L12160
.L11026:
	movq	%rax, %rcx
.L2992:
	movq	-680(%rbp), %r9
	cmpq	80(%r9), %rcx
	jne	.L2986
	jmp	.L10464
.L12160:
	testl	%edx, %edx
	jg	.L11026
	testl	%r12d, %r12d
	je	.L2992
	movq	-672(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11026
.L3000:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L2992
.L12159:
	xorl	%ecx, %ecx
	movq	-672(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2992
	testq	%rax, %rax
	je	.L10074
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L2990
.L2989:
	movq	-672(%rbp), %r8
	movq	40(%r8), %rcx
	jmp	.L2988
.L12150:
	cmpb	$95, 1(%rax)
	jne	.L2985
	jmp	.L3016
.L2979:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2980
	movq	-672(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L11025
.L2978:
	movq	80(%rcx), %r8
	movq	%r8, -2088(%rbp)
	jmp	.L2983
.L12149:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L2944
	movq	80(%rax), %rbx
.L2944:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2977
.L2976:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L2953
	cmpl	$32, %eax
	je	.L12161
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L2947:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2977
	jmp	.L2976
.L12161:
	movq	8(%rbx), %rcx
	movq	-680(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rcx), %r10
	movq	72(%r10), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10880
	movq	64(%rbx), %rbx
	jmp	.L2947
.L10880:
	movq	32(%rax), %rcx
	jmp	.L2977
.L2953:
	movq	-680(%rbp), %r8
	movq	80(%r8), %rdx
	movq	56(%rdx), %rbx
	testq	%rbx, %rbx
	je	.L2942
	movq	global_binding_level(%rip), %r11
	cmpq	%r11, current_binding_level(%rip)
	je	.L2956
	movq	48(%rbx), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L2957
.L2956:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2958
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10071
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2959
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12162
.L2959:
	testq	%rcx, %rcx
	jne	.L10071
.L10072:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10071
.L2958:
	movq	40(%rbx), %rcx
.L2957:
	testq	%rcx, %rcx
	je	.L10235
.L10071:
	cmpb	$32, 16(%rcx)
	je	.L2977
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2977
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L2969
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11024
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L12163
.L11024:
	movq	%rax, %rcx
	jmp	.L2977
.L12163:
	testl	%edx, %edx
	jg	.L11024
	movl	$1, %edi
	testl	%edi, %edi
	je	.L2977
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11024
.L2969:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L2977
	jmp	.L11024
.L12162:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2977
	testq	%rax, %rax
	je	.L10072
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2959
.L2940:
	movq	-672(%rbp), %rsi
	movq	-680(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -696(%rbp)
	je	.L3017
	movq	-672(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11027:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L3018:
	movq	-680(%rbp), %rdx
	movq	-672(%rbp), %rsi
	movq	%rdx, 8(%rsi)
	movq	-696(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L3021
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L3022
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L3022:
	movq	decl_obstack+24(%rip), %rdx
	movq	-696(%rbp), %r8
	leaq	8(%rdx), %rbx
	movq	%r8, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12164
.L3024:
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-696(%rbp), %rdx
	movq	%rdx, (%r11)
	cmpb	$32, 16(%rdx)
	je	.L12165
.L3021:
	movq	-696(%rbp), %r12
	movq	%r12, -2088(%rbp)
	jmp	.L10467
.L12165:
	cmpq	$0, 72(%rdx)
	jne	.L3021
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -704(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L3026
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-696(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-704(%rbp), %r9
	movq	%r9, 8(%rax)
	jmp	.L3021
.L3026:
	movq	-696(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L3021
.L12164:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L3024
.L3017:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L3018
	movq	-672(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L11027
.L12148:
	cmpq	$0, class_binding_level(%rip)
	je	.L2939
	movq	144(%rax), %rsi
	testb	$16, 3(%rsi)
	jne	.L2938
	jmp	.L2939
.L12147:
	movq	-672(%rbp), %rdi
	movq	-680(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L2935
	.p2align 6,,7
.L2906:
	movq	-2112(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10458
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L10462
	testb	$8, 18(%r14)
	je	.L10462
	testb	$8, 18(%r13)
	jne	.L10462
	testb	$9, 53(%r13)
	jne	.L10462
	cmpq	%r13, current_function_decl(%rip)
	je	.L12166
.L2915:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L2916
	cmpq	$0, 8(%rax)
	jne	.L12167
.L2916:
	movq	-2112(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11023:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2112(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10462
.L12167:
	movq	-2112(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11023
.L12166:
	movq	-2112(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L2915
	.p2align 6,,7
.L12125:
	cmpq	$0, 64(%rcx)
	jne	.L2904
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L2904
.L12124:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2112(%rbp)
	call	error_with_decl
	jmp	.L2902
.L2895:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L2897
.L2901:
	cmpq	%r14, 56(%rax)
	je	.L2897
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L2901
.L2897:
	movq	%rax, -2112(%rbp)
	jmp	.L2894
.L12123:
	movq	40(%r14), %rax
	jmp	.L2897
.L12122:
	movq	56(%r13), %r14
	jmp	.L2891
.L12121:
	testb	$32, 53(%r13)
	jne	.L2889
	jmp	.L2890
.L10454:
	movzbl	16(%r13), %edx
	jmp	.L2889
.L11270:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L2882
.L2872:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2873
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11021
.L2871:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12168
.L11022:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2878:
	movq	$0, 8
	jmp	.L2870
.L12168:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2878
	jmp	.L11022
	.p2align 6,,7
.L11269:
	movq	-2024(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %rcx
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%rcx, -2056(%rbp)
	cmpq	%rax, %r13
	je	.L10435
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12169
.L2590:
	movq	%rax, 64(%r13)
.L2589:
	cmpb	$32, %dl
	je	.L12170
.L2591:
	testq	%r15, %r15
	je	.L2592
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12171
	cmpq	$0, 48(%r15)
	jne	.L2595
	movq	$0, -2064(%rbp)
.L2594:
	cmpq	$0, -2064(%rbp)
	je	.L2612
	movq	-2064(%rbp), %rax
	cmpq	error_mark_node(%rip), %rax
	je	.L12172
.L2602:
	cmpq	$0, -2064(%rbp)
	je	.L10439
	movq	-2064(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12173
.L2604:
	movq	-2064(%rbp), %r8
	testq	%r8, %r8
	movq	24(%r8), %r12
	movq	%r8, %rsi
	movl	32(%r8), %ebx
	je	.L10439
	movzbl	16(%r8), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L2606
	cmpb	$32, %al
	je	.L2612
	cmpb	$32, %dl
	je	.L10905
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10439
.L10443:
	movq	global_binding_level(%rip), %rax
.L2611:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L2865
	movq	-2024(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11020:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2866:
	movq	-2024(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L2588
	movq	-2064(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L2588
.L2865:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2866
	movq	-2024(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11020
.L10439:
	movzbl	16(%r13), %edx
.L2612:
	cmpb	$32, %dl
	je	.L10905
.L2620:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L2758
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2758
	testb	$1, 53(%rax)
	jne	.L2759
	testb	$8, 18(%rax)
	je	.L2758
.L2759:
	andb	$8, %dl
	je	.L12174
.L2758:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10451
	testb	$1, 53(%r13)
	je	.L10451
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L2762
	movq	48(%r15), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L2763
.L2762:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2764
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10069
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2765
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12175
.L2765:
	testq	%rcx, %rcx
	jne	.L10069
.L10070:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10069
.L2764:
	movq	40(%r15), %rcx
.L2763:
	testq	%rcx, %rcx
	je	.L10234
.L10069:
	cmpb	$32, 16(%rcx)
	je	.L2767
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L2767
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L2775
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11017
	testl	%ebx, %ebx
	jle	.L12176
.L11017:
	movq	%rax, %rcx
.L2767:
	testq	%rcx, %rcx
	jne	.L10451
.L10234:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2056(%rbp)
.L2761:
	cmpq	%rax, -2056(%rbp)
	je	.L12177
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12178
.L11018:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L2806:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12179
.L2817:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L2819
	testq	%r12, %r12
	je	.L2820
	testb	$1, 53(%r13)
	jne	.L2820
	cmpb	$34, 16(%r12)
	je	.L12180
.L2820:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L2819
	testb	$1, 53(%r13)
	jne	.L2819
	movl	32(%r13), %r11d
	testl	%r11d, %r11d
	je	.L2819
	testq	%rax, %rax
	jne	.L2819
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L2825
	cmpb	$34, 16(%r12)
	je	.L12181
.L2825:
	cmpq	$0, 56(%r15)
	je	.L2827
	movl	$.LC41, %edi
.L2826:
	testq	%rdi, %rdi
	jne	.L11019
.L2819:
	testq	%r12, %r12
	je	.L10452
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2056(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10452:
	movzbl	16(%r13), %edx
.L2804:
	leal	-128(%rdx), %ecx
	cmpb	$1, %cl
	jbe	.L2592
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L2592
	cmpb	$18, 16(%rcx)
	je	.L12182
.L2836:
	testb	$64, 46(%rcx)
	je	.L2592
.L2835:
	movq	-2056(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12183
.L10453:
	movzbl	16(%r13), %edx
.L2592:
	cmpb	$32, %dl
	je	.L12184
.L2838:
	movq	-2056(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %r11
	cmpq	%rax, %rbx
	movq	%r11, (%r13)
	movq	%r13, (%rbx)
	je	.L12185
.L2864:
	movq	%r13, -2064(%rbp)
	jmp	.L2611
.L12185:
	testb	$4, 17(%r13)
	jne	.L2864
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L2864
.L12184:
	testq	%r15, %r15
	je	.L2838
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2839
	cmpq	class_binding_level(%rip), %rax
	je	.L2840
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L2844
	cmpb	$32, 16(%rax)
	je	.L2842
.L2844:
	cmpq	$0, current_class_type(%rip)
	je	.L2839
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L2839
	cmpb	$32, 16(%rax)
	je	.L2842
.L2839:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2843
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2842
	cmpb	$-127, %dl
	je	.L12186
.L2843:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L2838
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12187
.L2850:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2854
	cmpq	class_binding_level(%rip), %rax
	je	.L2855
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L2859
	cmpb	$32, 16(%rax)
	je	.L2857
.L2859:
	cmpq	$0, current_class_type(%rip)
	je	.L2854
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L2854
	cmpb	$32, 16(%rax)
	je	.L2857
.L2854:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L2838
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2857
	cmpb	$-127, %dl
	jne	.L2838
	movq	$0, 8(%rbx)
	jmp	.L2838
.L2857:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L2838
.L2855:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2859
.L12187:
	cmpb	$32, 16(%r13)
	movq	56(%r13), %rbx
	sete	%sil
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L2850
.L12186:
	movq	$0, 8(%r15)
	jmp	.L2843
.L2842:
	movq	8(%rax), %r10
	movq	%r10, 8(%r15)
	jmp	.L2843
.L2840:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2844
.L12183:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10453
.L12182:
	movq	8(%rcx), %rdi
	testb	$64, 46(%rdi)
	jne	.L2835
	jmp	.L2836
.L11019:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L2819
.L2827:
	testq	%r12, %r12
	je	.L2829
	movl	$.LC42, %edi
	jmp	.L2826
.L2829:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L2826
.L12181:
	movl	$.LC40, %edi
	jmp	.L2826
.L12180:
	cmpb	$34, 16(%r13)
	je	.L2820
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L2821
	movq	56(%rax), %rax
.L2821:
	movzbl	66(%rax), %esi
	andl	$15, %esi
	decl	%esi
	jne	.L2819
	movl	$.LC40, %edi
	jmp	.L11019
	.p2align 6,,7
.L12179:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12188
.L2809:
	testq	%r12, %r12
	jne	.L2817
	testq	%r8, %r8
	jne	.L2817
	testb	$1, 53(%r13)
	je	.L2817
	testb	$8, 18(%r13)
	je	.L2817
	orb	$8, 18(%r15)
	jmp	.L2817
	.p2align 6,,7
.L12188:
	testq	%r8, %r8
	je	.L2809
	cmpb	$29, 16(%r13)
	jne	.L2809
	cmpb	$29, 16(%r8)
	jne	.L2809
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12189
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L2812
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %edi
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%dil, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L2813
	movq	88(%r8), %rax
.L2814:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %r9
	movq	72(%r8), %rdx
	movzbl	17(%r13), %r10d
	movq	%rcx, 136(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %r11d
	movq	%r8, 96(%r13)
	andb	$127, %r10b
	shrb	$7, %r11b
	movzbl	%r11b, %ebx
	movl	%ebx, %esi
	salb	$7, %sil
	orb	%sil, %r10b
	movb	%r10b, 17(%r13)
	movzbl	53(%r8), %ecx
.L2812:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L2815
	movzbl	53(%r13), %edi
	salb	$4, %al
	andb	$-17, %dil
	orb	%al, %dil
	movb	%dil, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L2815:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L2809
	cmpq	$0, 88(%r8)
	je	.L2809
	movq	8(%r13), %r10
	cmpq	$0, 24(%r10)
	jne	.L2809
	movq	%rdx, 8(%r13)
	jmp	.L2809
.L2813:
	xorl	%eax, %eax
	jmp	.L2814
.L12189:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L2809
	.p2align 6,,7
.L12178:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2806
	jmp	.L11018
.L12177:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12190
.L2781:
	cmpq	$0, 40(%r15)
	jne	.L2782
	testb	$8, 18(%r13)
	je	.L2782
	orb	$8, 18(%r15)
.L2782:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12191
.L2784:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L2783:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2795
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2785
	testb	$1, 18(%rcx)
	je	.L2785
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L2785:
	testq	%rax, %rax
	je	.L2795
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2790
	testb	$8, 17(%rcx)
	je	.L2790
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L2790:
	testq	%rax, %rax
	je	.L2795
	cmpq	$0, 8(%rax)
	je	.L2795
	cmpb	$29, %dl
	je	.L12192
.L2798:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L2795:
	testb	$8, 18(%r15)
	je	.L2804
	cmpb	$32, %dl
	je	.L2804
	testb	$8, 18(%r13)
	jne	.L2804
	testb	$1, 53(%r13)
	jne	.L2804
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2800
	cmpq	$0, 8(%rax)
	jne	.L12193
.L2800:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11231:
	xorl	%eax, %eax
	call	warning
	jmp	.L10452
.L12193:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11231
.L12192:
	movq	8(%r13), %r9
	movq	integer_type_node(%rip), %r8
	cmpq	%r8, 8(%r9)
	jne	.L2798
	jmp	.L2795
	.p2align 6,,7
.L12191:
	cmpq	$0, -2064(%rbp)
	je	.L2784
	movq	-2064(%rbp), %r11
	cmpb	$32, 16(%r11)
	jne	.L2783
	jmp	.L2784
.L12190:
	testb	$8, 54(%r13)
	jne	.L2781
	andb	$-9, 18(%r13)
	jmp	.L2781
	.p2align 6,,7
.L10451:
	movq	global_binding_level(%rip), %rax
	jmp	.L2761
.L12176:
	testl	%esi, %esi
	jg	.L11017
	testl	%ebx, %ebx
	je	.L2767
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11017
	.p2align 6,,7
.L2775:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L2767
.L12175:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2767
	testq	%rax, %rax
	je	.L10070
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2765
	.p2align 6,,7
.L12174:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L2758
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L2758
	.p2align 6,,7
.L10905:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -2048(%rbp)
	je	.L2622
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L2621
.L2622:
	movq	global_binding_level(%rip), %r10
	movq	%r13, -2048(%rbp)
	cmpq	%r10, current_binding_level(%rip)
	jne	.L10444
	movq	%r13, 80(%rdx)
.L10444:
	movzbl	16(%r13), %eax
.L2625:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2048(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L2749
	cmpq	$0, 72(%rax)
	je	.L12194
.L2749:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L2620
	cmpq	$0, 56(%rax)
	je	.L2620
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r8
	cmpq	global_binding_level(%rip), %rbx
	movq	%r8, -664(%rbp)
	je	.L2754
	movq	-664(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
.L11016:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2755:
	movq	-664(%rbp), %rdi
	movq	%r12, 8(%rdi)
	jmp	.L2620
.L2754:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2755
	movq	-664(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L11016
.L12194:
	movq	8(%r13), %r10
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r10, -656(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L2750
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-656(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L2749
.L2750:
	movq	%rbx, 72(%r13)
	jmp	.L2749
.L2621:
	movq	-2048(%rbp), %r12
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r12), %rbx
	movq	%rbx, -616(%rbp)
	jne	.L2625
	movq	-616(%rbp), %r11
	movq	32(%r11), %rcx
	cmpb	$36, (%rcx)
	jne	.L2625
	cmpb	$95, 1(%rcx)
	jne	.L2625
	movq	class_binding_level(%rip), %rdi
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%rdi, %rdi
	movq	%rdi, -2032(%rbp)
	movq	%rdx, -624(%rbp)
	jne	.L2629
	testb	$-128, 66(%rsi)
	movq	%rsi, -2032(%rbp)
	je	.L2629
.L2633:
	movq	-2032(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -2032(%rbp)
	jne	.L2633
.L2629:
	movq	-2032(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12195
	movq	-2032(%rbp), %r8
	movq	-616(%rbp), %rdi
	xorl	%eax, %eax
	movq	-624(%rbp), %rsi
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-2032(%rbp), %rcx
	movq	%rax, 8(%rcx)
.L2635:
	cmpq	$0, -616(%rbp)
	je	.L2636
	movq	-624(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L2637
	movq	-616(%rbp), %r9
	movq	%r9, 80(%rcx)
.L2637:
	movq	-616(%rbp), %r10
	movq	-624(%rbp), %rax
	cmpq	%rax, 8(%r10)
	je	.L2638
	cmpb	$21, 16(%rax)
	je	.L12196
.L2639:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L2641
	cmpq	$0, 32(%rax)
	je	.L2640
.L2641:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L12197
.L2642:
	xorl	%ecx, %ecx
.L2677:
	testq	%rcx, %rcx
	jne	.L2678
.L10233:
	movq	-616(%rbp), %rsi
	movq	-624(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-624(%rbp), %rdi
	movq	%rax, -2040(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-2040(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2679
	movq	-616(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
.L11013:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2680:
	movq	-624(%rbp), %rdi
	movq	-616(%rbp), %rdx
	movq	%rdi, 8(%rdx)
.L2683:
	movq	-616(%rbp), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	je	.L12198
.L2685:
	movq	-624(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L2716
	cmpb	$32, 16(%rdx)
	je	.L12199
.L2686:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2705
	movq	-2040(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10446
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2707
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2707:
	movq	decl_obstack+24(%rip), %rdx
	movq	-2040(%rbp), %r10
	leaq	8(%rdx), %r9
	movq	%r10, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r9
	ja	.L12200
.L2709:
	movq	-2040(%rbp), %rbx
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r11)
	cmpb	$32, 16(%rbx)
	je	.L12201
.L10447:
	movq	-616(%rbp), %rdi
	movq	32(%rdi), %rax
.L2716:
	cmpb	$36, (%rax)
	je	.L12202
.L2730:
	movq	current_class_type(%rip), %rdx
	movq	-2040(%rbp), %rsi
	movq	-624(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rsi, 80(%rcx)
	jne	.L2733
	cmpq	$0, current_function_decl(%rip)
	je	.L2732
.L2733:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L2731
.L2732:
	movq	-616(%rbp), %r11
	movq	-2040(%rbp), %r10
	movq	%r11, 72(%r10)
.L2638:
	movq	-2032(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12203
.L2636:
	movq	-624(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12204
	movq	-624(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-624(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2048(%rbp), %r9
	movzbl	16(%r9), %eax
	jmp	.L2625
.L12204:
	movq	%rax, (%rdx)
	movq	-2048(%rbp), %rdi
	movzbl	16(%rdi), %eax
	jmp	.L2625
.L12203:
	movq	-624(%rbp), %r11
	orb	$64, 18(%r11)
	movq	80(%r11), %rsi
	movq	-616(%rbp), %r10
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r10)
	cmpq	$0, 32(%rax)
	jne	.L2636
	movq	-2032(%rbp), %r8
	movq	144(%rax), %r12
	movq	8(%r8), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L2636
.L2731:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12205
	cmpq	$0, 32(%rdx)
	jne	.L2638
	movq	-2040(%rbp), %rdi
	movq	80(%rdx), %r9
	movl	$136, %esi
	cmpb	$32, 16(%rdi)
	movq	72(%r9), %rbx
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2741
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	-616(%rbp), %r11
	cmpb	$1, 16(%r11)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-616(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2040(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-624(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2743:
	movq	-2040(%rbp), %rbx
	movq	current_class_type(%rip), %r9
	movq	152(%rbx), %rdi
	movq	%r9, 64(%rbx)
	movq	%r9, 16(%rdi)
	jmp	.L2638
.L2741:
	movq	-616(%rbp), %r8
	movq	-2040(%rbp), %rdx
	movq	%r8, 72(%rdx)
	jmp	.L2743
.L12205:
	movq	-2040(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2736
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-616(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-616(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2040(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-624(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2738:
	movq	current_function_decl(%rip), %rdx
	movq	-2040(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L2638
.L2736:
	movq	-616(%rbp), %r11
	movq	-2040(%rbp), %r10
	movq	%r11, 72(%r10)
	jmp	.L2738
.L12202:
	cmpb	$95, 1(%rax)
	jne	.L2730
	movq	-2040(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L2730
.L12201:
	cmpq	$0, 72(%rbx)
	je	.L12206
.L10448:
	movq	-616(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L2716
.L12206:
	movq	-2040(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -632(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2711
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-2040(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-632(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10445:
	movq	-616(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L2716
.L2711:
	movq	-2040(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10447
.L12200:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2709
.L10446:
	movq	-616(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L2716
.L2705:
	movq	-2040(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -2040(%rbp)
	jmp	.L10448
.L12199:
	movq	global_binding_level(%rip), %r9
	movl	$1, %r12d
	cmpq	%r9, current_binding_level(%rip)
	je	.L2687
	movq	-616(%rbp), %rbx
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L2688
.L2687:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2689
	movq	-616(%rbp), %r11
	movq	56(%r11), %rcx
	testq	%rcx, %rcx
	jne	.L10067
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L2690
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12207
.L2690:
	testq	%rcx, %rcx
	jne	.L10067
.L10068:
	movq	-616(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10067
	movq	-616(%rbp), %rax
	movq	40(%rax), %rcx
.L2688:
	testq	%rcx, %rcx
	je	.L2692
.L10067:
	cmpb	$32, 16(%rcx)
	je	.L2692
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2692
	movq	-616(%rbp), %r8
	movq	8(%r8), %rax
	testq	%rax, %rax
	je	.L2700
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11014
	testl	%r12d, %r12d
	jle	.L12208
.L11014:
	movq	%rax, %rcx
.L2692:
	movq	-624(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L2686
	jmp	.L10445
.L12208:
	testl	%edx, %edx
	jg	.L11014
	testl	%r12d, %r12d
	je	.L2692
	movq	-616(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11014
.L2700:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L2692
.L12207:
	xorl	%ecx, %ecx
	movq	-616(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2692
	testq	%rax, %rax
	je	.L10068
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L2690
.L2689:
	movq	-616(%rbp), %rdi
	movq	40(%rdi), %rcx
	jmp	.L2688
.L12198:
	cmpb	$95, 1(%rax)
	jne	.L2685
	jmp	.L2716
.L2679:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2680
	movq	-616(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L11013
.L2678:
	movq	80(%rcx), %rsi
	movq	%rsi, -2040(%rbp)
	jmp	.L2683
.L12197:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L2644
	movq	80(%rax), %rbx
.L2644:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2677
.L2676:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L2653
	cmpl	$32, %eax
	je	.L12209
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L2647:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2677
	jmp	.L2676
.L12209:
	movq	8(%rbx), %rdi
	xorl	%eax, %eax
	movq	144(%rdi), %rdx
	movq	-624(%rbp), %rdi
	movq	72(%rdx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10879
	movq	64(%rbx), %rbx
	jmp	.L2647
.L10879:
	movq	32(%rax), %rcx
	jmp	.L2677
.L2653:
	movq	-624(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L2642
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L2656
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L2657
.L2656:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2658
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10065
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2659
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12210
.L2659:
	testq	%rcx, %rcx
	jne	.L10065
.L10066:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10065
.L2658:
	movq	40(%rbx), %rcx
.L2657:
	testq	%rcx, %rcx
	je	.L10233
.L10065:
	cmpb	$32, 16(%rcx)
	je	.L2677
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2677
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L2669
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11012
	movl	$1, %r10d
	testl	%r10d, %r10d
	jle	.L12211
.L11012:
	movq	%rax, %rcx
	jmp	.L2677
.L12211:
	testl	%edx, %edx
	jg	.L11012
	movl	$1, %r12d
	testl	%r12d, %r12d
	je	.L2677
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11012
.L2669:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L2677
	jmp	.L11012
.L12210:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2677
	testq	%rax, %rax
	je	.L10066
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2659
.L2640:
	movq	-616(%rbp), %rsi
	movq	-624(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -640(%rbp)
	je	.L2717
	movq	-616(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11015:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2718:
	movq	-624(%rbp), %rdx
	movq	-616(%rbp), %r11
	movq	%rdx, 8(%r11)
	movq	-640(%rbp), %r10
	movq	56(%r10), %r12
	testq	%r12, %r12
	je	.L2721
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2722
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2722:
	movq	decl_obstack+24(%rip), %rdx
	movq	-640(%rbp), %rdi
	leaq	8(%rdx), %rbx
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12212
.L2724:
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-640(%rbp), %rdx
	movq	%rdx, (%r8)
	cmpb	$32, 16(%rdx)
	je	.L12213
.L2721:
	movq	-640(%rbp), %r12
	movq	%r12, -2040(%rbp)
	jmp	.L10448
.L12213:
	cmpq	$0, 72(%rdx)
	jne	.L2721
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -648(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2726
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-640(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-648(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L2721
.L2726:
	movq	-640(%rbp), %r11
	movq	%r12, 72(%r11)
	jmp	.L2721
.L12212:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2724
.L2717:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2718
	movq	-616(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11015
.L12196:
	cmpq	$0, class_binding_level(%rip)
	je	.L2639
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L2638
	jmp	.L2639
.L12195:
	movq	-616(%rbp), %rdi
	movq	-624(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L2635
	.p2align 6,,7
.L2606:
	movq	-2064(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10439
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10443
	testb	$8, 18(%r15)
	je	.L10443
	testb	$8, 18(%r13)
	jne	.L10443
	testb	$9, 53(%r13)
	jne	.L10443
	cmpq	%r13, current_function_decl(%rip)
	je	.L12214
.L2615:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2616
	cmpq	$0, 8(%rax)
	jne	.L12215
.L2616:
	movq	-2064(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11011:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2064(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10443
.L12215:
	movq	-2064(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11011
.L12214:
	movq	-2064(%rbp), %r9
	movq	%r9, current_function_decl(%rip)
	jmp	.L2615
	.p2align 6,,7
.L12173:
	cmpq	$0, 64(%rcx)
	jne	.L2604
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L2604
.L12172:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2064(%rbp)
	call	error_with_decl
	jmp	.L2602
.L2595:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L2597
.L2601:
	cmpq	%r15, 56(%rax)
	je	.L2597
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L2601
.L2597:
	movq	%rax, -2064(%rbp)
	jmp	.L2594
.L12171:
	movq	40(%r15), %rax
	jmp	.L2597
.L12170:
	movq	56(%r13), %r15
	jmp	.L2591
.L12169:
	testb	$32, 53(%r13)
	jne	.L2589
	jmp	.L2590
.L10435:
	movzbl	16(%r13), %edx
	jmp	.L2589
.L11268:
	leal	(%rcx,%rcx), %edi
	xorl	%eax, %eax
	movl	%edi, builtin_type_tdescs_max(%rip)
	movslq	%edi,%rsi
	movq	builtin_type_tdescs_arr(%rip), %rdi
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L2582
.L2572:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2573
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L11009
.L2571:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12216
.L11010:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2578:
	movq	$0, 8
	jmp	.L2570
.L12216:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2578
	jmp	.L11010
	.p2align 6,,7
.L11267:
	movq	-1976(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r9
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r9, -2008(%rbp)
	cmpq	%rax, %r13
	je	.L10416
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12217
.L2290:
	movq	%rax, 64(%r13)
.L2289:
	cmpb	$32, %dl
	je	.L12218
.L2291:
	testq	%r15, %r15
	je	.L2292
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12219
	cmpq	$0, 48(%r15)
	jne	.L2295
	movq	$0, -2016(%rbp)
.L2294:
	cmpq	$0, -2016(%rbp)
	je	.L2312
	movq	-2016(%rbp), %r10
	cmpq	error_mark_node(%rip), %r10
	je	.L12220
.L2302:
	cmpq	$0, -2016(%rbp)
	je	.L10420
	movq	-2016(%rbp), %rdx
	cmpb	$34, 16(%rdx)
	je	.L12221
.L2304:
	movq	-2016(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movl	32(%rcx), %ebx
	je	.L10420
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L2306
	cmpb	$32, %al
	je	.L2312
	cmpb	$32, %dl
	je	.L10904
	xorl	%eax, %eax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10420
.L10424:
	movq	global_binding_level(%rip), %rax
.L2311:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L2565
	movq	-1976(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L11008:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2566:
	movq	-1976(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L2288
	movq	-2016(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L2288
.L2565:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2566
	movq	-1976(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L11008
.L10420:
	movzbl	16(%r13), %edx
.L2312:
	cmpb	$32, %dl
	je	.L10904
.L2320:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L2458
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2458
	testb	$1, 53(%rax)
	jne	.L2459
	testb	$8, 18(%rax)
	je	.L2458
.L2459:
	andb	$8, %dl
	je	.L12222
.L2458:
	movl	flag_traditional(%rip), %ecx
	testl	%ecx, %ecx
	je	.L10432
	testb	$1, 53(%r13)
	je	.L10432
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L2462
	movq	48(%r15), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L2463
.L2462:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2464
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10063
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2465
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12223
.L2465:
	testq	%rcx, %rcx
	jne	.L10063
.L10064:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10063
.L2464:
	movq	40(%r15), %rcx
.L2463:
	testq	%rcx, %rcx
	je	.L10232
.L10063:
	cmpb	$32, 16(%rcx)
	je	.L2467
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L2467
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L2475
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11005
	testl	%ebx, %ebx
	jle	.L12224
.L11005:
	movq	%rax, %rcx
.L2467:
	testq	%rcx, %rcx
	jne	.L10432
.L10232:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -2008(%rbp)
.L2461:
	cmpq	%rax, -2008(%rbp)
	je	.L12225
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12226
.L11006:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L2506:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12227
.L2517:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L2519
	testq	%r12, %r12
	je	.L2520
	testb	$1, 53(%r13)
	jne	.L2520
	cmpb	$34, 16(%r12)
	je	.L12228
.L2520:
	movl	warn_shadow(%rip), %edx
	testl	%edx, %edx
	je	.L2519
	testb	$1, 53(%r13)
	jne	.L2519
	movl	32(%r13), %r9d
	testl	%r9d, %r9d
	je	.L2519
	testq	%rax, %rax
	jne	.L2519
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L2525
	cmpb	$34, 16(%r12)
	je	.L12229
.L2525:
	cmpq	$0, 56(%r15)
	je	.L2527
	movl	$.LC41, %edi
.L2526:
	testq	%rdi, %rdi
	jne	.L11007
.L2519:
	testq	%r12, %r12
	je	.L10433
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-2008(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10433:
	movzbl	16(%r13), %edx
.L2504:
	leal	-128(%rdx), %r11d
	cmpb	$1, %r11b
	jbe	.L2292
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L2292
	cmpb	$18, 16(%rcx)
	je	.L12230
.L2536:
	testb	$64, 46(%rcx)
	je	.L2292
.L2535:
	movq	-2008(%rbp), %rcx
	movzwl	64(%rcx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rcx)
	je	.L12231
.L10434:
	movzbl	16(%r13), %edx
.L2292:
	cmpb	$32, %dl
	je	.L12232
.L2538:
	movq	-2008(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %r9
	cmpq	%rax, %rbx
	movq	%r9, (%r13)
	movq	%r13, (%rbx)
	je	.L12233
.L2564:
	movq	%r13, -2016(%rbp)
	jmp	.L2311
.L12233:
	testb	$4, 17(%r13)
	jne	.L2564
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L2564
.L12232:
	testq	%r15, %r15
	je	.L2538
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2539
	cmpq	class_binding_level(%rip), %rax
	je	.L2540
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L2544
	cmpb	$32, 16(%rax)
	je	.L2542
.L2544:
	cmpq	$0, current_class_type(%rip)
	je	.L2539
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L2539
	cmpb	$32, 16(%rax)
	je	.L2542
.L2539:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2543
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2542
	cmpb	$-127, %dl
	je	.L12234
.L2543:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L2538
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12235
.L2550:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2554
	cmpq	class_binding_level(%rip), %rax
	je	.L2555
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L2559
	cmpb	$32, 16(%rax)
	je	.L2557
.L2559:
	cmpq	$0, current_class_type(%rip)
	je	.L2554
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L2554
	cmpb	$32, 16(%rax)
	je	.L2557
.L2554:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L2538
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2557
	cmpb	$-127, %dl
	jne	.L2538
	movq	$0, 8(%rbx)
	jmp	.L2538
.L2557:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L2538
.L2555:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2559
.L12235:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%dl
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L2550
.L12234:
	movq	$0, 8(%r15)
	jmp	.L2543
.L2542:
	movq	8(%rax), %rdi
	movq	%rdi, 8(%r15)
	jmp	.L2543
.L2540:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2544
.L12231:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10434
.L12230:
	movq	8(%rcx), %rsi
	testb	$64, 46(%rsi)
	jne	.L2535
	jmp	.L2536
.L11007:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L2519
.L2527:
	testq	%r12, %r12
	je	.L2529
	movl	$.LC42, %edi
	jmp	.L2526
.L2529:
	testq	%r8, %r8
	movl	$.LC43, %r10d
	cmovne	%r10, %rdi
	jmp	.L2526
.L12229:
	movl	$.LC40, %edi
	jmp	.L2526
.L12228:
	cmpb	$34, 16(%r13)
	je	.L2520
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L2521
	movq	56(%rax), %rax
.L2521:
	movzbl	66(%rax), %ebx
	andl	$15, %ebx
	decl	%ebx
	jne	.L2519
	movl	$.LC40, %edi
	jmp	.L11007
	.p2align 6,,7
.L12227:
	movzbl	53(%r13), %r11d
	andb	$9, %r11b
	decb	%r11b
	je	.L12236
.L2509:
	testq	%r12, %r12
	jne	.L2517
	testq	%r8, %r8
	jne	.L2517
	testb	$1, 53(%r13)
	je	.L2517
	testb	$8, 18(%r13)
	je	.L2517
	orb	$8, 18(%r15)
	jmp	.L2517
	.p2align 6,,7
.L12236:
	testq	%r8, %r8
	je	.L2509
	cmpb	$29, 16(%r13)
	jne	.L2509
	cmpb	$29, 16(%r8)
	jne	.L2509
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12237
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L2512
	movzbl	53(%r13), %esi
	leal	0(,%rax,8), %ecx
	leaq	88(%r13), %rdx
	andb	$-9, %sil
	orb	%cl, %sil
	movb	%sil, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L2513
	movq	88(%r8), %rax
.L2514:
	movq	136(%r8), %rsi
	movq	72(%r8), %r11
	movq	%rax, (%rdx)
	movq	80(%r8), %r10
	movzbl	17(%r13), %edi
	movq	%rsi, 136(%r13)
	movq	%r11, 72(%r13)
	movq	%r10, 80(%r13)
	movzbl	17(%r8), %r9d
	movq	%r8, 96(%r13)
	andb	$127, %dil
	shrb	$7, %r9b
	movzbl	%r9b, %edx
	movl	%edx, %ebx
	salb	$7, %bl
	orb	%bl, %dil
	movb	%dil, 17(%r13)
	movzbl	53(%r8), %ecx
.L2512:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L2515
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L2515:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L2509
	cmpq	$0, 88(%r8)
	je	.L2509
	movq	8(%r13), %rdi
	cmpq	$0, 24(%rdi)
	jne	.L2509
	movq	%rdx, 8(%r13)
	jmp	.L2509
.L2513:
	xorl	%eax, %eax
	jmp	.L2514
.L12237:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L2509
	.p2align 6,,7
.L12226:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2506
	jmp	.L11006
.L12225:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12238
.L2481:
	cmpq	$0, 40(%r15)
	jne	.L2482
	testb	$8, 18(%r13)
	je	.L2482
	orb	$8, 18(%r15)
.L2482:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12239
.L2484:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L2483:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2495
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2485
	testb	$1, 18(%rcx)
	je	.L2485
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L2485:
	testq	%rax, %rax
	je	.L2495
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2490
	testb	$8, 17(%rcx)
	je	.L2490
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L2490:
	testq	%rax, %rax
	je	.L2495
	cmpq	$0, 8(%rax)
	je	.L2495
	cmpb	$29, %dl
	je	.L12240
.L2498:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L2495:
	testb	$8, 18(%r15)
	je	.L2504
	cmpb	$32, %dl
	je	.L2504
	testb	$8, 18(%r13)
	jne	.L2504
	testb	$1, 53(%r13)
	jne	.L2504
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2500
	cmpq	$0, 8(%rax)
	jne	.L12241
.L2500:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11230:
	xorl	%eax, %eax
	call	warning
	jmp	.L10433
.L12241:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11230
.L12240:
	movq	8(%r13), %r10
	movq	integer_type_node(%rip), %r9
	cmpq	%r9, 8(%r10)
	jne	.L2498
	jmp	.L2495
	.p2align 6,,7
.L12239:
	cmpq	$0, -2016(%rbp)
	je	.L2484
	movq	-2016(%rbp), %r12
	cmpb	$32, 16(%r12)
	jne	.L2483
	jmp	.L2484
.L12238:
	testb	$8, 54(%r13)
	jne	.L2481
	andb	$-9, 18(%r13)
	jmp	.L2481
	.p2align 6,,7
.L10432:
	movq	global_binding_level(%rip), %rax
	jmp	.L2461
.L12224:
	testl	%esi, %esi
	jg	.L11005
	testl	%ebx, %ebx
	je	.L2467
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11005
	.p2align 6,,7
.L2475:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L2467
.L12223:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2467
	testq	%rax, %rax
	je	.L10064
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2465
	.p2align 6,,7
.L12222:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L2458
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L2458
	.p2align 6,,7
.L10904:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rax
	testq	%rax, %rax
	movq	%rax, -2000(%rbp)
	je	.L2322
	movzbl	16(%rax), %eax
	cmpb	$32, %al
	je	.L2321
.L2322:
	movq	global_binding_level(%rip), %rcx
	movq	%r13, -2000(%rbp)
	cmpq	%rcx, current_binding_level(%rip)
	jne	.L10425
	movq	%r13, 80(%rdx)
.L10425:
	movzbl	16(%r13), %eax
.L2325:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-2000(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L2449
	cmpq	$0, 72(%rax)
	je	.L12242
.L2449:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L2320
	cmpq	$0, 56(%rax)
	je	.L2320
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %r10
	cmpq	global_binding_level(%rip), %rbx
	movq	%r10, -608(%rbp)
	je	.L2454
	movq	-608(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L11004:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2455:
	movq	-608(%rbp), %rsi
	movq	%r12, 8(%rsi)
	jmp	.L2320
.L2454:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2455
	movq	-608(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L11004
.L12242:
	movq	8(%r13), %rdi
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%rdi, -600(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L2450
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-600(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L2449
.L2450:
	movq	%rbx, 72(%r13)
	jmp	.L2449
.L2321:
	movq	-2000(%rbp), %rbx
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rbx), %r11
	movq	%r11, -560(%rbp)
	jne	.L2325
	movq	-560(%rbp), %rdi
	movq	32(%rdi), %rcx
	cmpb	$36, (%rcx)
	jne	.L2325
	cmpb	$95, 1(%rcx)
	jne	.L2325
	movq	class_binding_level(%rip), %rax
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%rax, %rax
	movq	%rax, -1984(%rbp)
	movq	%rdx, -568(%rbp)
	jne	.L2329
	testb	$-128, 66(%rsi)
	movq	%rsi, -1984(%rbp)
	je	.L2329
.L2333:
	movq	-1984(%rbp), %r12
	movq	56(%r12), %r9
	testb	$-128, 66(%r9)
	movq	%r9, -1984(%rbp)
	jne	.L2333
.L2329:
	movq	-1984(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12243
	movq	-1984(%rbp), %r8
	movq	-560(%rbp), %rdi
	xorl	%eax, %eax
	movq	-568(%rbp), %rsi
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-1984(%rbp), %r10
	movq	%rax, 8(%r10)
.L2335:
	cmpq	$0, -560(%rbp)
	je	.L2336
	movq	-568(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L2337
	movq	-560(%rbp), %rsi
	movq	%rsi, 80(%rcx)
.L2337:
	movq	-560(%rbp), %rcx
	movq	-568(%rbp), %rax
	cmpq	%rax, 8(%rcx)
	je	.L2338
	cmpb	$21, 16(%rax)
	je	.L12244
.L2339:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L2341
	cmpq	$0, 32(%rax)
	je	.L2340
.L2341:
	movq	lang_name_cplusplus(%rip), %rdi
	cmpq	%rdi, current_lang_name(%rip)
	je	.L12245
.L2342:
	xorl	%ecx, %ecx
.L2377:
	testq	%rcx, %rcx
	jne	.L2378
.L10231:
	movq	-560(%rbp), %rsi
	movq	-568(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-568(%rbp), %rdi
	movq	%rax, -1992(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1992(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2379
	movq	-560(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L11001:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2380:
	movq	-568(%rbp), %r12
	movq	-560(%rbp), %rdx
	movq	%r12, 8(%rdx)
.L2383:
	movq	-560(%rbp), %r10
	movq	32(%r10), %rax
	cmpb	$36, (%rax)
	je	.L12246
.L2385:
	movq	-568(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L2416
	cmpb	$32, 16(%rdx)
	je	.L12247
.L2386:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2405
	movq	-1992(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10427
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2407
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2407:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1992(%rbp), %r11
	leaq	8(%rdx), %rsi
	movq	%r11, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rsi
	ja	.L12248
.L2409:
	movq	-1992(%rbp), %rbx
	movq	%rdx, %rdi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rdi)
	cmpb	$32, 16(%rbx)
	je	.L12249
.L10428:
	movq	-560(%rbp), %r10
	movq	32(%r10), %rax
.L2416:
	cmpb	$36, (%rax)
	je	.L12250
.L2430:
	movq	-568(%rbp), %rdx
	movq	-1992(%rbp), %rcx
	movq	%rcx, 80(%rdx)
	movq	current_class_type(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L2433
	cmpq	$0, current_function_decl(%rip)
	je	.L2432
.L2433:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L2431
.L2432:
	movq	-560(%rbp), %r12
	movq	-1992(%rbp), %rdi
	movq	%r12, 72(%rdi)
.L2338:
	movq	-1984(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12251
.L2336:
	movq	-568(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12252
	movq	-568(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-568(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-2000(%rbp), %r11
	movzbl	16(%r11), %eax
	jmp	.L2325
.L12252:
	movq	%rax, (%rdx)
	movq	-2000(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L2325
.L12251:
	movq	-568(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rdx
	movq	-560(%rbp), %r12
	movq	current_class_type(%rip), %rax
	movq	%rdx, 56(%r12)
	cmpq	$0, 32(%rax)
	jne	.L2336
	movq	-1984(%rbp), %r8
	movq	144(%rax), %r9
	movq	8(%r8), %r10
	movq	%r10, 72(%r9)
	jmp	.L2336
.L2431:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12253
	cmpq	$0, 32(%rdx)
	jne	.L2338
	movq	-1992(%rbp), %r8
	movq	80(%rdx), %r11
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	movq	72(%r11), %rbx
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2441
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-560(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-560(%rbp), %r12
	movl	24(%r12), %eax
	movq	32(%r12), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1992(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-568(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2443:
	movq	-1992(%rbp), %rbx
	movq	current_class_type(%rip), %r11
	movq	152(%rbx), %r8
	movq	%r11, 64(%rbx)
	movq	%r11, 16(%r8)
	jmp	.L2338
.L2441:
	movq	-560(%rbp), %r10
	movq	-1992(%rbp), %rsi
	movq	%r10, 72(%rsi)
	jmp	.L2443
.L12253:
	movq	-1992(%rbp), %r9
	movq	112(%rax), %rbx
	cmpb	$32, 16(%r9)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2436
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-560(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-560(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1992(%rbp), %r10
	movq	%rax, 72(%r10)
	movq	-568(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2438:
	movq	current_function_decl(%rip), %rsi
	movq	-1992(%rbp), %r9
	movq	%rsi, 64(%r9)
	jmp	.L2338
.L2436:
	movq	-560(%rbp), %r12
	movq	-1992(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L2438
.L12250:
	cmpb	$95, 1(%rax)
	jne	.L2430
	movq	-1992(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L2430
.L12249:
	cmpq	$0, 72(%rbx)
	je	.L12254
.L10429:
	movq	-560(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L2416
.L12254:
	movq	-1992(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %r9
	xorl	%eax, %eax
	movq	%r9, -576(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2411
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1992(%rbp), %r10
	movq	%rax, 72(%r10)
	movq	-576(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10426:
	movq	-560(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L2416
.L2411:
	movq	-1992(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10428
.L12248:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2409
.L10427:
	movq	-560(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L2416
.L2405:
	movq	-1992(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1992(%rbp)
	jmp	.L10429
.L12247:
	movq	global_binding_level(%rip), %rsi
	movl	$1, %r12d
	cmpq	%rsi, current_binding_level(%rip)
	je	.L2387
	movq	-560(%rbp), %rdi
	movq	48(%rdi), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L2388
.L2387:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2389
	movq	-560(%rbp), %rbx
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10061
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L2390
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12255
.L2390:
	testq	%rcx, %rcx
	jne	.L10061
.L10062:
	movq	-560(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10061
	movq	-560(%rbp), %rax
	movq	40(%rax), %rcx
.L2388:
	testq	%rcx, %rcx
	je	.L2392
.L10061:
	cmpb	$32, 16(%rcx)
	je	.L2392
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2392
	movq	-560(%rbp), %r10
	movq	8(%r10), %rax
	testq	%rax, %rax
	je	.L2400
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11002
	testl	%r12d, %r12d
	jle	.L12256
.L11002:
	movq	%rax, %rcx
.L2392:
	movq	-568(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L2386
	jmp	.L10426
.L12256:
	testl	%edx, %edx
	jg	.L11002
	testl	%r12d, %r12d
	je	.L2392
	movq	-560(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11002
.L2400:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L2392
.L12255:
	xorl	%ecx, %ecx
	movq	-560(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2392
	testq	%rax, %rax
	je	.L10062
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L2390
.L2389:
	movq	-560(%rbp), %rdx
	movq	40(%rdx), %rcx
	jmp	.L2388
.L12246:
	cmpb	$95, 1(%rax)
	jne	.L2385
	jmp	.L2416
.L2379:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2380
	movq	-560(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L11001
.L2378:
	movq	80(%rcx), %rax
	movq	%rax, -1992(%rbp)
	jmp	.L2383
.L12245:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L2344
	movq	80(%rax), %rbx
.L2344:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2377
.L2376:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L2353
	cmpl	$32, %eax
	je	.L12257
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L2347:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2377
	jmp	.L2376
.L12257:
	movq	8(%rbx), %r9
	movq	-568(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r9), %rdx
	movq	72(%rdx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10878
	movq	64(%rbx), %rbx
	jmp	.L2347
.L10878:
	movq	32(%rax), %rcx
	jmp	.L2377
.L2353:
	movq	-568(%rbp), %rax
	movq	80(%rax), %r12
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L2342
	movq	global_binding_level(%rip), %r10
	cmpq	%r10, current_binding_level(%rip)
	je	.L2356
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L2357
.L2356:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2358
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10059
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2359
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12258
.L2359:
	testq	%rcx, %rcx
	jne	.L10059
.L10060:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10059
.L2358:
	movq	40(%rbx), %rcx
.L2357:
	testq	%rcx, %rcx
	je	.L10231
.L10059:
	cmpb	$32, 16(%rcx)
	je	.L2377
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2377
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L2369
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L11000
	movl	$1, %r11d
	testl	%r11d, %r11d
	jle	.L12259
.L11000:
	movq	%rax, %rcx
	jmp	.L2377
.L12259:
	testl	%edx, %edx
	jg	.L11000
	movl	$1, %edi
	testl	%edi, %edi
	je	.L2377
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L11000
.L2369:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L2377
	jmp	.L11000
.L12258:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2377
	testq	%rax, %rax
	je	.L10060
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2359
.L2340:
	movq	-560(%rbp), %rsi
	movq	-568(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -584(%rbp)
	je	.L2417
	movq	-560(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L11003:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2418:
	movq	-568(%rbp), %r9
	movq	-560(%rbp), %rsi
	movq	%r9, 8(%rsi)
	movq	-584(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L2421
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2422
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2422:
	movq	decl_obstack+24(%rip), %rdx
	movq	-584(%rbp), %r10
	leaq	8(%rdx), %rbx
	movq	%r10, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12260
.L2424:
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-584(%rbp), %rdx
	movq	%rdx, (%r8)
	cmpb	$32, 16(%rdx)
	je	.L12261
.L2421:
	movq	-584(%rbp), %r12
	movq	%r12, -1992(%rbp)
	jmp	.L10429
.L12261:
	cmpq	$0, 72(%rdx)
	jne	.L2421
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -592(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2426
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-584(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-592(%rbp), %rdx
	movq	%rdx, 8(%rax)
	jmp	.L2421
.L2426:
	movq	-584(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L2421
.L12260:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2424
.L2417:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2418
	movq	-560(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L11003
.L12244:
	cmpq	$0, class_binding_level(%rip)
	je	.L2339
	movq	144(%rax), %r11
	testb	$16, 3(%r11)
	jne	.L2338
	jmp	.L2339
.L12243:
	movq	-560(%rbp), %rdi
	movq	-568(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L2335
	.p2align 6,,7
.L2306:
	movq	-2016(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10420
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L10424
	testb	$8, 18(%r15)
	je	.L10424
	testb	$8, 18(%r13)
	jne	.L10424
	testb	$9, 53(%r13)
	jne	.L10424
	cmpq	%r13, current_function_decl(%rip)
	je	.L12262
.L2315:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2316
	cmpq	$0, 8(%rax)
	jne	.L12263
.L2316:
	movq	-2016(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10999:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-2016(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10424
.L12263:
	movq	-2016(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10999
.L12262:
	movq	-2016(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L2315
	.p2align 6,,7
.L12221:
	cmpq	$0, 64(%rdx)
	jne	.L2304
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L2304
.L12220:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -2016(%rbp)
	call	error_with_decl
	jmp	.L2302
.L2295:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L2297
.L2301:
	cmpq	%r15, 56(%rax)
	je	.L2297
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L2301
.L2297:
	movq	%rax, -2016(%rbp)
	jmp	.L2294
.L12219:
	movq	40(%r15), %r12
	movq	%r12, -2016(%rbp)
	jmp	.L2294
.L12218:
	movq	56(%r13), %r15
	jmp	.L2291
.L12217:
	testb	$32, 53(%r13)
	jne	.L2289
	jmp	.L2290
.L10416:
	movzbl	16(%r13), %edx
	jmp	.L2289
.L11266:
	leal	(%rcx,%rcx), %edi
	xorl	%eax, %eax
	movl	%edi, builtin_type_tdescs_max(%rip)
	movslq	%edi,%rsi
	movq	builtin_type_tdescs_arr(%rip), %rdi
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L2282
.L2272:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2273
	movq	-1920(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10997
.L2271:
	movq	-1920(%rbp), %rsi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2277
	movq	-1920(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L10998:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2278:
	movq	-1920(%rbp), %r11
	movq	$0, 8(%r11)
	jmp	.L2270
.L2277:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2278
	movq	-1920(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10998
	.p2align 6,,7
.L11265:
	movq	-1928(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r8
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r8, -1960(%rbp)
	cmpq	%rax, %r13
	je	.L10397
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12264
.L1990:
	movq	%rax, 64(%r13)
.L1989:
	cmpb	$32, %dl
	je	.L12265
.L1991:
	testq	%r15, %r15
	je	.L1992
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12266
	cmpq	$0, 48(%r15)
	jne	.L1995
	movq	$0, -1968(%rbp)
.L1994:
	cmpq	$0, -1968(%rbp)
	je	.L2012
	movq	-1968(%rbp), %r11
	cmpq	error_mark_node(%rip), %r11
	je	.L12267
.L2002:
	cmpq	$0, -1968(%rbp)
	je	.L10401
	movq	-1968(%rbp), %rsi
	cmpb	$34, 16(%rsi)
	je	.L12268
.L2004:
	movq	-1968(%rbp), %rax
	testq	%rax, %rax
	movq	24(%rax), %r12
	movl	32(%rax), %ebx
	je	.L10401
	movzbl	16(%rax), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L2006
	cmpb	$32, %al
	je	.L2012
	cmpb	$32, %dl
	je	.L10903
	movq	-1968(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10401
.L10405:
	movq	global_binding_level(%rip), %rax
.L2011:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L2265
	movq	-1928(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10996:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2266:
	movq	-1928(%rbp), %rdx
	movl	$13, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L1988
	movq	-1968(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L1988
.L2265:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2266
	movq	-1928(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L10996
.L10401:
	movzbl	16(%r13), %edx
.L2012:
	cmpb	$32, %dl
	je	.L10903
.L2020:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L2158
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2158
	testb	$1, 53(%rax)
	jne	.L2159
	testb	$8, 18(%rax)
	je	.L2158
.L2159:
	andb	$8, %dl
	je	.L12269
.L2158:
	movl	flag_traditional(%rip), %ecx
	testl	%ecx, %ecx
	je	.L10413
	testb	$1, 53(%r13)
	je	.L10413
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L2162
	movq	48(%r15), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L2163
.L2162:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2164
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10057
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2165
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L12270
.L2165:
	testq	%rcx, %rcx
	jne	.L10057
.L10058:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10057
.L2164:
	movq	40(%r15), %rcx
.L2163:
	testq	%rcx, %rcx
	je	.L10230
.L10057:
	cmpb	$32, 16(%rcx)
	je	.L2167
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L2167
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L2175
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10993
	testl	%ebx, %ebx
	jle	.L12271
.L10993:
	movq	%rax, %rcx
.L2167:
	testq	%rcx, %rcx
	jne	.L10413
.L10230:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1960(%rbp)
.L2161:
	cmpq	%rax, -1960(%rbp)
	je	.L12272
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12273
.L10994:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L2206:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12274
.L2217:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L2219
	testq	%r12, %r12
	je	.L2220
	testb	$1, 53(%r13)
	jne	.L2220
	cmpb	$34, 16(%r12)
	je	.L12275
.L2220:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L2219
	testb	$1, 53(%r13)
	jne	.L2219
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L2219
	testq	%rax, %rax
	jne	.L2219
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L2225
	cmpb	$34, 16(%r12)
	je	.L12276
.L2225:
	cmpq	$0, 56(%r15)
	je	.L2227
	movl	$.LC41, %edi
.L2226:
	testq	%rdi, %rdi
	jne	.L10995
.L2219:
	testq	%r12, %r12
	je	.L10414
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-1960(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10414:
	movzbl	16(%r13), %edx
.L2204:
	leal	-128(%rdx), %r10d
	cmpb	$1, %r10b
	jbe	.L1992
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L1992
	cmpb	$18, 16(%rcx)
	je	.L12277
.L2236:
	testb	$64, 46(%rcx)
	je	.L1992
.L2235:
	movq	-1960(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12278
.L10415:
	movzbl	16(%r13), %edx
.L1992:
	cmpb	$32, %dl
	je	.L12279
.L2238:
	movq	-1960(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rdi
	cmpq	%rax, %rbx
	movq	%rdi, (%r13)
	movq	%r13, (%rbx)
	je	.L12280
.L2264:
	movq	%r13, -1968(%rbp)
	jmp	.L2011
.L12280:
	testb	$4, 17(%r13)
	jne	.L2264
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L2264
.L12279:
	testq	%r15, %r15
	je	.L2238
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2239
	cmpq	class_binding_level(%rip), %rax
	je	.L2240
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L2244
	cmpb	$32, 16(%rax)
	je	.L2242
.L2244:
	cmpq	$0, current_class_type(%rip)
	je	.L2239
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L2239
	cmpb	$32, 16(%rax)
	je	.L2242
.L2239:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L2243
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2242
	cmpb	$-127, %dl
	je	.L12281
.L2243:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L2238
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12282
.L2250:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L2254
	cmpq	class_binding_level(%rip), %rax
	je	.L2255
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L2259
	cmpb	$32, 16(%rax)
	je	.L2257
.L2259:
	cmpq	$0, current_class_type(%rip)
	je	.L2254
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L2254
	cmpb	$32, 16(%rax)
	je	.L2257
.L2254:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L2238
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L2257
	cmpb	$-127, %dl
	jne	.L2238
	movq	$0, 8(%rbx)
	jmp	.L2238
.L2257:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L2238
.L2255:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2259
.L12282:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r11b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L2250
.L12281:
	movq	$0, 8(%r15)
	jmp	.L2243
.L2242:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r15)
	jmp	.L2243
.L2240:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L2244
.L12278:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10415
.L12277:
	movq	8(%rcx), %rsi
	testb	$64, 46(%rsi)
	jne	.L2235
	jmp	.L2236
.L10995:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L2219
.L2227:
	testq	%r12, %r12
	je	.L2229
	movl	$.LC42, %edi
	jmp	.L2226
.L2229:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L2226
.L12276:
	movl	$.LC40, %edi
	jmp	.L2226
.L12275:
	cmpb	$34, 16(%r13)
	je	.L2220
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L2221
	movq	56(%rax), %rax
.L2221:
	movzbl	66(%rax), %r11d
	andl	$15, %r11d
	decl	%r11d
	jne	.L2219
	movl	$.LC40, %edi
	jmp	.L10995
	.p2align 6,,7
.L12274:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12283
.L2209:
	testq	%r12, %r12
	jne	.L2217
	testq	%r8, %r8
	jne	.L2217
	testb	$1, 53(%r13)
	je	.L2217
	testb	$8, 18(%r13)
	je	.L2217
	orb	$8, 18(%r15)
	jmp	.L2217
	.p2align 6,,7
.L12283:
	testq	%r8, %r8
	je	.L2209
	cmpb	$29, 16(%r13)
	jne	.L2209
	cmpb	$29, 16(%r8)
	jne	.L2209
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12284
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L2212
	movzbl	53(%r13), %r10d
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %r10b
	orb	%sil, %r10b
	movb	%r10b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L2213
	movq	88(%r8), %rax
.L2214:
	movq	%rax, (%rdx)
	movq	136(%r8), %r10
	movq	80(%r8), %r9
	movq	72(%r8), %rdx
	movzbl	17(%r13), %ecx
	movq	%r10, 136(%r13)
	movq	%r9, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %cl
	shrb	$7, %dil
	movzbl	%dil, %ebx
	movl	%ebx, %r11d
	salb	$7, %r11b
	orb	%r11b, %cl
	movb	%cl, 17(%r13)
	movzbl	53(%r8), %ecx
.L2212:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L2215
	movzbl	53(%r13), %esi
	salb	$4, %al
	andb	$-17, %sil
	orb	%al, %sil
	movb	%sil, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L2215:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L2209
	cmpq	$0, 88(%r8)
	je	.L2209
	movq	8(%r13), %rcx
	cmpq	$0, 24(%rcx)
	jne	.L2209
	movq	%rdx, 8(%r13)
	jmp	.L2209
.L2213:
	xorl	%eax, %eax
	jmp	.L2214
.L12284:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L2209
	.p2align 6,,7
.L12273:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2206
	jmp	.L10994
.L12272:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12285
.L2181:
	cmpq	$0, 40(%r15)
	jne	.L2182
	testb	$8, 18(%r13)
	je	.L2182
	orb	$8, 18(%r15)
.L2182:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12286
.L2184:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L2183:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2195
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2185
	testb	$1, 18(%rcx)
	je	.L2185
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L2185:
	testq	%rax, %rax
	je	.L2195
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L2190
	testb	$8, 17(%rcx)
	je	.L2190
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L2190:
	testq	%rax, %rax
	je	.L2195
	cmpq	$0, 8(%rax)
	je	.L2195
	cmpb	$29, %dl
	je	.L12287
.L2198:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L2195:
	testb	$8, 18(%r15)
	je	.L2204
	cmpb	$32, %dl
	je	.L2204
	testb	$8, 18(%r13)
	jne	.L2204
	testb	$1, 53(%r13)
	jne	.L2204
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2200
	cmpq	$0, 8(%rax)
	jne	.L12288
.L2200:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11229:
	xorl	%eax, %eax
	call	warning
	jmp	.L10414
.L12288:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11229
.L12287:
	movq	8(%r13), %r9
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r9)
	jne	.L2198
	jmp	.L2195
	.p2align 6,,7
.L12286:
	cmpq	$0, -1968(%rbp)
	je	.L2184
	movq	-1968(%rbp), %r12
	cmpb	$32, 16(%r12)
	jne	.L2183
	jmp	.L2184
.L12285:
	testb	$8, 54(%r13)
	jne	.L2181
	andb	$-9, 18(%r13)
	jmp	.L2181
	.p2align 6,,7
.L10413:
	movq	global_binding_level(%rip), %rax
	jmp	.L2161
.L12271:
	testl	%esi, %esi
	jg	.L10993
	testl	%ebx, %ebx
	je	.L2167
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10993
	.p2align 6,,7
.L2175:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L2167
.L12270:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2167
	testq	%rax, %rax
	je	.L10058
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2165
	.p2align 6,,7
.L12269:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L2158
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L2158
	.p2align 6,,7
.L10903:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, -1952(%rbp)
	je	.L2022
	movzbl	16(%rbx), %eax
	cmpb	$32, %al
	je	.L2021
.L2022:
	movq	global_binding_level(%rip), %r10
	movq	%r13, -1952(%rbp)
	cmpq	%r10, current_binding_level(%rip)
	jne	.L10406
	movq	%r13, 80(%rdx)
.L10406:
	movzbl	16(%r13), %eax
.L2025:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-1952(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L2149
	cmpq	$0, 72(%rax)
	je	.L12289
.L2149:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L2020
	cmpq	$0, 56(%rax)
	je	.L2020
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -552(%rbp)
	je	.L2154
	movq	-552(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
.L10992:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2155:
	movq	-552(%rbp), %rsi
	movq	%r12, 8(%rsi)
	jmp	.L2020
.L2154:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2155
	movq	-552(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10992
.L12289:
	movq	8(%r13), %r8
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r8, -544(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L2150
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-544(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L2149
.L2150:
	movq	%rbx, 72(%r13)
	jmp	.L2149
.L2021:
	movq	-1952(%rbp), %r12
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r12), %rcx
	movq	%rcx, -504(%rbp)
	jne	.L2025
	movq	32(%rcx), %rcx
	cmpb	$36, (%rcx)
	jne	.L2025
	cmpb	$95, 1(%rcx)
	jne	.L2025
	movq	class_binding_level(%rip), %rbx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rcx
	testq	%rbx, %rbx
	movq	%rbx, -1936(%rbp)
	movq	%rcx, -512(%rbp)
	jne	.L2029
	testb	$-128, 66(%rsi)
	movq	%rsi, -1936(%rbp)
	je	.L2029
.L2033:
	movq	-1936(%rbp), %rsi
	movq	56(%rsi), %rdx
	testb	$-128, 66(%rdx)
	movq	%rdx, -1936(%rbp)
	jne	.L2033
.L2029:
	movq	-1936(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12290
	movq	-1936(%rbp), %r9
	movq	-504(%rbp), %rdi
	xorl	%eax, %eax
	movq	-512(%rbp), %rsi
	movq	8(%r9), %rdx
	call	saveable_tree_cons
	movq	-1936(%rbp), %r11
	movq	%rax, 8(%r11)
.L2035:
	cmpq	$0, -504(%rbp)
	je	.L2036
	movq	-512(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L2037
	movq	-504(%rbp), %rax
	movq	%rax, 80(%rcx)
.L2037:
	movq	-504(%rbp), %rdi
	movq	-512(%rbp), %rax
	cmpq	%rax, 8(%rdi)
	je	.L2038
	cmpb	$21, 16(%rax)
	je	.L12291
.L2039:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L2041
	cmpq	$0, 32(%rax)
	je	.L2040
.L2041:
	movq	lang_name_cplusplus(%rip), %r8
	cmpq	%r8, current_lang_name(%rip)
	je	.L12292
.L2042:
	xorl	%ecx, %ecx
.L2077:
	testq	%rcx, %rcx
	jne	.L2078
.L10229:
	movq	-504(%rbp), %rsi
	movq	-512(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-512(%rbp), %rdi
	movq	%rax, -1944(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1944(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L2079
	movq	-504(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L10989:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2080:
	movq	-512(%rbp), %rsi
	movq	-504(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L2083:
	movq	-504(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$36, (%rax)
	je	.L12293
.L2085:
	movq	-512(%rbp), %rdi
	movq	80(%rdi), %rdx
	testq	%rdx, %rdx
	je	.L2116
	cmpb	$32, 16(%rdx)
	je	.L12294
.L2086:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2105
	movq	-1944(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10408
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2107
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2107:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1944(%rbp), %r8
	leaq	8(%rdx), %r10
	movq	%r8, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r10
	ja	.L12295
.L2109:
	movq	-1944(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L12296
.L10409:
	movq	-504(%rbp), %r11
	movq	32(%r11), %rax
.L2116:
	cmpb	$36, (%rax)
	je	.L12297
.L2130:
	movq	current_class_type(%rip), %rdx
	movq	-1944(%rbp), %rdi
	movq	-512(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rdi, 80(%rcx)
	jne	.L2133
	cmpq	$0, current_function_decl(%rip)
	je	.L2132
.L2133:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L2131
.L2132:
	movq	-504(%rbp), %r12
	movq	-1944(%rbp), %r8
	movq	%r12, 72(%r8)
.L2038:
	movq	-1936(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12298
.L2036:
	movq	-512(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12299
	movq	-512(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-512(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-1952(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L2025
.L12299:
	movq	%rax, (%rdx)
	movq	-1952(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L2025
.L12298:
	movq	-512(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r12
	movq	-504(%rbp), %r8
	movq	current_class_type(%rip), %rax
	movq	%r12, 56(%r8)
	cmpq	$0, 32(%rax)
	jne	.L2036
	movq	-1936(%rbp), %r9
	movq	144(%rax), %r11
	movq	8(%r9), %rdx
	movq	%rdx, 72(%r11)
	jmp	.L2036
.L2131:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12300
	cmpq	$0, 32(%rdx)
	jne	.L2038
	movq	-1944(%rbp), %r9
	movq	80(%rdx), %r10
	movl	$136, %esi
	cmpb	$32, 16(%r9)
	movq	72(%r10), %rbx
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2141
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-504(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-504(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1944(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-512(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2143:
	movq	-1944(%rbp), %rbx
	movq	current_class_type(%rip), %r10
	movq	152(%rbx), %r9
	movq	%r10, 64(%rbx)
	movq	%r10, 16(%r9)
	jmp	.L2038
.L2141:
	movq	-504(%rbp), %r11
	movq	-1944(%rbp), %rsi
	movq	%r11, 72(%rsi)
	jmp	.L2143
.L12300:
	movq	-1944(%rbp), %rdx
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rdx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2136
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-504(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-504(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1944(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-512(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L2138:
	movq	current_function_decl(%rip), %rsi
	movq	-1944(%rbp), %rdx
	movq	%rsi, 64(%rdx)
	jmp	.L2038
.L2136:
	movq	-504(%rbp), %r12
	movq	-1944(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L2138
.L12297:
	cmpb	$95, 1(%rax)
	jne	.L2130
	movq	-1944(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L2130
.L12296:
	cmpq	$0, 72(%rbx)
	je	.L12301
.L10410:
	movq	-504(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L2116
.L12301:
	movq	-1944(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -520(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2111
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1944(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-520(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10407:
	movq	-504(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L2116
.L2111:
	movq	-1944(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L10409
.L12295:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2109
.L10408:
	movq	-504(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L2116
.L2105:
	movq	-1944(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1944(%rbp)
	jmp	.L10410
.L12294:
	movq	global_binding_level(%rip), %r10
	movl	$1, %r12d
	cmpq	%r10, current_binding_level(%rip)
	je	.L2087
	movq	-504(%rbp), %rbx
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L2088
.L2087:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2089
	movq	-504(%rbp), %rdx
	movq	56(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.L10055
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L2090
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12302
.L2090:
	testq	%rcx, %rcx
	jne	.L10055
.L10056:
	movq	-504(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10055
	movq	-504(%rbp), %rax
	movq	40(%rax), %rcx
.L2088:
	testq	%rcx, %rcx
	je	.L2092
.L10055:
	cmpb	$32, 16(%rcx)
	je	.L2092
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2092
	movq	-504(%rbp), %r9
	movq	8(%r9), %rax
	testq	%rax, %rax
	je	.L2100
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10990
	testl	%r12d, %r12d
	jle	.L12303
.L10990:
	movq	%rax, %rcx
.L2092:
	movq	-512(%rbp), %rdi
	cmpq	80(%rdi), %rcx
	jne	.L2086
	jmp	.L10407
.L12303:
	testl	%edx, %edx
	jg	.L10990
	testl	%r12d, %r12d
	je	.L2092
	movq	-504(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10990
.L2100:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L2092
.L12302:
	xorl	%ecx, %ecx
	movq	-504(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2092
	testq	%rax, %rax
	je	.L10056
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L2090
.L2089:
	movq	-504(%rbp), %r11
	movq	40(%r11), %rcx
	jmp	.L2088
.L12293:
	cmpb	$95, 1(%rax)
	jne	.L2085
	jmp	.L2116
.L2079:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2080
	movq	-504(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10989
.L2078:
	movq	80(%rcx), %r11
	movq	%r11, -1944(%rbp)
	jmp	.L2083
.L12292:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L2044
	movq	80(%rax), %rbx
.L2044:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2077
.L2076:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L2053
	cmpl	$32, %eax
	je	.L12304
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L2047:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L2077
	jmp	.L2076
.L12304:
	movq	8(%rbx), %rdx
	movq	-512(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10877
	movq	64(%rbx), %rbx
	jmp	.L2047
.L10877:
	movq	32(%rax), %rcx
	jmp	.L2077
.L2053:
	movq	-512(%rbp), %r11
	movq	80(%r11), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L2042
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L2056
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L2057
.L2056:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L2058
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10053
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L2059
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L12305
.L2059:
	testq	%rcx, %rcx
	jne	.L10053
.L10054:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10053
.L2058:
	movq	40(%rbx), %rcx
.L2057:
	testq	%rcx, %rcx
	je	.L10229
.L10053:
	cmpb	$32, 16(%rcx)
	je	.L2077
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L2077
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L2069
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10988
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L12306
.L10988:
	movq	%rax, %rcx
	jmp	.L2077
.L12306:
	testl	%edx, %edx
	jg	.L10988
	movl	$1, %r8d
	testl	%r8d, %r8d
	je	.L2077
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10988
.L2069:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L2077
	jmp	.L10988
.L12305:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L2077
	testq	%rax, %rax
	je	.L10054
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L2059
.L2040:
	movq	-504(%rbp), %rsi
	movq	-512(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -528(%rbp)
	je	.L2117
	movq	-504(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10991:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L2118:
	movq	-512(%rbp), %rdx
	movq	-504(%rbp), %rsi
	movq	%rdx, 8(%rsi)
	movq	-528(%rbp), %r8
	movq	56(%r8), %r12
	testq	%r12, %r12
	je	.L2121
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L2122
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L2122:
	movq	decl_obstack+24(%rip), %rdx
	movq	-528(%rbp), %r11
	leaq	8(%rdx), %rbx
	movq	%r11, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12307
.L2124:
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-528(%rbp), %rdx
	movq	%rdx, (%r9)
	cmpb	$32, 16(%rdx)
	je	.L12308
.L2121:
	movq	-528(%rbp), %r12
	movq	%r12, -1944(%rbp)
	jmp	.L10410
.L12308:
	cmpq	$0, 72(%rdx)
	jne	.L2121
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -536(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L2126
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-528(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-536(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L2121
.L2126:
	movq	-528(%rbp), %rsi
	movq	%r12, 72(%rsi)
	jmp	.L2121
.L12307:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L2124
.L2117:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L2118
	movq	-504(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10991
.L12291:
	cmpq	$0, class_binding_level(%rip)
	je	.L2039
	movq	144(%rax), %r10
	testb	$16, 3(%r10)
	jne	.L2038
	jmp	.L2039
.L12290:
	movq	-504(%rbp), %rdi
	movq	-512(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L2035
	.p2align 6,,7
.L2006:
	movq	-1968(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10401
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10405
	testb	$8, 18(%r15)
	je	.L10405
	testb	$8, 18(%r13)
	jne	.L10405
	testb	$9, 53(%r13)
	jne	.L10405
	cmpq	%r13, current_function_decl(%rip)
	je	.L12309
.L2015:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L2016
	cmpq	$0, 8(%rax)
	jne	.L12310
.L2016:
	movq	-1968(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10987:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1968(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10405
.L12310:
	movq	-1968(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10987
.L12309:
	movq	-1968(%rbp), %rdi
	movq	%rdi, current_function_decl(%rip)
	jmp	.L2015
	.p2align 6,,7
.L12268:
	cmpq	$0, 64(%rsi)
	jne	.L2004
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L2004
.L12267:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1968(%rbp)
	call	error_with_decl
	jmp	.L2002
.L1995:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L1997
.L2001:
	cmpq	%r15, 56(%rax)
	je	.L1997
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L2001
.L1997:
	movq	%rax, -1968(%rbp)
	jmp	.L1994
.L12266:
	movq	40(%r15), %rax
	movq	%rax, -1968(%rbp)
	jmp	.L1994
.L12265:
	movq	56(%r13), %r15
	jmp	.L1991
.L12264:
	testb	$32, 53(%r13)
	jne	.L1989
	jmp	.L1990
.L10397:
	movzbl	16(%r13), %edx
	jmp	.L1989
.L1984:
	movl	$.LC48, %edi
	xorl	%eax, %eax
	call	get_identifier
	movq	40(%rax), %rdx
	movq	8(%rdx), %rax
	jmp	.L10986
.L11264:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L1980
.L1970:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1971
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L10984
.L1969:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r15, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12311
.L10985:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1976:
	movq	$0, 8
	jmp	.L1968
.L12311:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1976
	jmp	.L10985
	.p2align 6,,7
.L11263:
	movq	-1872(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r15, %rdx
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_binding_level(%rip), %rax
	movq	%rax, -1904(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10378
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12312
.L1688:
	movq	%rax, 64(%r13)
.L1687:
	cmpb	$32, %dl
	je	.L12313
.L1689:
	testq	%r14, %r14
	je	.L1690
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12314
	cmpq	$0, 48(%r14)
	jne	.L1693
	movq	$0, -1912(%rbp)
.L1692:
	cmpq	$0, -1912(%rbp)
	je	.L1710
	movq	-1912(%rbp), %r8
	cmpq	error_mark_node(%rip), %r8
	je	.L12315
.L1700:
	cmpq	$0, -1912(%rbp)
	je	.L10382
	movq	-1912(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12316
.L1702:
	movq	-1912(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movq	%rcx, %rsi
	movl	32(%rcx), %ebx
	je	.L10382
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L1704
	cmpb	$32, %al
	je	.L1710
	cmpb	$32, %dl
	je	.L10902
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10382
.L10386:
	movq	global_binding_level(%rip), %rax
.L1709:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L1963
	movq	-1872(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10983:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1964:
	movq	-1872(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L1686
	movq	-1912(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L1686
.L1963:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1964
	movq	-1872(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10983
.L10382:
	movzbl	16(%r13), %edx
.L1710:
	cmpb	$32, %dl
	je	.L10902
.L1718:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L1856
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L1856
	testb	$1, 53(%rax)
	jne	.L1857
	testb	$8, 18(%rax)
	je	.L1856
.L1857:
	andb	$8, %dl
	je	.L12317
.L1856:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10394
	testb	$1, 53(%r13)
	je	.L10394
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L1860
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L1861
.L1860:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1862
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10051
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1863
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L12318
.L1863:
	testq	%rcx, %rcx
	jne	.L10051
.L10052:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10051
.L1862:
	movq	40(%r14), %rcx
.L1861:
	testq	%rcx, %rcx
	je	.L10228
.L10051:
	cmpb	$32, 16(%rcx)
	je	.L1865
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L1865
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L1873
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10980
	testl	%ebx, %ebx
	jle	.L12319
.L10980:
	movq	%rax, %rcx
.L1865:
	testq	%rcx, %rcx
	jne	.L10394
.L10228:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1904(%rbp)
.L1859:
	cmpq	%rax, -1904(%rbp)
	je	.L12320
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L12321
.L10981:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L1904:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L12322
.L1915:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L1917
	testq	%r12, %r12
	je	.L1918
	testb	$1, 53(%r13)
	jne	.L1918
	cmpb	$34, 16(%r12)
	je	.L12323
.L1918:
	movl	warn_shadow(%rip), %r9d
	testl	%r9d, %r9d
	je	.L1917
	testb	$1, 53(%r13)
	jne	.L1917
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L1917
	testq	%rax, %rax
	jne	.L1917
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L1923
	cmpb	$34, 16(%r12)
	je	.L12324
.L1923:
	cmpq	$0, 56(%r14)
	je	.L1925
	movl	$.LC41, %edi
.L1924:
	testq	%rdi, %rdi
	jne	.L10982
.L1917:
	testq	%r12, %r12
	je	.L10395
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-1904(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10395:
	movzbl	16(%r13), %edx
.L1902:
	leal	-128(%rdx), %ecx
	cmpb	$1, %cl
	jbe	.L1690
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L1690
	cmpb	$18, 16(%rcx)
	je	.L12325
.L1934:
	testb	$64, 46(%rcx)
	je	.L1690
.L1933:
	movq	-1904(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12326
.L10396:
	movzbl	16(%r13), %edx
.L1690:
	cmpb	$32, %dl
	je	.L12327
.L1936:
	movq	-1904(%rbp), %r9
	movq	global_binding_level(%rip), %rax
	movq	(%r9), %rdi
	cmpq	%rax, %r9
	movq	%rdi, (%r13)
	movq	%r13, (%r9)
	je	.L12328
.L1962:
	movq	%r13, -1912(%rbp)
	jmp	.L1709
.L12328:
	testb	$4, 17(%r13)
	jne	.L1962
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L1962
.L12327:
	testq	%r14, %r14
	je	.L1936
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1937
	cmpq	class_binding_level(%rip), %rax
	je	.L1938
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L1942
	cmpb	$32, 16(%rax)
	je	.L1940
.L1942:
	cmpq	$0, current_class_type(%rip)
	je	.L1937
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L1937
	cmpb	$32, 16(%rax)
	je	.L1940
.L1937:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L1941
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1940
	cmpb	$-127, %dl
	je	.L12329
.L1941:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L1936
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12330
.L1948:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1952
	cmpq	class_binding_level(%rip), %rax
	je	.L1953
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L1957
	cmpb	$32, 16(%rax)
	je	.L1955
.L1957:
	cmpq	$0, current_class_type(%rip)
	je	.L1952
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L1952
	cmpb	$32, 16(%rax)
	je	.L1955
.L1952:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L1936
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1955
	cmpb	$-127, %dl
	jne	.L1936
	movq	$0, 8(%rbx)
	jmp	.L1936
.L1955:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L1936
.L1953:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1957
.L12330:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r11b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L1948
.L12329:
	movq	$0, 8(%r14)
	jmp	.L1941
.L1940:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r14)
	jmp	.L1941
.L1938:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1942
.L12326:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10396
.L12325:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L1933
	jmp	.L1934
.L10982:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L1917
.L1925:
	testq	%r12, %r12
	je	.L1927
	movl	$.LC42, %edi
	jmp	.L1924
.L1927:
	testq	%r8, %r8
	movl	$.LC43, %r10d
	cmovne	%r10, %rdi
	jmp	.L1924
.L12324:
	movl	$.LC40, %edi
	jmp	.L1924
.L12323:
	cmpb	$34, 16(%r13)
	je	.L1918
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L1919
	movq	56(%rax), %rax
.L1919:
	movzbl	66(%rax), %r11d
	andl	$15, %r11d
	decl	%r11d
	jne	.L1917
	movl	$.LC40, %edi
	jmp	.L10982
	.p2align 6,,7
.L12322:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12331
.L1907:
	testq	%r12, %r12
	jne	.L1915
	testq	%r8, %r8
	jne	.L1915
	testb	$1, 53(%r13)
	je	.L1915
	testb	$8, 18(%r13)
	je	.L1915
	orb	$8, 18(%r14)
	jmp	.L1915
	.p2align 6,,7
.L12331:
	testq	%r8, %r8
	je	.L1907
	cmpb	$29, 16(%r13)
	jne	.L1907
	cmpb	$29, 16(%r8)
	jne	.L1907
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12332
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L1910
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %ebx
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%bl, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L1911
	movq	88(%r8), %rax
.L1912:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %r10
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%rcx, 136(%r13)
	movq	%r10, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %dil
	movzbl	%dil, %r9d
	movl	%r9d, %r11d
	salb	$7, %r11b
	orb	%r11b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L1910:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L1913
	movzbl	53(%r13), %ebx
	salb	$4, %al
	andb	$-17, %bl
	orb	%al, %bl
	movb	%bl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L1913:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L1907
	cmpq	$0, 88(%r8)
	je	.L1907
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L1907
	movq	%rdx, 8(%r13)
	jmp	.L1907
.L1911:
	xorl	%eax, %eax
	jmp	.L1912
.L12332:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L1907
	.p2align 6,,7
.L12321:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1904
	jmp	.L10981
.L12320:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12333
.L1879:
	cmpq	$0, 40(%r14)
	jne	.L1880
	testb	$8, 18(%r13)
	je	.L1880
	orb	$8, 18(%r14)
.L1880:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12334
.L1882:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L1881:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L1893
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1883
	testb	$1, 18(%rcx)
	je	.L1883
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L1883:
	testq	%rax, %rax
	je	.L1893
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1888
	testb	$8, 17(%rcx)
	je	.L1888
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L1888:
	testq	%rax, %rax
	je	.L1893
	cmpq	$0, 8(%rax)
	je	.L1893
	cmpb	$29, %dl
	je	.L12335
.L1896:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L1893:
	testb	$8, 18(%r14)
	je	.L1902
	cmpb	$32, %dl
	je	.L1902
	testb	$8, 18(%r13)
	jne	.L1902
	testb	$1, 53(%r13)
	jne	.L1902
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L1898
	cmpq	$0, 8(%rax)
	jne	.L12336
.L1898:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11228:
	xorl	%eax, %eax
	call	warning
	jmp	.L10395
.L12336:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11228
.L12335:
	movq	8(%r13), %r10
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r10)
	jne	.L1896
	jmp	.L1893
	.p2align 6,,7
.L12334:
	cmpq	$0, -1912(%rbp)
	je	.L1882
	movq	-1912(%rbp), %r8
	cmpb	$32, 16(%r8)
	jne	.L1881
	jmp	.L1882
.L12333:
	testb	$8, 54(%r13)
	jne	.L1879
	andb	$-9, 18(%r13)
	jmp	.L1879
	.p2align 6,,7
.L10394:
	movq	global_binding_level(%rip), %rax
	jmp	.L1859
.L12319:
	testl	%esi, %esi
	jg	.L10980
	testl	%ebx, %ebx
	je	.L1865
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10980
	.p2align 6,,7
.L1873:
	movq	8(%rcx), %r9
	cmpq	error_mark_node(%rip), %r9
	cmove	%r9, %rcx
	jmp	.L1865
.L12318:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1865
	testq	%rax, %rax
	je	.L10052
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1863
	.p2align 6,,7
.L12317:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L1856
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L1856
	.p2align 6,,7
.L10902:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -1896(%rbp)
	je	.L1720
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L1719
.L1720:
	movq	global_binding_level(%rip), %r12
	movq	%r13, -1896(%rbp)
	cmpq	%r12, current_binding_level(%rip)
	jne	.L10387
	movq	%r13, 80(%rdx)
.L10387:
	movzbl	16(%r13), %eax
.L1723:
	cmpb	$32, %al
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$140, %esi
	call	my_friendly_assert
	movq	-1896(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L1847
	cmpq	$0, 72(%rax)
	je	.L12337
.L1847:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L1718
	cmpq	$0, 56(%rax)
	je	.L1718
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -496(%rbp)
	je	.L1852
	movq	-496(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L10979:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1853:
	movq	-496(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L1718
.L1852:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1853
	movq	-496(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10979
.L12337:
	movq	8(%r13), %r11
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r11, -488(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L1848
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-488(%rbp), %r9
	movq	%r9, 8(%rax)
	jmp	.L1847
.L1848:
	movq	%rbx, 72(%r13)
	jmp	.L1847
.L1719:
	movq	-1896(%rbp), %rdi
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rdi), %rbx
	movq	%rbx, -448(%rbp)
	jne	.L1723
	movq	-448(%rbp), %r11
	movq	32(%r11), %rcx
	cmpb	$36, (%rcx)
	jne	.L1723
	cmpb	$95, 1(%rcx)
	jne	.L1723
	movq	class_binding_level(%rip), %r9
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%r9, %r9
	movq	%r9, -1880(%rbp)
	movq	%rdx, -456(%rbp)
	jne	.L1727
	testb	$-128, 66(%rsi)
	movq	%rsi, -1880(%rbp)
	je	.L1727
.L1731:
	movq	-1880(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -1880(%rbp)
	jne	.L1731
.L1727:
	movq	-1880(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12338
	movq	-1880(%rbp), %rcx
	movq	-448(%rbp), %rdi
	xorl	%eax, %eax
	movq	-456(%rbp), %rsi
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-1880(%rbp), %r8
	movq	%rax, 8(%r8)
.L1733:
	cmpq	$0, -448(%rbp)
	je	.L1734
	movq	-456(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L1735
	movq	-448(%rbp), %r10
	movq	%r10, 80(%rcx)
.L1735:
	movq	-448(%rbp), %r12
	movq	-456(%rbp), %rax
	cmpq	%rax, 8(%r12)
	je	.L1736
	cmpb	$21, 16(%rax)
	je	.L12339
.L1737:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L1739
	cmpq	$0, 32(%rax)
	je	.L1738
.L1739:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L12340
.L1740:
	xorl	%ecx, %ecx
.L1775:
	testq	%rcx, %rcx
	jne	.L1776
.L10227:
	movq	-448(%rbp), %rsi
	movq	-456(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-456(%rbp), %rdi
	movq	%rax, -1888(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1888(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1777
	movq	-448(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L10976:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1778:
	movq	-456(%rbp), %rsi
	movq	-448(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L1781:
	movq	-448(%rbp), %r8
	movq	32(%r8), %rax
	cmpb	$36, (%rax)
	je	.L12341
.L1783:
	movq	-456(%rbp), %r10
	movq	80(%r10), %rdx
	testq	%rdx, %rdx
	je	.L1814
	cmpb	$32, 16(%rdx)
	je	.L12342
.L1784:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1803
	movq	-1888(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10389
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1805
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1805:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1888(%rbp), %rdi
	leaq	8(%rdx), %r11
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r11
	ja	.L12343
.L1807:
	movq	-1888(%rbp), %rbx
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r9)
	cmpb	$32, 16(%rbx)
	je	.L12344
.L10390:
	movq	-448(%rbp), %rsi
	movq	32(%rsi), %rax
.L1814:
	cmpb	$36, (%rax)
	je	.L12345
.L1828:
	movq	current_class_type(%rip), %rdx
	movq	-1888(%rbp), %rcx
	movq	-456(%rbp), %r10
	testq	%rdx, %rdx
	movq	%rcx, 80(%r10)
	jne	.L1831
	cmpq	$0, current_function_decl(%rip)
	je	.L1830
.L1831:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L1829
.L1830:
	movq	-448(%rbp), %rdi
	movq	-1888(%rbp), %r9
	movq	%rdi, 72(%r9)
.L1736:
	movq	-1880(%rbp), %rax
	movzbl	66(%rax), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L12346
.L1734:
	movq	-456(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12347
	movq	-456(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-456(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-1896(%rbp), %rbx
	movzbl	16(%rbx), %eax
	jmp	.L1723
.L12347:
	movq	%rax, (%rdx)
	movq	-1896(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L1723
.L12346:
	movq	-456(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r9
	movq	-448(%rbp), %r11
	movq	current_class_type(%rip), %rax
	movq	%r9, 56(%r11)
	cmpq	$0, 32(%rax)
	jne	.L1734
	movq	-1880(%rbp), %r8
	movq	144(%rax), %r12
	movq	8(%r8), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L1734
.L1829:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12348
	cmpq	$0, 32(%rdx)
	jne	.L1736
	movq	-1888(%rbp), %r10
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r10)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1839
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-448(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-448(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1888(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-456(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1841:
	movq	-1888(%rbp), %rcx
	movq	current_class_type(%rip), %rbx
	movq	152(%rcx), %r10
	movq	%rbx, 64(%rcx)
	movq	%rbx, 16(%r10)
	jmp	.L1736
.L1839:
	movq	-448(%rbp), %r8
	movq	-1888(%rbp), %rdx
	movq	%r8, 72(%rdx)
	jmp	.L1841
.L12348:
	movq	-1888(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1834
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	-448(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-448(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1888(%rbp), %r8
	movq	%rax, 72(%r8)
	movq	-456(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1836:
	movq	current_function_decl(%rip), %rdx
	movq	-1888(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L1736
.L1834:
	movq	-448(%rbp), %rdi
	movq	-1888(%rbp), %r9
	movq	%rdi, 72(%r9)
	jmp	.L1836
.L12345:
	cmpb	$95, 1(%rax)
	jne	.L1828
	movq	-1888(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L1828
.L12344:
	cmpq	$0, 72(%rbx)
	je	.L12349
.L10391:
	movq	-448(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L1814
.L12349:
	movq	-1888(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -464(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1809
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1888(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-464(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10388:
	movq	-448(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L1814
.L1809:
	movq	-1888(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10390
.L12343:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1807
.L10389:
	movq	-448(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L1814
.L1803:
	movq	-1888(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1888(%rbp)
	jmp	.L10391
.L12342:
	movq	global_binding_level(%rip), %r11
	movl	$1, %r12d
	cmpq	%r11, current_binding_level(%rip)
	je	.L1785
	movq	-448(%rbp), %rbx
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L1786
.L1785:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1787
	movq	-448(%rbp), %r9
	movq	56(%r9), %rcx
	testq	%rcx, %rcx
	jne	.L10049
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L1788
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12350
.L1788:
	testq	%rcx, %rcx
	jne	.L10049
.L10050:
	movq	-448(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10049
	movq	-448(%rbp), %rsi
	movq	40(%rsi), %rcx
.L1786:
	testq	%rcx, %rcx
	je	.L1790
.L10049:
	cmpb	$32, 16(%rcx)
	je	.L1790
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1790
	movq	-448(%rbp), %r8
	movq	8(%r8), %rax
	testq	%rax, %rax
	je	.L1798
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10977
	testl	%r12d, %r12d
	jle	.L12351
.L10977:
	movq	%rax, %rcx
.L1790:
	movq	-456(%rbp), %r10
	cmpq	80(%r10), %rcx
	jne	.L1784
	jmp	.L10388
.L12351:
	testl	%edx, %edx
	jg	.L10977
	testl	%r12d, %r12d
	je	.L1790
	movq	-448(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10977
.L1798:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L1790
.L12350:
	xorl	%ecx, %ecx
	movq	-448(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1790
	testq	%rax, %rax
	je	.L10050
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L1788
.L1787:
	movq	-448(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L1786
.L12341:
	cmpb	$95, 1(%rax)
	jne	.L1783
	jmp	.L1814
.L1777:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1778
	movq	-448(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
	jmp	.L10976
.L1776:
	movq	80(%rcx), %rax
	movq	%rax, -1888(%rbp)
	jmp	.L1781
.L12340:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L1742
	movq	80(%rax), %rbx
.L1742:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1775
.L1774:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L1751
	cmpl	$32, %eax
	je	.L12352
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L1745:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1775
	jmp	.L1774
.L12352:
	movq	8(%rbx), %rdx
	movq	-456(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %r9
	movq	72(%r9), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10876
	movq	64(%rbx), %rbx
	jmp	.L1745
.L10876:
	movq	32(%rax), %rcx
	jmp	.L1775
.L1751:
	movq	-456(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L1740
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	je	.L1754
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L1755
.L1754:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1756
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10047
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1757
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L12353
.L1757:
	testq	%rcx, %rcx
	jne	.L10047
.L10048:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10047
.L1756:
	movq	40(%rbx), %rcx
.L1755:
	testq	%rcx, %rcx
	je	.L10227
.L10047:
	cmpb	$32, 16(%rcx)
	je	.L1775
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1775
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L1767
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10975
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L12354
.L10975:
	movq	%rax, %rcx
	jmp	.L1775
.L12354:
	testl	%edx, %edx
	jg	.L10975
	movl	$1, %edi
	testl	%edi, %edi
	je	.L1775
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10975
.L1767:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L1775
	jmp	.L10975
.L12353:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1775
	testq	%rax, %rax
	je	.L10048
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1757
.L1738:
	movq	-448(%rbp), %rsi
	movq	-456(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -472(%rbp)
	je	.L1815
	movq	-448(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10978:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1816:
	movq	-456(%rbp), %rdx
	movq	-448(%rbp), %r9
	movq	%rdx, 8(%r9)
	movq	-472(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L1819
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1820
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1820:
	movq	decl_obstack+24(%rip), %rdx
	movq	-472(%rbp), %rsi
	leaq	8(%rdx), %rbx
	movq	%rsi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12355
.L1822:
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-472(%rbp), %rdx
	movq	%rdx, (%r8)
	cmpb	$32, 16(%rdx)
	je	.L12356
.L1819:
	movq	-472(%rbp), %r12
	movq	%r12, -1888(%rbp)
	jmp	.L10391
.L12356:
	cmpq	$0, 72(%rdx)
	jne	.L1819
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -480(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1824
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-472(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-480(%rbp), %r10
	movq	%r10, 8(%rax)
	jmp	.L1819
.L1824:
	movq	-472(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L1819
.L12355:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1822
.L1815:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1816
	movq	-448(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10978
.L12339:
	cmpq	$0, class_binding_level(%rip)
	je	.L1737
	movq	144(%rax), %rbx
	testb	$16, 3(%rbx)
	jne	.L1736
	jmp	.L1737
.L12338:
	movq	-448(%rbp), %rdi
	movq	-456(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L1733
	.p2align 6,,7
.L1704:
	movq	-1912(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10382
	movl	flag_traditional(%rip), %r10d
	testl	%r10d, %r10d
	jne	.L10386
	testb	$8, 18(%r14)
	je	.L10386
	testb	$8, 18(%r13)
	jne	.L10386
	testb	$9, 53(%r13)
	jne	.L10386
	cmpq	%r13, current_function_decl(%rip)
	je	.L12357
.L1713:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L1714
	cmpq	$0, 8(%rax)
	jne	.L12358
.L1714:
	movq	-1912(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10974:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1912(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10386
.L12358:
	movq	-1912(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10974
.L12357:
	movq	-1912(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L1713
	.p2align 6,,7
.L12316:
	cmpq	$0, 64(%rcx)
	jne	.L1702
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L1702
.L12315:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1912(%rbp)
	call	error_with_decl
	jmp	.L1700
.L1693:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L1695
.L1699:
	cmpq	%r14, 56(%rax)
	je	.L1695
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L1699
.L1695:
	movq	%rax, -1912(%rbp)
	jmp	.L1692
.L12314:
	movq	40(%r14), %rax
	jmp	.L1695
.L12313:
	movq	56(%r13), %r14
	jmp	.L1689
.L12312:
	testb	$32, 53(%r13)
	jne	.L1687
	jmp	.L1688
.L10378:
	movzbl	16(%r13), %edx
	jmp	.L1687
.L11262:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L1680
.L1670:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1671
	movq	8, %rsi
	xorl	%edi, %edi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L10972
.L1669:
	xorl	%esi, %esi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12359
.L10973:
	movq	8, %rsi
	movq	32(%rbx), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1676:
	movq	$0, 8
	jmp	.L1668
.L12359:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1676
	jmp	.L10973
	.p2align 6,,7
.L11261:
	movq	-1824(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r10
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r10, -1856(%rbp)
	cmpq	%rax, %r13
	je	.L10359
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12360
.L1388:
	movq	%rax, 64(%r13)
.L1387:
	cmpb	$32, %dl
	je	.L12361
.L1389:
	testq	%r15, %r15
	je	.L1390
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12362
	cmpq	$0, 48(%r15)
	jne	.L1393
	movq	$0, -1864(%rbp)
.L1392:
	cmpq	$0, -1864(%rbp)
	je	.L1410
	movq	-1864(%rbp), %rcx
	cmpq	error_mark_node(%rip), %rcx
	je	.L12363
.L1400:
	cmpq	$0, -1864(%rbp)
	je	.L10363
	movq	-1864(%rbp), %rdx
	cmpb	$34, 16(%rdx)
	je	.L12364
.L1402:
	movq	-1864(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movl	32(%rcx), %ebx
	je	.L10363
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L1404
	cmpb	$32, %al
	je	.L1410
	cmpb	$32, %dl
	je	.L10901
	xorl	%eax, %eax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10363
.L10367:
	movq	global_binding_level(%rip), %rax
.L1409:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L1663
	movq	-1824(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10971:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1664:
	movq	-1824(%rbp), %rdx
	movl	$32, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L1386
	movq	-1864(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L1386
.L1663:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1664
	movq	-1824(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
	jmp	.L10971
.L10363:
	movzbl	16(%r13), %edx
.L1410:
	cmpb	$32, %dl
	je	.L10901
.L1418:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L1556
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L1556
	testb	$1, 53(%rax)
	jne	.L1557
	testb	$8, 18(%rax)
	je	.L1556
.L1557:
	andb	$8, %dl
	je	.L12365
.L1556:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10375
	testb	$1, 53(%r13)
	je	.L10375
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L1560
	movq	48(%r15), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L1561
.L1560:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1562
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10045
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1563
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12366
.L1563:
	testq	%rcx, %rcx
	jne	.L10045
.L10046:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10045
.L1562:
	movq	40(%r15), %rcx
.L1561:
	testq	%rcx, %rcx
	je	.L10226
.L10045:
	cmpb	$32, 16(%rcx)
	je	.L1565
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L1565
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L1573
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10968
	testl	%ebx, %ebx
	jle	.L12367
.L10968:
	movq	%rax, %rcx
.L1565:
	testq	%rcx, %rcx
	jne	.L10375
.L10226:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1856(%rbp)
.L1559:
	cmpq	%rax, -1856(%rbp)
	je	.L12368
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12369
.L10969:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L1604:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12370
.L1615:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L1617
	testq	%r12, %r12
	je	.L1618
	testb	$1, 53(%r13)
	jne	.L1618
	cmpb	$34, 16(%r12)
	je	.L12371
.L1618:
	movl	warn_shadow(%rip), %ecx
	testl	%ecx, %ecx
	je	.L1617
	testb	$1, 53(%r13)
	jne	.L1617
	movl	32(%r13), %r10d
	testl	%r10d, %r10d
	je	.L1617
	testq	%rax, %rax
	jne	.L1617
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L1623
	cmpb	$34, 16(%r12)
	je	.L12372
.L1623:
	cmpq	$0, 56(%r15)
	je	.L1625
	movl	$.LC41, %edi
.L1624:
	testq	%rdi, %rdi
	jne	.L10970
.L1617:
	testq	%r12, %r12
	je	.L10376
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-1856(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10376:
	movzbl	16(%r13), %edx
.L1602:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L1390
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L1390
	cmpb	$18, 16(%rcx)
	je	.L12373
.L1634:
	testb	$64, 46(%rcx)
	je	.L1390
.L1633:
	movq	-1856(%rbp), %r9
	movzwl	64(%r9), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%r9)
	je	.L12374
.L10377:
	movzbl	16(%r13), %edx
.L1390:
	cmpb	$32, %dl
	je	.L12375
.L1636:
	movq	-1856(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %r10
	cmpq	%rax, %rbx
	movq	%r10, (%r13)
	movq	%r13, (%rbx)
	je	.L12376
.L1662:
	movq	%r13, -1864(%rbp)
	jmp	.L1409
.L12376:
	testb	$4, 17(%r13)
	jne	.L1662
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L1662
.L12375:
	testq	%r15, %r15
	je	.L1636
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1637
	cmpq	class_binding_level(%rip), %rax
	je	.L1638
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L1642
	cmpb	$32, 16(%rax)
	je	.L1640
.L1642:
	cmpq	$0, current_class_type(%rip)
	je	.L1637
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L1637
	cmpb	$32, 16(%rax)
	je	.L1640
.L1637:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L1641
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1640
	cmpb	$-127, %dl
	je	.L12377
.L1641:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L1636
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12378
.L1648:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1652
	cmpq	class_binding_level(%rip), %rax
	je	.L1653
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L1657
	cmpb	$32, 16(%rax)
	je	.L1655
.L1657:
	cmpq	$0, current_class_type(%rip)
	je	.L1652
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L1652
	cmpb	$32, 16(%rax)
	je	.L1655
.L1652:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L1636
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1655
	cmpb	$-127, %dl
	jne	.L1636
	movq	$0, 8(%rbx)
	jmp	.L1636
.L1655:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L1636
.L1653:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1657
.L12378:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%cl
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L1648
.L12377:
	movq	$0, 8(%r15)
	jmp	.L1641
.L1640:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r15)
	jmp	.L1641
.L1638:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1642
.L12374:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10377
.L12373:
	movq	8(%rcx), %r11
	testb	$64, 46(%r11)
	jne	.L1633
	jmp	.L1634
.L10970:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L1617
.L1625:
	testq	%r12, %r12
	je	.L1627
	movl	$.LC42, %edi
	jmp	.L1624
.L1627:
	testq	%r8, %r8
	movl	$.LC43, %edx
	cmovne	%rdx, %rdi
	jmp	.L1624
.L12372:
	movl	$.LC40, %edi
	jmp	.L1624
.L12371:
	cmpb	$34, 16(%r13)
	je	.L1618
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L1619
	movq	56(%rax), %rax
.L1619:
	movzbl	66(%rax), %ebx
	andl	$15, %ebx
	decl	%ebx
	jne	.L1617
	movl	$.LC40, %edi
	jmp	.L10970
	.p2align 6,,7
.L12370:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12379
.L1607:
	testq	%r12, %r12
	jne	.L1615
	testq	%r8, %r8
	jne	.L1615
	testb	$1, 53(%r13)
	je	.L1615
	testb	$8, 18(%r13)
	je	.L1615
	orb	$8, 18(%r15)
	jmp	.L1615
	.p2align 6,,7
.L12379:
	testq	%r8, %r8
	je	.L1607
	cmpb	$29, 16(%r13)
	jne	.L1607
	cmpb	$29, 16(%r8)
	jne	.L1607
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12380
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L1610
	movzbl	53(%r13), %r11d
	leal	0(,%rax,8), %r9d
	leaq	88(%r13), %rdx
	andb	$-9, %r11b
	orb	%r9b, %r11b
	movb	%r11b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L1611
	movq	88(%r8), %rax
.L1612:
	movq	%rax, (%rdx)
	movq	136(%r8), %r11
	movq	80(%r8), %rdi
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%r11, 136(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %r10d
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %r10b
	movzbl	%r10b, %ecx
	movl	%ecx, %ebx
	salb	$7, %bl
	orb	%bl, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L1610:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L1613
	movzbl	53(%r13), %r9d
	salb	$4, %al
	andb	$-17, %r9b
	orb	%al, %r9b
	movb	%r9b, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L1613:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L1607
	cmpq	$0, 88(%r8)
	je	.L1607
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L1607
	movq	%rdx, 8(%r13)
	jmp	.L1607
.L1611:
	xorl	%eax, %eax
	jmp	.L1612
.L12380:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L1607
	.p2align 6,,7
.L12369:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1604
	jmp	.L10969
.L12368:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12381
.L1579:
	cmpq	$0, 40(%r15)
	jne	.L1580
	testb	$8, 18(%r13)
	je	.L1580
	orb	$8, 18(%r15)
.L1580:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12382
.L1582:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L1581:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1593
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1583
	testb	$1, 18(%rcx)
	je	.L1583
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L1583:
	testq	%rax, %rax
	je	.L1593
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1588
	testb	$8, 17(%rcx)
	je	.L1588
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L1588:
	testq	%rax, %rax
	je	.L1593
	cmpq	$0, 8(%rax)
	je	.L1593
	cmpb	$29, %dl
	je	.L12383
.L1596:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L1593:
	testb	$8, 18(%r15)
	je	.L1602
	cmpb	$32, %dl
	je	.L1602
	testb	$8, 18(%r13)
	jne	.L1602
	testb	$1, 53(%r13)
	jne	.L1602
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1598
	cmpq	$0, 8(%rax)
	jne	.L12384
.L1598:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11227:
	xorl	%eax, %eax
	call	warning
	jmp	.L10376
.L12384:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11227
.L12383:
	movq	8(%r13), %rdi
	movq	integer_type_node(%rip), %r10
	cmpq	%r10, 8(%rdi)
	jne	.L1596
	jmp	.L1593
	.p2align 6,,7
.L12382:
	cmpq	$0, -1864(%rbp)
	je	.L1582
	movq	-1864(%rbp), %rcx
	cmpb	$32, 16(%rcx)
	jne	.L1581
	jmp	.L1582
.L12381:
	testb	$8, 54(%r13)
	jne	.L1579
	andb	$-9, 18(%r13)
	jmp	.L1579
	.p2align 6,,7
.L10375:
	movq	global_binding_level(%rip), %rax
	jmp	.L1559
.L12367:
	testl	%esi, %esi
	jg	.L10968
	testl	%ebx, %ebx
	je	.L1565
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10968
	.p2align 6,,7
.L1573:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L1565
.L12366:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1565
	testq	%rax, %rax
	je	.L10046
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1563
	.p2align 6,,7
.L12365:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L1556
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L1556
	.p2align 6,,7
.L10901:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rax
	testq	%rax, %rax
	movq	%rax, -1848(%rbp)
	je	.L1420
	movzbl	16(%rax), %eax
	cmpb	$32, %al
	je	.L1419
.L1420:
	movq	global_binding_level(%rip), %rdi
	movq	%r13, -1848(%rbp)
	cmpq	%rdi, current_binding_level(%rip)
	jne	.L10368
	movq	%r13, 80(%rdx)
.L10368:
	movzbl	16(%r13), %eax
.L1423:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-1848(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L1547
	cmpq	$0, 72(%rax)
	je	.L12385
.L1547:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L1418
	cmpq	$0, 56(%rax)
	je	.L1418
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -440(%rbp)
	je	.L1552
	movq	-440(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
.L10967:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1553:
	movq	-440(%rbp), %r8
	movq	%r12, 8(%r8)
	jmp	.L1418
.L1552:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1553
	movq	-440(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10967
.L12385:
	movq	8(%r13), %r9
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r9, -432(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L1548
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-432(%rbp), %rsi
	movq	%rsi, 8(%rax)
	jmp	.L1547
.L1548:
	movq	%rbx, 72(%r13)
	jmp	.L1547
.L1419:
	movq	-1848(%rbp), %r9
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r9), %r8
	movq	%r8, -392(%rbp)
	jne	.L1423
	movq	-392(%rbp), %r10
	movq	32(%r10), %rcx
	cmpb	$36, (%rcx)
	jne	.L1423
	cmpb	$95, 1(%rcx)
	jne	.L1423
	movq	class_binding_level(%rip), %rax
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rbx
	testq	%rax, %rax
	movq	%rax, -1832(%rbp)
	movq	%rbx, -400(%rbp)
	jne	.L1427
	testb	$-128, 66(%rsi)
	movq	%rsi, -1832(%rbp)
	je	.L1427
.L1431:
	movq	-1832(%rbp), %rdx
	movq	56(%rdx), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -1832(%rbp)
	jne	.L1431
.L1427:
	movq	-1832(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12386
	movq	-1832(%rbp), %rcx
	movq	-392(%rbp), %rdi
	xorl	%eax, %eax
	movq	-400(%rbp), %rsi
	movq	8(%rcx), %rdx
	call	saveable_tree_cons
	movq	-1832(%rbp), %r12
	movq	%rax, 8(%r12)
.L1433:
	cmpq	$0, -392(%rbp)
	je	.L1434
	movq	-400(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L1435
	movq	-392(%rbp), %r11
	movq	%r11, 80(%rcx)
.L1435:
	movq	-392(%rbp), %rdi
	movq	-400(%rbp), %rax
	cmpq	%rax, 8(%rdi)
	je	.L1436
	cmpb	$21, 16(%rax)
	je	.L12387
.L1437:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L1439
	cmpq	$0, 32(%rax)
	je	.L1438
.L1439:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L12388
.L1440:
	xorl	%ecx, %ecx
.L1475:
	testq	%rcx, %rcx
	jne	.L1476
.L10225:
	movq	-392(%rbp), %rsi
	movq	-400(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-400(%rbp), %rdi
	movq	%rax, -1840(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1840(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1477
	movq	-392(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L10964:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1478:
	movq	-400(%rbp), %rsi
	movq	-392(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L1481:
	movq	-392(%rbp), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	je	.L12389
.L1483:
	movq	-400(%rbp), %r11
	movq	80(%r11), %rdx
	testq	%rdx, %rdx
	je	.L1514
	cmpb	$32, 16(%rdx)
	je	.L12390
.L1484:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1503
	movq	-1840(%rbp), %rax
	movq	56(%rax), %r12
	testq	%r12, %r12
	je	.L10370
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1505
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1505:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1840(%rbp), %rdi
	leaq	8(%rdx), %rcx
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rcx
	ja	.L12391
.L1507:
	movq	-1840(%rbp), %rbx
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r9)
	cmpb	$32, 16(%rbx)
	je	.L12392
.L10371:
	movq	-392(%rbp), %rsi
	movq	32(%rsi), %rax
.L1514:
	cmpb	$36, (%rax)
	je	.L12393
.L1528:
	movq	current_class_type(%rip), %rdx
	movq	-1840(%rbp), %r12
	movq	-400(%rbp), %r8
	testq	%rdx, %rdx
	movq	%r12, 80(%r8)
	jne	.L1531
	cmpq	$0, current_function_decl(%rip)
	je	.L1530
.L1531:
	movq	lang_name_cplusplus(%rip), %rax
	cmpq	%rax, current_lang_name(%rip)
	je	.L1529
.L1530:
	movq	-392(%rbp), %rdi
	movq	-1840(%rbp), %rcx
	movq	%rdi, 72(%rcx)
.L1436:
	movq	-1832(%rbp), %rax
	movzbl	66(%rax), %r12d
	andl	$15, %r12d
	cmpl	$2, %r12d
	je	.L12394
.L1434:
	movq	-400(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12395
	movq	-400(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-400(%rbp), %r12
	movq	%rax, (%r12)
	movq	-1848(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L1423
.L12395:
	movq	%rax, (%rdx)
	movq	-1848(%rbp), %r11
	movzbl	16(%r11), %eax
	jmp	.L1423
.L12394:
	movq	-400(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-392(%rbp), %r9
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r9)
	cmpq	$0, 32(%rax)
	jne	.L1434
	movq	-1832(%rbp), %rdx
	movq	144(%rax), %rcx
	movq	8(%rdx), %r10
	movq	%r10, 72(%rcx)
	jmp	.L1434
.L1529:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12396
	cmpq	$0, 32(%rdx)
	jne	.L1436
	movq	-1840(%rbp), %r11
	movq	80(%rdx), %r8
	movl	$136, %esi
	cmpb	$32, 16(%r11)
	movq	72(%r8), %rbx
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1539
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	-392(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-392(%rbp), %r9
	movl	24(%r9), %eax
	movq	32(%r9), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1840(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-400(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1541:
	movq	-1840(%rbp), %rbx
	movq	current_class_type(%rip), %r8
	movq	152(%rbx), %r11
	movq	%r8, 64(%rbx)
	movq	%r8, 16(%r11)
	jmp	.L1436
.L1539:
	movq	-392(%rbp), %rdx
	movq	-1840(%rbp), %r10
	movq	%rdx, 72(%r10)
	jmp	.L1541
.L12396:
	movq	-1840(%rbp), %r10
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%r10)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1534
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-392(%rbp), %r12
	cmpb	$1, 16(%r12)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-392(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1840(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-400(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1536:
	movq	current_function_decl(%rip), %r10
	movq	-1840(%rbp), %r9
	movq	%r10, 64(%r9)
	jmp	.L1436
.L1534:
	movq	-392(%rbp), %rcx
	movq	-1840(%rbp), %rdi
	movq	%rcx, 72(%rdi)
	jmp	.L1536
.L12393:
	cmpb	$95, 1(%rax)
	jne	.L1528
	movq	-1840(%rbp), %r11
	orb	$64, 53(%r11)
	jmp	.L1528
.L12392:
	cmpq	$0, 72(%rbx)
	je	.L12397
.L10372:
	movq	-392(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L1514
.L12397:
	movq	-1840(%rbp), %rdx
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rdx), %r10
	movq	%r10, -408(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1509
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1840(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-408(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10369:
	movq	-392(%rbp), %r10
	movq	32(%r10), %rax
	jmp	.L1514
.L1509:
	movq	-1840(%rbp), %rax
	movq	%r12, 72(%rax)
	jmp	.L10371
.L12391:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1507
.L10370:
	movq	-392(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L1514
.L1503:
	movq	-1840(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1840(%rbp)
	jmp	.L10372
.L12390:
	movq	global_binding_level(%rip), %r8
	movl	$1, %r12d
	cmpq	%r8, current_binding_level(%rip)
	je	.L1485
	movq	-392(%rbp), %r9
	movq	48(%r9), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L1486
.L1485:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1487
	movq	-392(%rbp), %rbx
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10043
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L1488
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L12398
.L1488:
	testq	%rcx, %rcx
	jne	.L10043
.L10044:
	movq	-392(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10043
	movq	-392(%rbp), %rsi
	movq	40(%rsi), %rcx
.L1486:
	testq	%rcx, %rcx
	je	.L1490
.L10043:
	cmpb	$32, 16(%rcx)
	je	.L1490
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1490
	movq	-392(%rbp), %r11
	movq	8(%r11), %rax
	testq	%rax, %rax
	je	.L1498
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10965
	testl	%r12d, %r12d
	jle	.L12399
.L10965:
	movq	%rax, %rcx
.L1490:
	movq	-400(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L1484
	jmp	.L10369
.L12399:
	testl	%edx, %edx
	jg	.L10965
	testl	%r12d, %r12d
	je	.L1490
	movq	-392(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10965
.L1498:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L1490
.L12398:
	xorl	%ecx, %ecx
	movq	-392(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1490
	testq	%rax, %rax
	je	.L10044
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L1488
.L1487:
	movq	-392(%rbp), %rdx
	movq	40(%rdx), %rcx
	jmp	.L1486
.L12389:
	cmpb	$95, 1(%rax)
	jne	.L1483
	jmp	.L1514
.L1477:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1478
	movq	-392(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
	jmp	.L10964
.L1476:
	movq	80(%rcx), %rax
	movq	%rax, -1840(%rbp)
	jmp	.L1481
.L12388:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L1442
	movq	80(%rax), %rbx
.L1442:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1475
.L1474:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L1451
	cmpl	$32, %eax
	je	.L12400
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L1445:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1475
	jmp	.L1474
.L12400:
	movq	8(%rbx), %rsi
	movq	-400(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rsi), %rdx
	movq	72(%rdx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10875
	movq	64(%rbx), %rbx
	jmp	.L1445
.L10875:
	movq	32(%rax), %rcx
	jmp	.L1475
.L1451:
	movq	-400(%rbp), %rax
	movq	80(%rax), %r12
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.L1440
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L1454
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L1455
.L1454:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1456
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10041
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1457
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L12401
.L1457:
	testq	%rcx, %rcx
	jne	.L10041
.L10042:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10041
.L1456:
	movq	40(%rbx), %rcx
.L1455:
	testq	%rcx, %rcx
	je	.L10225
.L10041:
	cmpb	$32, 16(%rcx)
	je	.L1475
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1475
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L1467
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10963
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L12402
.L10963:
	movq	%rax, %rcx
	jmp	.L1475
.L12402:
	testl	%edx, %edx
	jg	.L10963
	movl	$1, %r9d
	testl	%r9d, %r9d
	je	.L1475
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10963
.L1467:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L1475
	jmp	.L10963
.L12401:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1475
	testq	%rax, %rax
	je	.L10042
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1457
.L1438:
	movq	-392(%rbp), %rsi
	movq	-400(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -416(%rbp)
	je	.L1515
	movq	-392(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10966:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1516:
	movq	-400(%rbp), %r10
	movq	-392(%rbp), %r9
	movq	%r10, 8(%r9)
	movq	-416(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L1519
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1520
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1520:
	movq	-416(%rbp), %rdx
	movq	%rdx, 56(%r12)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %rbx
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12403
.L1522:
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-416(%rbp), %rdx
	movq	%rdx, (%rsi)
	cmpb	$32, 16(%rdx)
	je	.L12404
.L1519:
	movq	-416(%rbp), %r9
	movq	%r9, -1840(%rbp)
	jmp	.L10372
.L12404:
	cmpq	$0, 72(%rdx)
	jne	.L1519
	movq	current_class_name(%rip), %rbx
	movq	8(%rdx), %r11
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	%r11, -424(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1524
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-416(%rbp), %r12
	movq	%rax, 72(%r12)
	movq	-424(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L1519
.L1524:
	movq	-416(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L1519
.L12403:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1522
.L1515:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1516
	movq	-392(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10966
.L12387:
	cmpq	$0, class_binding_level(%rip)
	je	.L1437
	movq	144(%rax), %r8
	testb	$16, 3(%r8)
	jne	.L1436
	jmp	.L1437
.L12386:
	movq	-392(%rbp), %rdi
	movq	-400(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L1433
	.p2align 6,,7
.L1404:
	movq	-1864(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10363
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10367
	testb	$8, 18(%r15)
	je	.L10367
	testb	$8, 18(%r13)
	jne	.L10367
	testb	$9, 53(%r13)
	jne	.L10367
	cmpq	%r13, current_function_decl(%rip)
	je	.L12405
.L1413:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1414
	cmpq	$0, 8(%rax)
	jne	.L12406
.L1414:
	movq	-1864(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10962:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1864(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10367
.L12406:
	movq	-1864(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10962
.L12405:
	movq	-1864(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L1413
	.p2align 6,,7
.L12364:
	cmpq	$0, 64(%rdx)
	jne	.L1402
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L1402
.L12363:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1864(%rbp)
	call	error_with_decl
	jmp	.L1400
.L1393:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L1395
.L1399:
	cmpq	%r15, 56(%rax)
	je	.L1395
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L1399
.L1395:
	movq	%rax, -1864(%rbp)
	jmp	.L1392
.L12362:
	movq	40(%r15), %r12
	movq	%r12, -1864(%rbp)
	jmp	.L1392
.L12361:
	movq	56(%r13), %r15
	jmp	.L1389
.L12360:
	testb	$32, 53(%r13)
	jne	.L1387
	jmp	.L1388
.L10359:
	movzbl	16(%r13), %edx
	jmp	.L1387
.L11260:
	leal	(%rcx,%rcx), %ebx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%ebx,%rsi
	movl	%ebx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L1380
.L1370:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1371
	movq	-1768(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10960
.L1369:
	movq	-1768(%rbp), %rsi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1375
	movq	-1768(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L10961:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1376:
	movq	-1768(%rbp), %r9
	movq	$0, 8(%r9)
	jmp	.L1368
.L1375:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1376
	movq	-1768(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10961
	.p2align 6,,7
.L11259:
	movq	-1776(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r10
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r10, -1808(%rbp)
	cmpq	%rax, %r13
	je	.L10340
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12407
.L1088:
	movq	%rax, 64(%r13)
.L1087:
	cmpb	$32, %dl
	je	.L12408
.L1089:
	testq	%r15, %r15
	je	.L1090
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12409
	cmpq	$0, 48(%r15)
	jne	.L1093
	movq	$0, -1816(%rbp)
.L1092:
	cmpq	$0, -1816(%rbp)
	je	.L1110
	movq	-1816(%rbp), %r12
	cmpq	error_mark_node(%rip), %r12
	je	.L12410
.L1100:
	cmpq	$0, -1816(%rbp)
	je	.L10344
	movq	-1816(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12411
.L1102:
	movq	-1816(%rbp), %rcx
	testq	%rcx, %rcx
	movq	24(%rcx), %r12
	movq	%rcx, %rsi
	movl	32(%rcx), %ebx
	je	.L10344
	movzbl	16(%rcx), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L1104
	cmpb	$32, %al
	je	.L1110
	cmpb	$32, %dl
	je	.L10900
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10344
.L10348:
	movq	global_binding_level(%rip), %rax
.L1109:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L1363
	movq	-1776(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10959:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1364:
	movq	-1776(%rbp), %rdx
	movl	$12, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L1086
	movq	-1816(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L1086
.L1363:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1364
	movq	-1776(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
	jmp	.L10959
.L10344:
	movzbl	16(%r13), %edx
.L1110:
	cmpb	$32, %dl
	je	.L10900
.L1118:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L1256
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L1256
	testb	$1, 53(%rax)
	jne	.L1257
	testb	$8, 18(%rax)
	je	.L1256
.L1257:
	andb	$8, %dl
	je	.L12412
.L1256:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10356
	testb	$1, 53(%r13)
	je	.L10356
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L1260
	movq	48(%r15), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L1261
.L1260:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1262
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10039
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1263
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12413
.L1263:
	testq	%rcx, %rcx
	jne	.L10039
.L10040:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10039
.L1262:
	movq	40(%r15), %rcx
.L1261:
	testq	%rcx, %rcx
	je	.L10224
.L10039:
	cmpb	$32, 16(%rcx)
	je	.L1265
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L1265
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L1273
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10956
	testl	%ebx, %ebx
	jle	.L12414
.L10956:
	movq	%rax, %rcx
.L1265:
	testq	%rcx, %rcx
	jne	.L10356
.L10224:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1808(%rbp)
.L1259:
	cmpq	%rax, -1808(%rbp)
	je	.L12415
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12416
.L10957:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L1304:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12417
.L1315:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L1317
	testq	%r12, %r12
	je	.L1318
	testb	$1, 53(%r13)
	jne	.L1318
	cmpb	$34, 16(%r12)
	je	.L12418
.L1318:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L1317
	testb	$1, 53(%r13)
	jne	.L1317
	movl	32(%r13), %r10d
	testl	%r10d, %r10d
	je	.L1317
	testq	%rax, %rax
	jne	.L1317
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L1323
	cmpb	$34, 16(%r12)
	je	.L12419
.L1323:
	cmpq	$0, 56(%r15)
	je	.L1325
	movl	$.LC41, %edi
.L1324:
	testq	%rdi, %rdi
	jne	.L10958
.L1317:
	testq	%r12, %r12
	je	.L10357
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-1808(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10357:
	movzbl	16(%r13), %edx
.L1302:
	leal	-128(%rdx), %edi
	cmpb	$1, %dil
	jbe	.L1090
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L1090
	cmpb	$18, 16(%rcx)
	je	.L12420
.L1334:
	testb	$64, 46(%rcx)
	je	.L1090
.L1333:
	movq	-1808(%rbp), %rcx
	movzwl	64(%rcx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rcx)
	je	.L12421
.L10358:
	movzbl	16(%r13), %edx
.L1090:
	cmpb	$32, %dl
	je	.L12422
.L1336:
	movq	-1808(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %r10
	cmpq	%rax, %rbx
	movq	%r10, (%r13)
	movq	%r13, (%rbx)
	je	.L12423
.L1362:
	movq	%r13, -1816(%rbp)
	jmp	.L1109
.L12423:
	testb	$4, 17(%r13)
	jne	.L1362
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L1362
.L12422:
	testq	%r15, %r15
	je	.L1336
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1337
	cmpq	class_binding_level(%rip), %rax
	je	.L1338
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L1342
	cmpb	$32, 16(%rax)
	je	.L1340
.L1342:
	cmpq	$0, current_class_type(%rip)
	je	.L1337
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L1337
	cmpb	$32, 16(%rax)
	je	.L1340
.L1337:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L1341
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1340
	cmpb	$-127, %dl
	je	.L12424
.L1341:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L1336
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12425
.L1348:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1352
	cmpq	class_binding_level(%rip), %rax
	je	.L1353
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L1357
	cmpb	$32, 16(%rax)
	je	.L1355
.L1357:
	cmpq	$0, current_class_type(%rip)
	je	.L1352
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L1352
	cmpb	$32, 16(%rax)
	je	.L1355
.L1352:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L1336
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1355
	cmpb	$-127, %dl
	jne	.L1336
	movq	$0, 8(%rbx)
	jmp	.L1336
.L1355:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L1336
.L1353:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1357
.L12425:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r9b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L1348
.L12424:
	movq	$0, 8(%r15)
	jmp	.L1341
.L1340:
	movq	8(%rax), %r11
	movq	%r11, 8(%r15)
	jmp	.L1341
.L1338:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1342
.L12421:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10358
.L12420:
	movq	8(%rcx), %rsi
	testb	$64, 46(%rsi)
	jne	.L1333
	jmp	.L1334
.L10958:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L1317
.L1325:
	testq	%r12, %r12
	je	.L1327
	movl	$.LC42, %edi
	jmp	.L1324
.L1327:
	testq	%r8, %r8
	movl	$.LC43, %edx
	cmovne	%rdx, %rdi
	jmp	.L1324
.L12419:
	movl	$.LC40, %edi
	jmp	.L1324
.L12418:
	cmpb	$34, 16(%r13)
	je	.L1318
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L1319
	movq	56(%rax), %rax
.L1319:
	movzbl	66(%rax), %r9d
	andl	$15, %r9d
	decl	%r9d
	jne	.L1317
	movl	$.LC40, %edi
	jmp	.L10958
	.p2align 6,,7
.L12417:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12426
.L1307:
	testq	%r12, %r12
	jne	.L1315
	testq	%r8, %r8
	jne	.L1315
	testb	$1, 53(%r13)
	je	.L1315
	testb	$8, 18(%r13)
	je	.L1315
	orb	$8, 18(%r15)
	jmp	.L1315
	.p2align 6,,7
.L12426:
	testq	%r8, %r8
	je	.L1307
	cmpb	$29, 16(%r13)
	jne	.L1307
	cmpb	$29, 16(%r8)
	jne	.L1307
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12427
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L1310
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %esi
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%sil, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L1311
	movq	88(%r8), %rax
.L1312:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %rdi
	movq	72(%r8), %rdx
	movzbl	17(%r13), %r11d
	movq	%rcx, 136(%r13)
	movq	%rdi, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %r10d
	movq	%r8, 96(%r13)
	andb	$127, %r11b
	shrb	$7, %r10b
	movzbl	%r10b, %ebx
	movl	%ebx, %r9d
	salb	$7, %r9b
	orb	%r9b, %r11b
	movb	%r11b, 17(%r13)
	movzbl	53(%r8), %ecx
.L1310:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L1313
	movzbl	53(%r13), %esi
	salb	$4, %al
	andb	$-17, %sil
	orb	%al, %sil
	movb	%sil, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L1313:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L1307
	cmpq	$0, 88(%r8)
	je	.L1307
	movq	8(%r13), %r11
	cmpq	$0, 24(%r11)
	jne	.L1307
	movq	%rdx, 8(%r13)
	jmp	.L1307
.L1311:
	xorl	%eax, %eax
	jmp	.L1312
.L12427:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L1307
	.p2align 6,,7
.L12416:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1304
	jmp	.L10957
.L12415:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12428
.L1279:
	cmpq	$0, 40(%r15)
	jne	.L1280
	testb	$8, 18(%r13)
	je	.L1280
	orb	$8, 18(%r15)
.L1280:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12429
.L1282:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L1281:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1293
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1283
	testb	$1, 18(%rcx)
	je	.L1283
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L1283:
	testq	%rax, %rax
	je	.L1293
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L1288
	testb	$8, 17(%rcx)
	je	.L1288
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L1288:
	testq	%rax, %rax
	je	.L1293
	cmpq	$0, 8(%rax)
	je	.L1293
	cmpb	$29, %dl
	je	.L12430
.L1296:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L1293:
	testb	$8, 18(%r15)
	je	.L1302
	cmpb	$32, %dl
	je	.L1302
	testb	$8, 18(%r13)
	jne	.L1302
	testb	$1, 53(%r13)
	jne	.L1302
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1298
	cmpq	$0, 8(%rax)
	jne	.L12431
.L1298:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11226:
	xorl	%eax, %eax
	call	warning
	jmp	.L10357
.L12431:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11226
.L12430:
	movq	8(%r13), %r8
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r8)
	jne	.L1296
	jmp	.L1293
	.p2align 6,,7
.L12429:
	cmpq	$0, -1816(%rbp)
	je	.L1282
	movq	-1816(%rbp), %r10
	cmpb	$32, 16(%r10)
	jne	.L1281
	jmp	.L1282
.L12428:
	testb	$8, 54(%r13)
	jne	.L1279
	andb	$-9, 18(%r13)
	jmp	.L1279
	.p2align 6,,7
.L10356:
	movq	global_binding_level(%rip), %rax
	jmp	.L1259
.L12414:
	testl	%esi, %esi
	jg	.L10956
	testl	%ebx, %ebx
	je	.L1265
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10956
	.p2align 6,,7
.L1273:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L1265
.L12413:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1265
	testq	%rax, %rax
	je	.L10040
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1263
	.p2align 6,,7
.L12412:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L1256
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L1256
	.p2align 6,,7
.L10900:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -1800(%rbp)
	je	.L1120
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L1119
.L1120:
	movq	global_binding_level(%rip), %r8
	movq	%r13, -1800(%rbp)
	cmpq	%r8, current_binding_level(%rip)
	jne	.L10349
	movq	%r13, 80(%rdx)
.L10349:
	movzbl	16(%r13), %eax
.L1123:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-1800(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L1247
	cmpq	$0, 72(%rax)
	je	.L12432
.L1247:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L1118
	cmpq	$0, 56(%rax)
	je	.L1118
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -384(%rbp)
	je	.L1252
	movq	-384(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L10955:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1253:
	movq	-384(%rbp), %rsi
	movq	%r12, 8(%rsi)
	jmp	.L1118
.L1252:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1253
	movq	-384(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10955
.L12432:
	movq	8(%r13), %r11
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r11, -376(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L1248
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-376(%rbp), %r9
	movq	%r9, 8(%rax)
	jmp	.L1247
.L1248:
	movq	%rbx, 72(%r13)
	jmp	.L1247
.L1119:
	movq	-1800(%rbp), %rbx
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rbx), %r11
	movq	%r11, -336(%rbp)
	jne	.L1123
	movq	-336(%rbp), %r9
	movq	32(%r9), %rcx
	cmpb	$36, (%rcx)
	jne	.L1123
	cmpb	$95, 1(%rcx)
	jne	.L1123
	movq	class_binding_level(%rip), %rdi
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%rdi, %rdi
	movq	%rdi, -1784(%rbp)
	movq	%rdx, -344(%rbp)
	jne	.L1127
	testb	$-128, 66(%rsi)
	movq	%rsi, -1784(%rbp)
	je	.L1127
.L1131:
	movq	-1784(%rbp), %r12
	movq	56(%r12), %r10
	testb	$-128, 66(%r10)
	movq	%r10, -1784(%rbp)
	jne	.L1131
.L1127:
	movq	-1784(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12433
	movq	-1784(%rbp), %r8
	movq	-336(%rbp), %rdi
	xorl	%eax, %eax
	movq	-344(%rbp), %rsi
	movq	8(%r8), %rdx
	call	saveable_tree_cons
	movq	-1784(%rbp), %rcx
	movq	%rax, 8(%rcx)
.L1133:
	cmpq	$0, -336(%rbp)
	je	.L1134
	movq	-344(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L1135
	movq	-336(%rbp), %rax
	movq	%rax, 80(%rcx)
.L1135:
	movq	-336(%rbp), %r11
	movq	-344(%rbp), %rax
	cmpq	%rax, 8(%r11)
	je	.L1136
	cmpb	$21, 16(%rax)
	je	.L12434
.L1137:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L1139
	cmpq	$0, 32(%rax)
	je	.L1138
.L1139:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L12435
.L1140:
	xorl	%ecx, %ecx
.L1175:
	testq	%rcx, %rcx
	jne	.L1176
.L10223:
	movq	-336(%rbp), %rsi
	movq	-344(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-344(%rbp), %rdi
	movq	%rax, -1792(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1792(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1177
	movq	-336(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	%rdx, %rdi
.L10952:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1178:
	movq	-344(%rbp), %r10
	movq	-336(%rbp), %rdi
	movq	%r10, 8(%rdi)
.L1181:
	movq	-336(%rbp), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	je	.L12436
.L1183:
	movq	-344(%rbp), %r8
	movq	80(%r8), %rdx
	testq	%rdx, %rdx
	je	.L1214
	cmpb	$32, 16(%rdx)
	je	.L12437
.L1184:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1203
	movq	-1792(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10351
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1205
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1205:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1792(%rbp), %r9
	leaq	8(%rdx), %r11
	movq	%r9, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r11
	ja	.L12438
.L1207:
	movq	-1792(%rbp), %rbx
	movq	%rdx, %rsi
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%rsi)
	cmpb	$32, 16(%rbx)
	je	.L12439
.L10352:
	movq	-336(%rbp), %rdi
	movq	32(%rdi), %rax
.L1214:
	cmpb	$36, (%rax)
	je	.L12440
.L1228:
	movq	current_class_type(%rip), %rdx
	movq	-1792(%rbp), %rcx
	movq	-344(%rbp), %r8
	testq	%rdx, %rdx
	movq	%rcx, 80(%r8)
	jne	.L1231
	cmpq	$0, current_function_decl(%rip)
	je	.L1230
.L1231:
	movq	lang_name_cplusplus(%rip), %r11
	cmpq	%r11, current_lang_name(%rip)
	je	.L1229
.L1230:
	movq	-336(%rbp), %r12
	movq	-1792(%rbp), %r9
	movq	%r12, 72(%r9)
.L1136:
	movq	-1784(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12441
.L1134:
	movq	-344(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12442
	movq	-344(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-344(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-1800(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L1123
.L12442:
	movq	%rax, (%rdx)
	movq	-1800(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L1123
.L12441:
	movq	-344(%rbp), %r12
	orb	$64, 18(%r12)
	movq	80(%r12), %r9
	movq	-336(%rbp), %r11
	movq	current_class_type(%rip), %rax
	movq	%r9, 56(%r11)
	cmpq	$0, 32(%rax)
	jne	.L1134
	movq	-1784(%rbp), %rdi
	movq	144(%rax), %r10
	movq	8(%rdi), %rdx
	movq	%rdx, 72(%r10)
	jmp	.L1134
.L1229:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12443
	cmpq	$0, 32(%rdx)
	jne	.L1136
	movq	-1792(%rbp), %rdi
	movq	80(%rdx), %r8
	movl	$136, %esi
	cmpb	$32, 16(%rdi)
	movq	72(%r8), %rbx
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1239
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-336(%rbp), %r12
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-336(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1792(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-344(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1241:
	movq	-1792(%rbp), %rbx
	movq	current_class_type(%rip), %r8
	movq	152(%rbx), %rdi
	movq	%r8, 64(%rbx)
	movq	%r8, 16(%rdi)
	jmp	.L1136
.L1239:
	movq	-336(%rbp), %r10
	movq	-1792(%rbp), %rsi
	movq	%r10, 72(%rsi)
	jmp	.L1241
.L12443:
	movq	-1792(%rbp), %rdx
	movq	112(%rax), %rbx
	cmpb	$32, 16(%rdx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1234
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-336(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-336(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1792(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-344(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L1236:
	movq	current_function_decl(%rip), %rsi
	movq	-1792(%rbp), %rdx
	movq	%rsi, 64(%rdx)
	jmp	.L1136
.L1234:
	movq	-336(%rbp), %r12
	movq	-1792(%rbp), %r9
	movq	%r12, 72(%r9)
	jmp	.L1236
.L12440:
	cmpb	$95, 1(%rax)
	jne	.L1228
	movq	-1792(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L1228
.L12439:
	cmpq	$0, 72(%rbx)
	je	.L12444
.L10353:
	movq	-336(%rbp), %r10
	movq	32(%r10), %rax
	jmp	.L1214
.L12444:
	movq	-1792(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -352(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1209
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1792(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-352(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10350:
	movq	-336(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L1214
.L1209:
	movq	-1792(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10352
.L12438:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1207
.L10351:
	movq	-336(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L1214
.L1203:
	movq	-1792(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1792(%rbp)
	jmp	.L10353
.L12437:
	movq	global_binding_level(%rip), %r11
	movl	$1, %r12d
	cmpq	%r11, current_binding_level(%rip)
	je	.L1185
	movq	-336(%rbp), %rbx
	movq	48(%rbx), %r9
	testq	%r9, %r9
	movq	%r9, %rcx
	jne	.L1186
.L1185:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1187
	movq	-336(%rbp), %rsi
	movq	56(%rsi), %rcx
	testq	%rcx, %rcx
	jne	.L10037
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L1188
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12445
.L1188:
	testq	%rcx, %rcx
	jne	.L10037
.L10038:
	movq	-336(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10037
	movq	-336(%rbp), %rax
	movq	40(%rax), %rcx
.L1186:
	testq	%rcx, %rcx
	je	.L1190
.L10037:
	cmpb	$32, 16(%rcx)
	je	.L1190
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1190
	movq	-336(%rbp), %r10
	movq	8(%r10), %rax
	testq	%rax, %rax
	je	.L1198
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10953
	testl	%r12d, %r12d
	jle	.L12446
.L10953:
	movq	%rax, %rcx
.L1190:
	movq	-344(%rbp), %r8
	cmpq	80(%r8), %rcx
	jne	.L1184
	jmp	.L10350
.L12446:
	testl	%edx, %edx
	jg	.L10953
	testl	%r12d, %r12d
	je	.L1190
	movq	-336(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10953
.L1198:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L1190
.L12445:
	xorl	%ecx, %ecx
	movq	-336(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1190
	testq	%rax, %rax
	je	.L10038
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L1188
.L1187:
	movq	-336(%rbp), %rdi
	movq	40(%rdi), %rcx
	jmp	.L1186
.L12436:
	cmpb	$95, 1(%rax)
	jne	.L1183
	jmp	.L1214
.L1177:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1178
	movq	-336(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10952
.L1176:
	movq	80(%rcx), %r12
	movq	%r12, -1792(%rbp)
	jmp	.L1181
.L12435:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L1142
	movq	80(%rax), %rbx
.L1142:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1175
.L1174:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L1151
	cmpl	$32, %eax
	je	.L12447
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L1145:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L1175
	jmp	.L1174
.L12447:
	movq	8(%rbx), %rdi
	xorl	%eax, %eax
	movq	144(%rdi), %rdx
	movq	-344(%rbp), %rdi
	movq	72(%rdx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10874
	movq	64(%rbx), %rbx
	jmp	.L1145
.L10874:
	movq	32(%rax), %rcx
	jmp	.L1175
.L1151:
	movq	-344(%rbp), %r12
	movq	80(%r12), %r10
	movq	56(%r10), %rbx
	testq	%rbx, %rbx
	je	.L1140
	movq	global_binding_level(%rip), %rcx
	cmpq	%rcx, current_binding_level(%rip)
	je	.L1154
	movq	48(%rbx), %r8
	testq	%r8, %r8
	movq	%r8, %rcx
	jne	.L1155
.L1154:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1156
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10035
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L1157
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L12448
.L1157:
	testq	%rcx, %rcx
	jne	.L10035
.L10036:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10035
.L1156:
	movq	40(%rbx), %rcx
.L1155:
	testq	%rcx, %rcx
	je	.L10223
.L10035:
	cmpb	$32, 16(%rcx)
	je	.L1175
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L1175
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L1167
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10951
	movl	$1, %r9d
	testl	%r9d, %r9d
	jle	.L12449
.L10951:
	movq	%rax, %rcx
	jmp	.L1175
.L12449:
	testl	%edx, %edx
	jg	.L10951
	movl	$1, %esi
	testl	%esi, %esi
	je	.L1175
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10951
.L1167:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L1175
	jmp	.L10951
.L12448:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L1175
	testq	%rax, %rax
	je	.L10036
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L1157
.L1138:
	movq	-336(%rbp), %rsi
	movq	-344(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -360(%rbp)
	je	.L1215
	movq	-336(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10954:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1216:
	movq	-344(%rbp), %rdx
	movq	-336(%rbp), %rsi
	movq	%rdx, 8(%rsi)
	movq	-360(%rbp), %r9
	movq	56(%r9), %r12
	testq	%r12, %r12
	je	.L1219
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L1220
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L1220:
	movq	decl_obstack+24(%rip), %rdx
	movq	-360(%rbp), %rdi
	leaq	8(%rdx), %rbx
	movq	%rdi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12450
.L1222:
	movq	%rdx, %r10
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-360(%rbp), %rdx
	movq	%rdx, (%r10)
	cmpb	$32, 16(%rdx)
	je	.L12451
.L1219:
	movq	-360(%rbp), %r12
	movq	%r12, -1792(%rbp)
	jmp	.L10353
.L12451:
	cmpq	$0, 72(%rdx)
	jne	.L1219
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -368(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L1224
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-360(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-368(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L1219
.L1224:
	movq	-360(%rbp), %rsi
	movq	%r12, 72(%rsi)
	jmp	.L1219
.L12450:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L1222
.L1215:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1216
	movq	-336(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10954
.L12434:
	cmpq	$0, class_binding_level(%rip)
	je	.L1137
	movq	144(%rax), %rsi
	testb	$16, 3(%rsi)
	jne	.L1136
	jmp	.L1137
.L12433:
	movq	-336(%rbp), %rdi
	movq	-344(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L1133
	.p2align 6,,7
.L1104:
	movq	-1816(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10344
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10348
	testb	$8, 18(%r15)
	je	.L10348
	testb	$8, 18(%r13)
	jne	.L10348
	testb	$9, 53(%r13)
	jne	.L10348
	cmpq	%r13, current_function_decl(%rip)
	je	.L12452
.L1113:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L1114
	cmpq	$0, 8(%rax)
	jne	.L12453
.L1114:
	movq	-1816(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10950:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1816(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10348
.L12453:
	movq	-1816(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10950
.L12452:
	movq	-1816(%rbp), %rsi
	movq	%rsi, current_function_decl(%rip)
	jmp	.L1113
	.p2align 6,,7
.L12411:
	cmpq	$0, 64(%rcx)
	jne	.L1102
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L1102
.L12410:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1816(%rbp)
	call	error_with_decl
	jmp	.L1100
.L1093:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L1095
.L1099:
	cmpq	%r15, 56(%rax)
	je	.L1095
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L1099
.L1095:
	movq	%rax, -1816(%rbp)
	jmp	.L1092
.L12409:
	movq	40(%r15), %rax
	jmp	.L1095
.L12408:
	movq	56(%r13), %r15
	jmp	.L1089
.L12407:
	testb	$32, 53(%r13)
	jne	.L1087
	jmp	.L1088
.L10340:
	movzbl	16(%r13), %edx
	jmp	.L1087
.L11258:
	leal	(%rcx,%rcx), %edi
	xorl	%eax, %eax
	movl	%edi, builtin_type_tdescs_max(%rip)
	movslq	%edi,%rsi
	movq	builtin_type_tdescs_arr(%rip), %rdi
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L1080
.L1070:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1071
	movq	-1712(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10948
.L1069:
	movq	-1712(%rbp), %rsi
	movl	$32, %edi
	movq	%r14, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L1075
	movq	-1712(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L10949:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1076:
	movq	-1712(%rbp), %rbx
	movq	$0, 8(%rbx)
	jmp	.L1068
.L1075:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1076
	movq	-1712(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10949
	.p2align 6,,7
.L11257:
	movq	-1720(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%r14, %rdx
	call	build_decl
	movq	current_binding_level(%rip), %r9
	movq	%rax, %r13
	movq	112(%rax), %r15
	movq	current_function_decl(%rip), %rax
	movq	%r9, -1752(%rbp)
	cmpq	%rax, %r13
	je	.L10321
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12454
.L788:
	movq	%rax, 64(%r13)
.L787:
	cmpb	$32, %dl
	je	.L12455
.L789:
	testq	%r15, %r15
	je	.L790
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12456
	cmpq	$0, 48(%r15)
	jne	.L793
	movq	$0, -1760(%rbp)
.L792:
	cmpq	$0, -1760(%rbp)
	je	.L810
	movq	-1760(%rbp), %rbx
	cmpq	error_mark_node(%rip), %rbx
	je	.L12457
.L800:
	cmpq	$0, -1760(%rbp)
	je	.L10325
	movq	-1760(%rbp), %rcx
	cmpb	$34, 16(%rcx)
	je	.L12458
.L802:
	movq	-1760(%rbp), %rdi
	testq	%rdi, %rdi
	movq	24(%rdi), %r12
	movq	%rdi, %rsi
	movl	32(%rdi), %ebx
	je	.L10325
	movzbl	16(%rdi), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L804
	cmpb	$32, %al
	je	.L810
	cmpb	$32, %dl
	je	.L10899
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10325
.L10329:
	movq	global_binding_level(%rip), %rax
.L809:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L1063
	movq	-1720(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10947:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L1064:
	movq	-1720(%rbp), %rdx
	movl	$14, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L786
	movq	-1760(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L786
.L1063:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1064
	movq	-1720(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10947
.L10325:
	movzbl	16(%r13), %edx
.L810:
	cmpb	$32, %dl
	je	.L10899
.L818:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L956
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L956
	testb	$1, 53(%rax)
	jne	.L957
	testb	$8, 18(%rax)
	je	.L956
.L957:
	andb	$8, %dl
	je	.L12459
.L956:
	movl	flag_traditional(%rip), %r12d
	testl	%r12d, %r12d
	je	.L10337
	testb	$1, 53(%r13)
	je	.L10337
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L960
	movq	48(%r15), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L961
.L960:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L962
	movq	56(%r15), %rcx
	testq	%rcx, %rcx
	jne	.L10033
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L963
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L12460
.L963:
	testq	%rcx, %rcx
	jne	.L10033
.L10034:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10033
.L962:
	movq	40(%r15), %rcx
.L961:
	testq	%rcx, %rcx
	je	.L10222
.L10033:
	cmpb	$32, 16(%rcx)
	je	.L965
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L965
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.L973
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10944
	testl	%ebx, %ebx
	jle	.L12461
.L10944:
	movq	%rax, %rcx
.L965:
	testq	%rcx, %rcx
	jne	.L10337
.L10222:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1752(%rbp)
.L959:
	cmpq	%rax, -1752(%rbp)
	je	.L12462
	movq	current_binding_level(%rip), %rbx
	movq	48(%r15), %r12
	movq	40(%r15), %r8
	cmpq	%rax, %rbx
	je	.L12463
.L10945:
	movq	16(%rbx), %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L1004:
	testq	%r12, %r12
	movq	%r13, 48(%r15)
	je	.L12464
.L1015:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L1017
	testq	%r12, %r12
	je	.L1018
	testb	$1, 53(%r13)
	jne	.L1018
	cmpb	$34, 16(%r12)
	je	.L12465
.L1018:
	movl	warn_shadow(%rip), %r9d
	testl	%r9d, %r9d
	je	.L1017
	testb	$1, 53(%r13)
	jne	.L1017
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L1017
	testq	%rax, %rax
	jne	.L1017
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L1023
	cmpb	$34, 16(%r12)
	je	.L12466
.L1023:
	cmpq	$0, 56(%r15)
	je	.L1025
	movl	$.LC41, %edi
.L1024:
	testq	%rdi, %rdi
	jne	.L10946
.L1017:
	testq	%r12, %r12
	je	.L10338
	movq	%r12, %rsi
	movq	%r15, %rdi
	xorl	%eax, %eax
	movq	-1752(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10338:
	movzbl	16(%r13), %edx
.L1002:
	leal	-128(%rdx), %ecx
	cmpb	$1, %cl
	jbe	.L790
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L790
	cmpb	$18, 16(%rcx)
	je	.L12467
.L1034:
	testb	$64, 46(%rcx)
	je	.L790
.L1033:
	movq	-1752(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12468
.L10339:
	movzbl	16(%r13), %edx
.L790:
	cmpb	$32, %dl
	je	.L12469
.L1036:
	movq	-1752(%rbp), %r9
	movq	global_binding_level(%rip), %rax
	movq	(%r9), %rdi
	cmpq	%rax, %r9
	movq	%rdi, (%r13)
	movq	%r13, (%r9)
	je	.L12470
.L1062:
	movq	%r13, -1760(%rbp)
	jmp	.L809
.L12470:
	testb	$4, 17(%r13)
	jne	.L1062
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L1062
.L12469:
	testq	%r15, %r15
	je	.L1036
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1037
	cmpq	class_binding_level(%rip), %rax
	je	.L1038
	movq	48(%r15), %rax
	testq	%rax, %rax
	je	.L1042
	cmpb	$32, 16(%rax)
	je	.L1040
.L1042:
	cmpq	$0, current_class_type(%rip)
	je	.L1037
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.L1037
	cmpb	$32, 16(%rax)
	je	.L1040
.L1037:
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.L1041
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1040
	cmpb	$-127, %dl
	je	.L12471
.L1041:
	movq	current_class_name(%rip), %r15
	testq	%r15, %r15
	je	.L1036
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12472
.L1048:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L1052
	cmpq	class_binding_level(%rip), %rax
	je	.L1053
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L1057
	cmpb	$32, 16(%rax)
	je	.L1055
.L1057:
	cmpq	$0, current_class_type(%rip)
	je	.L1052
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L1052
	cmpb	$32, 16(%rax)
	je	.L1055
.L1052:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L1036
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L1055
	cmpb	$-127, %dl
	jne	.L1036
	movq	$0, 8(%rbx)
	jmp	.L1036
.L1055:
	movq	8(%rax), %r15
	movq	%r15, 8(%rbx)
	jmp	.L1036
.L1053:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1057
.L12472:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r11b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r15)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r15), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L1048
.L12471:
	movq	$0, 8(%r15)
	jmp	.L1041
.L1040:
	movq	8(%rax), %rsi
	movq	%rsi, 8(%r15)
	jmp	.L1041
.L1038:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L1042
.L12468:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10339
.L12467:
	movq	8(%rcx), %rbx
	testb	$64, 46(%rbx)
	jne	.L1033
	jmp	.L1034
.L10946:
	movq	32(%r15), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L1017
.L1025:
	testq	%r12, %r12
	je	.L1027
	movl	$.LC42, %edi
	jmp	.L1024
.L1027:
	testq	%r8, %r8
	movl	$.LC43, %r10d
	cmovne	%r10, %rdi
	jmp	.L1024
.L12466:
	movl	$.LC40, %edi
	jmp	.L1024
.L12465:
	cmpb	$34, 16(%r13)
	je	.L1018
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L1019
	movq	56(%rax), %rax
.L1019:
	movzbl	66(%rax), %r11d
	andl	$15, %r11d
	decl	%r11d
	jne	.L1017
	movl	$.LC40, %edi
	jmp	.L10946
	.p2align 6,,7
.L12464:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12473
.L1007:
	testq	%r12, %r12
	jne	.L1015
	testq	%r8, %r8
	jne	.L1015
	testb	$1, 53(%r13)
	je	.L1015
	testb	$8, 18(%r13)
	je	.L1015
	orb	$8, 18(%r15)
	jmp	.L1015
	.p2align 6,,7
.L12473:
	testq	%r8, %r8
	je	.L1007
	cmpb	$29, 16(%r13)
	jne	.L1007
	cmpb	$29, 16(%r8)
	jne	.L1007
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12474
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L1010
	movzbl	53(%r13), %ecx
	leal	0(,%rax,8), %ebx
	leaq	88(%r13), %rdx
	andb	$-9, %cl
	orb	%bl, %cl
	movb	%cl, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L1011
	movq	88(%r8), %rax
.L1012:
	movq	%rax, (%rdx)
	movq	136(%r8), %rcx
	movq	80(%r8), %r10
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%rcx, 136(%r13)
	movq	%r10, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %dil
	movzbl	%dil, %r9d
	movl	%r9d, %r11d
	salb	$7, %r11b
	orb	%r11b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L1010:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L1013
	movzbl	53(%r13), %ebx
	salb	$4, %al
	andb	$-17, %bl
	orb	%al, %bl
	movb	%bl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L1013:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L1007
	cmpq	$0, 88(%r8)
	je	.L1007
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L1007
	movq	%rdx, 8(%r13)
	jmp	.L1007
.L1011:
	xorl	%eax, %eax
	jmp	.L1012
.L12474:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L1007
	.p2align 6,,7
.L12463:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L1004
	jmp	.L10945
.L12462:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12475
.L979:
	cmpq	$0, 40(%r15)
	jne	.L980
	testb	$8, 18(%r13)
	je	.L980
	orb	$8, 18(%r15)
.L980:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12476
.L982:
	movq	%r13, 40(%r15)
	movzbl	16(%r13), %edx
.L981:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L993
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L983
	testb	$1, 18(%rcx)
	je	.L983
	orb	$1, 18(%r13)
	movq	72(%r15), %rax
.L983:
	testq	%rax, %rax
	je	.L993
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L988
	testb	$8, 17(%rcx)
	je	.L988
	orb	$8, 17(%r13)
	movq	72(%r15), %rax
.L988:
	testq	%rax, %rax
	je	.L993
	cmpq	$0, 8(%rax)
	je	.L993
	cmpb	$29, %dl
	je	.L12477
.L996:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L993:
	testb	$8, 18(%r15)
	je	.L1002
	cmpb	$32, %dl
	je	.L1002
	testb	$8, 18(%r13)
	jne	.L1002
	testb	$1, 53(%r13)
	jne	.L1002
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L998
	cmpq	$0, 8(%rax)
	jne	.L12478
.L998:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11225:
	xorl	%eax, %eax
	call	warning
	jmp	.L10338
.L12478:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11225
.L12477:
	movq	8(%r13), %r10
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r10)
	jne	.L996
	jmp	.L993
	.p2align 6,,7
.L12476:
	cmpq	$0, -1760(%rbp)
	je	.L982
	movq	-1760(%rbp), %r9
	cmpb	$32, 16(%r9)
	jne	.L981
	jmp	.L982
.L12475:
	testb	$8, 54(%r13)
	jne	.L979
	andb	$-9, 18(%r13)
	jmp	.L979
	.p2align 6,,7
.L10337:
	movq	global_binding_level(%rip), %rax
	jmp	.L959
.L12461:
	testl	%esi, %esi
	jg	.L10944
	testl	%ebx, %ebx
	je	.L965
	movq	%rax, %rdx
	movq	%r15, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10944
	.p2align 6,,7
.L973:
	movq	8(%rcx), %r8
	cmpq	error_mark_node(%rip), %r8
	cmove	%r8, %rcx
	jmp	.L965
.L12460:
	xorl	%ecx, %ecx
	movq	%r15, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L965
	testq	%rax, %rax
	je	.L10034
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L963
	.p2align 6,,7
.L12459:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L956
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r15), %rdi
	call	warning_with_decl
	jmp	.L956
	.p2align 6,,7
.L10899:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, -1744(%rbp)
	je	.L820
	movzbl	16(%rcx), %eax
	cmpb	$32, %al
	je	.L819
.L820:
	movq	global_binding_level(%rip), %r10
	movq	%r13, -1744(%rbp)
	cmpq	%r10, current_binding_level(%rip)
	jne	.L10330
	movq	%r13, 80(%rdx)
.L10330:
	movzbl	16(%r13), %eax
.L823:
	cmpb	$32, %al
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$140, %esi
	call	my_friendly_assert
	movq	-1744(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L947
	cmpq	$0, 72(%rax)
	je	.L12479
.L947:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L818
	cmpq	$0, 56(%rax)
	je	.L818
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -328(%rbp)
	je	.L952
	movq	-328(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L10943:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L953:
	movq	-328(%rbp), %rbx
	movq	%r12, 8(%rbx)
	jmp	.L818
.L952:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L953
	movq	-328(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10943
.L12479:
	movq	8(%r13), %r11
	movq	current_class_name(%rip), %r12
	movl	$136, %esi
	movq	%r11, -320(%rbp)
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L948
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-320(%rbp), %r8
	movq	%r8, 8(%rax)
	jmp	.L947
.L948:
	movq	%rbx, 72(%r13)
	jmp	.L947
.L819:
	movq	-1744(%rbp), %r11
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r11), %r12
	movq	%r12, -280(%rbp)
	jne	.L823
	movq	-280(%rbp), %r8
	movq	32(%r8), %rcx
	cmpb	$36, (%rcx)
	jne	.L823
	cmpb	$95, 1(%rcx)
	jne	.L823
	movq	class_binding_level(%rip), %rcx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%rcx, %rcx
	movq	%rcx, -1728(%rbp)
	movq	%rdx, -288(%rbp)
	jne	.L827
	testb	$-128, 66(%rsi)
	movq	%rsi, -1728(%rbp)
	je	.L827
.L831:
	movq	-1728(%rbp), %rax
	movq	56(%rax), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -1728(%rbp)
	jne	.L831
.L827:
	movq	-1728(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12480
	movq	-1728(%rbp), %rbx
	movq	-280(%rbp), %rdi
	xorl	%eax, %eax
	movq	-288(%rbp), %rsi
	movq	8(%rbx), %rdx
	call	saveable_tree_cons
	movq	-1728(%rbp), %r9
	movq	%rax, 8(%r9)
.L833:
	cmpq	$0, -280(%rbp)
	je	.L834
	movq	-288(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L835
	movq	-280(%rbp), %rdi
	movq	%rdi, 80(%rcx)
.L835:
	movq	-280(%rbp), %r10
	movq	-288(%rbp), %rax
	cmpq	%rax, 8(%r10)
	je	.L836
	cmpb	$21, 16(%rax)
	je	.L12481
.L837:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L839
	cmpq	$0, 32(%rax)
	je	.L838
.L839:
	movq	lang_name_cplusplus(%rip), %r8
	cmpq	%r8, current_lang_name(%rip)
	je	.L12482
.L840:
	xorl	%ecx, %ecx
.L875:
	testq	%rcx, %rcx
	jne	.L876
.L10221:
	movq	-280(%rbp), %rsi
	movq	-288(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-288(%rbp), %rdi
	movq	%rax, -1736(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1736(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L877
	movq	-280(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
.L10940:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L878:
	movq	-288(%rbp), %rsi
	movq	-280(%rbp), %rdx
	movq	%rsi, 8(%rdx)
.L881:
	movq	-280(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$36, (%rax)
	je	.L12483
.L883:
	movq	-288(%rbp), %rdi
	movq	80(%rdi), %rdx
	testq	%rdx, %rdx
	je	.L914
	cmpb	$32, 16(%rdx)
	je	.L12484
.L884:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L903
	movq	-1736(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10332
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L905
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L905:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1736(%rbp), %r11
	leaq	8(%rdx), %r10
	movq	%r11, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r10
	ja	.L12485
.L907:
	movq	-1736(%rbp), %rbx
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r8)
	cmpb	$32, 16(%rbx)
	je	.L12486
.L10333:
	movq	-280(%rbp), %rsi
	movq	32(%rsi), %rax
.L914:
	cmpb	$36, (%rax)
	je	.L12487
.L928:
	movq	current_class_type(%rip), %rdx
	movq	-1736(%rbp), %rdi
	movq	-288(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rdi, 80(%rcx)
	jne	.L931
	cmpq	$0, current_function_decl(%rip)
	je	.L930
.L931:
	movq	lang_name_cplusplus(%rip), %r10
	cmpq	%r10, current_lang_name(%rip)
	je	.L929
.L930:
	movq	-280(%rbp), %r8
	movq	-1736(%rbp), %r11
	movq	%r8, 72(%r11)
.L836:
	movq	-1728(%rbp), %rax
	movzbl	66(%rax), %esi
	andl	$15, %esi
	cmpl	$2, %esi
	je	.L12488
.L834:
	movq	-288(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12489
	movq	-288(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-288(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-1744(%rbp), %rbx
	movzbl	16(%rbx), %eax
	jmp	.L823
.L12489:
	movq	%rax, (%rdx)
	movq	-1744(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L823
.L12488:
	movq	-288(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %r8
	movq	-280(%rbp), %r11
	movq	current_class_type(%rip), %rax
	movq	%r8, 56(%r11)
	cmpq	$0, 32(%rax)
	jne	.L834
	movq	-1728(%rbp), %r9
	movq	144(%rax), %r12
	movq	8(%r9), %rdx
	movq	%rdx, 72(%r12)
	jmp	.L834
.L929:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12490
	cmpq	$0, 32(%rdx)
	jne	.L836
	movq	-1736(%rbp), %r10
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%r10)
	movq	72(%rsi), %rbx
	movl	$136, %esi
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L939
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-280(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-280(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1736(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-288(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L941:
	movq	-1736(%rbp), %rcx
	movq	current_class_type(%rip), %rbx
	movq	152(%rcx), %r10
	movq	%rbx, 64(%rcx)
	movq	%rbx, 16(%r10)
	jmp	.L836
.L939:
	movq	-280(%rbp), %r9
	movq	-1736(%rbp), %rdx
	movq	%r9, 72(%rdx)
	jmp	.L941
.L12490:
	movq	-1736(%rbp), %rdx
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%rdx)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L934
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	-280(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-280(%rbp), %r10
	movl	24(%r10), %eax
	movq	32(%r10), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1736(%rbp), %r9
	movq	%rax, 72(%r9)
	movq	-288(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L936:
	movq	current_function_decl(%rip), %rdx
	movq	-1736(%rbp), %r12
	movq	%rdx, 64(%r12)
	jmp	.L836
.L934:
	movq	-280(%rbp), %r8
	movq	-1736(%rbp), %rdi
	movq	%r8, 72(%rdi)
	jmp	.L936
.L12487:
	cmpb	$95, 1(%rax)
	jne	.L928
	movq	-1736(%rbp), %rax
	orb	$64, 53(%rax)
	jmp	.L928
.L12486:
	cmpq	$0, 72(%rbx)
	je	.L12491
.L10334:
	movq	-280(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L914
.L12491:
	movq	-1736(%rbp), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%rax), %rdx
	xorl	%eax, %eax
	movq	%rdx, -296(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L909
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1736(%rbp), %rsi
	movq	%rax, 72(%rsi)
	movq	-296(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10331:
	movq	-280(%rbp), %rdx
	movq	32(%rdx), %rax
	jmp	.L914
.L909:
	movq	-1736(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L10333
.L12485:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L907
.L10332:
	movq	-280(%rbp), %rbx
	movq	32(%rbx), %rax
	jmp	.L914
.L903:
	movq	-1736(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1736(%rbp)
	jmp	.L10334
.L12484:
	movq	global_binding_level(%rip), %r10
	movl	$1, %r12d
	cmpq	%r10, current_binding_level(%rip)
	je	.L885
	movq	-280(%rbp), %rbx
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L886
.L885:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L887
	movq	-280(%rbp), %r8
	movq	56(%r8), %rcx
	testq	%rcx, %rcx
	jne	.L10031
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L888
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L12492
.L888:
	testq	%rcx, %rcx
	jne	.L10031
.L10032:
	movq	-280(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10031
	movq	-280(%rbp), %rsi
	movq	40(%rsi), %rcx
.L886:
	testq	%rcx, %rcx
	je	.L890
.L10031:
	cmpb	$32, 16(%rcx)
	je	.L890
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L890
	movq	-280(%rbp), %r9
	movq	8(%r9), %rax
	testq	%rax, %rax
	je	.L898
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10941
	testl	%r12d, %r12d
	jle	.L12493
.L10941:
	movq	%rax, %rcx
.L890:
	movq	-288(%rbp), %rdi
	cmpq	80(%rdi), %rcx
	jne	.L884
	jmp	.L10331
.L12493:
	testl	%edx, %edx
	jg	.L10941
	testl	%r12d, %r12d
	je	.L890
	movq	-280(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10941
.L898:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L890
.L12492:
	xorl	%ecx, %ecx
	movq	-280(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L890
	testq	%rax, %rax
	je	.L10032
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L888
.L887:
	movq	-280(%rbp), %rax
	movq	40(%rax), %rcx
	jmp	.L886
.L12483:
	cmpb	$95, 1(%rax)
	jne	.L883
	jmp	.L914
.L877:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L878
	movq	-280(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10940
.L876:
	movq	80(%rcx), %rax
	movq	%rax, -1736(%rbp)
	jmp	.L881
.L12482:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L842
	movq	80(%rax), %rbx
.L842:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L875
.L874:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L851
	cmpl	$32, %eax
	je	.L12494
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L845:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L875
	jmp	.L874
.L12494:
	movq	8(%rbx), %rdx
	movq	-288(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10873
	movq	64(%rbx), %rbx
	jmp	.L845
.L10873:
	movq	32(%rax), %rcx
	jmp	.L875
.L851:
	movq	-288(%rbp), %rax
	movq	80(%rax), %rsi
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.L840
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L854
	movq	48(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L855
.L854:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L856
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10029
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L857
	movq	144(%rdi), %r10
	testb	$1, 3(%r10)
	jne	.L12495
.L857:
	testq	%rcx, %rcx
	jne	.L10029
.L10030:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10029
.L856:
	movq	40(%rbx), %rcx
.L855:
	testq	%rcx, %rcx
	je	.L10221
.L10029:
	cmpb	$32, 16(%rcx)
	je	.L875
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L875
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L867
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10939
	movl	$1, %r12d
	testl	%r12d, %r12d
	jle	.L12496
.L10939:
	movq	%rax, %rcx
	jmp	.L875
.L12496:
	testl	%edx, %edx
	jg	.L10939
	movl	$1, %r11d
	testl	%r11d, %r11d
	je	.L875
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10939
.L867:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L875
	jmp	.L10939
.L12495:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L875
	testq	%rax, %rax
	je	.L10030
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L857
.L838:
	movq	-280(%rbp), %rsi
	movq	-288(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -304(%rbp)
	je	.L915
	movq	-280(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10942:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L916:
	movq	-288(%rbp), %rdx
	movq	-280(%rbp), %r8
	movq	%rdx, 8(%r8)
	movq	-304(%rbp), %r11
	movq	56(%r11), %r12
	testq	%r12, %r12
	je	.L919
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L920
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L920:
	movq	decl_obstack+24(%rip), %rdx
	movq	-304(%rbp), %rsi
	leaq	8(%rdx), %rbx
	movq	%rsi, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rbx
	ja	.L12497
.L922:
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-304(%rbp), %rdx
	movq	%rdx, (%r9)
	cmpb	$32, 16(%rdx)
	je	.L12498
.L919:
	movq	-304(%rbp), %r12
	movq	%r12, -1736(%rbp)
	jmp	.L10334
.L12498:
	cmpq	$0, 72(%rdx)
	jne	.L919
	movq	8(%rdx), %rax
	movq	current_class_name(%rip), %rbx
	movl	$1, %edi
	movl	$136, %esi
	movq	%rax, -312(%rbp)
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L924
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-304(%rbp), %rdi
	movq	%rax, 72(%rdi)
	movq	-312(%rbp), %rcx
	movq	%rcx, 8(%rax)
	jmp	.L919
.L924:
	movq	-304(%rbp), %r8
	movq	%r12, 72(%r8)
	jmp	.L919
.L12497:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L922
.L915:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L916
	movq	-280(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10942
.L12481:
	cmpq	$0, class_binding_level(%rip)
	je	.L837
	movq	144(%rax), %r12
	testb	$16, 3(%r12)
	jne	.L836
	jmp	.L837
.L12480:
	movq	-280(%rbp), %rdi
	movq	-288(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L833
	.p2align 6,,7
.L804:
	movq	-1760(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10325
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L10329
	testb	$8, 18(%r15)
	je	.L10329
	testb	$8, 18(%r13)
	jne	.L10329
	testb	$9, 53(%r13)
	jne	.L10329
	cmpq	%r13, current_function_decl(%rip)
	je	.L12499
.L813:
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.L814
	cmpq	$0, 8(%rax)
	jne	.L12500
.L814:
	movq	-1760(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10938:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1760(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10329
.L12500:
	movq	-1760(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10938
.L12499:
	movq	-1760(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L813
	.p2align 6,,7
.L12458:
	cmpq	$0, 64(%rcx)
	jne	.L802
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L802
.L12457:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1760(%rbp)
	call	error_with_decl
	jmp	.L800
.L793:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L795
.L799:
	cmpq	%r15, 56(%rax)
	je	.L795
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L799
.L795:
	movq	%rax, -1760(%rbp)
	jmp	.L792
.L12456:
	movq	40(%r15), %rax
	jmp	.L795
.L12455:
	movq	56(%r13), %r15
	jmp	.L789
.L12454:
	testb	$32, 53(%r13)
	jne	.L787
	jmp	.L788
.L10321:
	movzbl	16(%r13), %edx
	jmp	.L787
.L11256:
	leal	(%rcx,%rcx), %edx
	movq	builtin_type_tdescs_arr(%rip), %rdi
	xorl	%eax, %eax
	movslq	%edx,%rsi
	movl	%edx, builtin_type_tdescs_max(%rip)
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L780
.L11255:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L771
	jmp	.L10936
.L769:
	movq	-216(%rbp), %rdx
	movl	$32, %edi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12501
.L10937:
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L776:
	movq	$0, 8(%r15)
	jmp	.L768
.L12501:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L776
	jmp	.L10937
	.p2align 6,,7
.L11254:
	movq	-1664(%rbp), %rsi
	movq	-216(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_decl
	movq	%rax, %r13
	movq	112(%rax), %r14
	movq	current_binding_level(%rip), %rax
	movq	%rax, -1696(%rbp)
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %r13
	je	.L10302
	movzbl	16(%r13), %edx
	cmpb	$29, %dl
	je	.L12502
.L488:
	movq	%rax, 64(%r13)
.L487:
	cmpb	$32, %dl
	je	.L12503
.L489:
	testq	%r14, %r14
	je	.L490
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L12504
	cmpq	$0, 48(%r14)
	jne	.L493
	movq	$0, -1704(%rbp)
.L492:
	cmpq	$0, -1704(%rbp)
	je	.L510
	movq	-1704(%rbp), %r12
	cmpq	error_mark_node(%rip), %r12
	je	.L12505
.L500:
	cmpq	$0, -1704(%rbp)
	je	.L10306
	movq	-1704(%rbp), %rsi
	cmpb	$34, 16(%rsi)
	je	.L12506
.L502:
	movq	-1704(%rbp), %rax
	testq	%rax, %rax
	movq	24(%rax), %r12
	movl	32(%rax), %ebx
	je	.L10306
	movzbl	16(%rax), %eax
	movzbl	16(%r13), %edx
	cmpb	%dl, %al
	je	.L504
	cmpb	$32, %al
	je	.L510
	cmpb	$32, %dl
	je	.L10898
	movq	-1704(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10306
.L10310:
	movq	global_binding_level(%rip), %rax
.L509:
	movq	current_binding_level(%rip), %rbx
	cmpq	%rax, %rbx
	je	.L763
	movq	-1664(%rbp), %r13
	movq	8(%r13), %rsi
	movq	%r13, %rdi
.L10935:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L764:
	movq	-1664(%rbp), %rdx
	movl	$2, %eax
	cmpl	$31, %eax
	movq	$0, 8(%rdx)
	jg	.L486
	movq	-1704(%rbp), %r12
	movq	%r12, 40(%rdx)
	jmp	.L486
.L763:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L764
	movq	-1664(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
	jmp	.L10935
.L10306:
	movzbl	16(%r13), %edx
.L510:
	cmpb	$32, %dl
	je	.L10898
.L518:
	movzbl	53(%r13), %edx
	testb	$1, %dl
	je	.L656
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L656
	testb	$1, 53(%rax)
	jne	.L657
	testb	$8, 18(%rax)
	je	.L656
.L657:
	andb	$8, %dl
	je	.L12507
.L656:
	movl	flag_traditional(%rip), %ecx
	testl	%ecx, %ecx
	je	.L10318
	testb	$1, 53(%r13)
	je	.L10318
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L660
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.L661
.L660:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L662
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	jne	.L10027
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L663
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12508
.L663:
	testq	%rcx, %rcx
	jne	.L10027
.L10028:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10027
.L662:
	movq	40(%r14), %rcx
.L661:
	testq	%rcx, %rcx
	je	.L10220
.L10027:
	cmpb	$32, 16(%rcx)
	je	.L665
	movl	looking_for_typename(%rip), %esi
	testl	%esi, %esi
	js	.L665
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.L673
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10932
	testl	%ebx, %ebx
	jle	.L12509
.L10932:
	movq	%rax, %rcx
.L665:
	testq	%rcx, %rcx
	jne	.L10318
.L10220:
	movq	global_binding_level(%rip), %rax
	movq	%rax, -1696(%rbp)
.L659:
	cmpq	%rax, -1696(%rbp)
	je	.L12510
	movq	current_binding_level(%rip), %rbx
	movq	48(%r14), %r12
	movq	40(%r14), %r8
	cmpq	%rax, %rbx
	je	.L12511
.L10933:
	movq	16(%rbx), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -2808(%rbp)
	call	tree_cons
	movq	-2808(%rbp), %r8
	movq	%rax, 16(%rbx)
.L704:
	testq	%r12, %r12
	movq	%r13, 48(%r14)
	je	.L12512
.L715:
	movq	96(%r13), %rax
	testq	%rax, %rax
	jne	.L717
	testq	%r12, %r12
	je	.L718
	testb	$1, 53(%r13)
	jne	.L718
	cmpb	$34, 16(%r12)
	je	.L12513
.L718:
	movl	warn_shadow(%rip), %ebx
	testl	%ebx, %ebx
	je	.L717
	testb	$1, 53(%r13)
	jne	.L717
	movl	32(%r13), %edi
	testl	%edi, %edi
	je	.L717
	testq	%rax, %rax
	jne	.L717
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L723
	cmpb	$34, 16(%r12)
	je	.L12514
.L723:
	cmpq	$0, 56(%r14)
	je	.L725
	movl	$.LC41, %edi
.L724:
	testq	%rdi, %rdi
	jne	.L10934
.L717:
	testq	%r12, %r12
	je	.L10319
	movq	%r12, %rsi
	movq	%r14, %rdi
	xorl	%eax, %eax
	movq	-1696(%rbp), %r12
	movq	16(%r12), %rdx
	call	tree_cons
	movq	%rax, 16(%r12)
.L10319:
	movzbl	16(%r13), %edx
.L702:
	leal	-128(%rdx), %r11d
	cmpb	$1, %r11b
	jbe	.L490
	movq	8(%r13), %rcx
	cmpq	$0, 32(%rcx)
	jne	.L490
	cmpb	$18, 16(%rcx)
	je	.L12515
.L734:
	testb	$64, 46(%rcx)
	je	.L490
.L733:
	movq	-1696(%rbp), %rdx
	movzwl	64(%rdx), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%rdx)
	je	.L12516
.L10320:
	movzbl	16(%r13), %edx
.L490:
	cmpb	$32, %dl
	je	.L12517
.L736:
	movq	-1696(%rbp), %rbx
	movq	global_binding_level(%rip), %rax
	movq	(%rbx), %rdi
	cmpq	%rax, %rbx
	movq	%rdi, (%r13)
	movq	%r13, (%rbx)
	je	.L12518
.L762:
	movq	%r13, -1704(%rbp)
	jmp	.L509
.L12518:
	testb	$4, 17(%r13)
	jne	.L762
	xorl	%eax, %eax
	movl	$124, %edi
	call	my_friendly_abort
	movq	global_binding_level(%rip), %rax
	jmp	.L762
.L12517:
	testq	%r14, %r14
	je	.L736
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L737
	cmpq	class_binding_level(%rip), %rax
	je	.L738
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.L742
	cmpb	$32, 16(%rax)
	je	.L740
.L742:
	cmpq	$0, current_class_type(%rip)
	je	.L737
	movq	56(%r14), %rax
	testq	%rax, %rax
	je	.L737
	cmpb	$32, 16(%rax)
	je	.L740
.L737:
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.L741
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L740
	cmpb	$-127, %dl
	je	.L12519
.L741:
	movq	current_class_name(%rip), %r14
	testq	%r14, %r14
	je	.L736
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12520
.L748:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L752
	cmpq	class_binding_level(%rip), %rax
	je	.L753
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L757
	cmpb	$32, 16(%rax)
	je	.L755
.L757:
	cmpq	$0, current_class_type(%rip)
	je	.L752
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L752
	cmpb	$32, 16(%rax)
	je	.L755
.L752:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L736
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L755
	cmpb	$-127, %dl
	jne	.L736
	movq	$0, 8(%rbx)
	jmp	.L736
.L755:
	movq	8(%rax), %r14
	movq	%r14, 8(%rbx)
	jmp	.L736
.L753:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L757
.L12520:
	cmpb	$32, 16(%r13)
	movl	$136, %esi
	movq	56(%r13), %rbx
	sete	%r9b
	movq	8(%r13), %r12
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	cmpb	$1, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r14), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r14), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	%r12, 8(%rax)
	movq	72(%r13), %rbx
	jmp	.L748
.L12519:
	movq	$0, 8(%r14)
	jmp	.L741
.L740:
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r14)
	jmp	.L741
.L738:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L742
.L12516:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L10320
.L12515:
	movq	8(%rcx), %rsi
	testb	$64, 46(%rsi)
	jne	.L733
	jmp	.L734
.L10934:
	movq	32(%r14), %rsi
	xorl	%eax, %eax
	call	warning
	jmp	.L717
.L725:
	testq	%r12, %r12
	je	.L727
	movl	$.LC42, %edi
	jmp	.L724
.L727:
	testq	%r8, %r8
	movl	$.LC43, %r10d
	cmovne	%r10, %rdi
	jmp	.L724
.L12514:
	movl	$.LC40, %edi
	jmp	.L724
.L12513:
	cmpb	$34, 16(%r13)
	je	.L718
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L719
	movq	56(%rax), %rax
.L719:
	movzbl	66(%rax), %r9d
	andl	$15, %r9d
	decl	%r9d
	jne	.L717
	movl	$.LC40, %edi
	jmp	.L10934
	.p2align 6,,7
.L12512:
	movzbl	53(%r13), %edx
	andb	$9, %dl
	decb	%dl
	je	.L12521
.L707:
	testq	%r12, %r12
	jne	.L715
	testq	%r8, %r8
	jne	.L715
	testb	$1, 53(%r13)
	je	.L715
	testb	$8, 18(%r13)
	je	.L715
	orb	$8, 18(%r14)
	jmp	.L715
	.p2align 6,,7
.L12521:
	testq	%r8, %r8
	je	.L707
	cmpb	$29, 16(%r13)
	jne	.L707
	cmpb	$29, 16(%r8)
	jne	.L707
	movq	8(%r8), %rsi
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -2808(%rbp)
	call	comptypes
	movq	-2808(%rbp), %r8
	testl	%eax, %eax
	je	.L12522
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L710
	movzbl	53(%r13), %r11d
	leal	0(,%rax,8), %ecx
	leaq	88(%r13), %rdx
	andb	$-9, %r11b
	orb	%cl, %r11b
	movb	%r11b, 53(%r13)
	cmpq	%r8, current_function_decl(%rip)
	je	.L711
	movq	88(%r8), %rax
.L712:
	movq	%rax, (%rdx)
	movq	136(%r8), %r11
	movq	80(%r8), %r10
	movq	72(%r8), %rdx
	movzbl	17(%r13), %esi
	movq	%r11, 136(%r13)
	movq	%r10, 80(%r13)
	movq	%rdx, 72(%r13)
	movzbl	17(%r8), %edi
	movq	%r8, 96(%r13)
	andb	$127, %sil
	shrb	$7, %dil
	movzbl	%dil, %ebx
	movl	%ebx, %r9d
	salb	$7, %r9b
	orb	%r9b, %sil
	movb	%sil, 17(%r13)
	movzbl	53(%r8), %ecx
.L710:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L713
	movzbl	53(%r13), %ecx
	salb	$4, %al
	andb	$-17, %cl
	orb	%al, %cl
	movb	%cl, 53(%r13)
	movl	128(%r8), %eax
	movl	%eax, 128(%r13)
.L713:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L707
	cmpq	$0, 88(%r8)
	je	.L707
	movq	8(%r13), %rsi
	cmpq	$0, 24(%rsi)
	jne	.L707
	movq	%rdx, 8(%r13)
	jmp	.L707
.L711:
	xorl	%eax, %eax
	jmp	.L712
.L12522:
	movq	%r13, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-2808(%rbp), %r8
	jmp	.L707
	.p2align 6,,7
.L12511:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L704
	jmp	.L10933
.L12510:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L12523
.L679:
	cmpq	$0, 40(%r14)
	jne	.L680
	testb	$8, 18(%r13)
	je	.L680
	orb	$8, 18(%r14)
.L680:
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12524
.L682:
	movq	%r13, 40(%r14)
	movzbl	16(%r13), %edx
.L681:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L693
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L683
	testb	$1, 18(%rcx)
	je	.L683
	orb	$1, 18(%r13)
	movq	72(%r14), %rax
.L683:
	testq	%rax, %rax
	je	.L693
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.L688
	testb	$8, 17(%rcx)
	je	.L688
	orb	$8, 17(%r13)
	movq	72(%r14), %rax
.L688:
	testq	%rax, %rax
	je	.L693
	cmpq	$0, 8(%rax)
	je	.L693
	cmpb	$29, %dl
	je	.L12525
.L696:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r13), %edx
.L693:
	testb	$8, 18(%r14)
	je	.L702
	cmpb	$32, %dl
	je	.L702
	testb	$8, 18(%r13)
	jne	.L702
	testb	$1, 53(%r13)
	jne	.L702
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L698
	cmpq	$0, 8(%rax)
	jne	.L12526
.L698:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L11224:
	xorl	%eax, %eax
	call	warning
	jmp	.L10319
.L12526:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L11224
.L12525:
	movq	8(%r13), %r10
	movq	integer_type_node(%rip), %rdi
	cmpq	%rdi, 8(%r10)
	jne	.L696
	jmp	.L693
	.p2align 6,,7
.L12524:
	cmpq	$0, -1704(%rbp)
	je	.L682
	movq	-1704(%rbp), %r12
	cmpb	$32, 16(%r12)
	jne	.L681
	jmp	.L682
.L12523:
	testb	$8, 54(%r13)
	jne	.L679
	andb	$-9, 18(%r13)
	jmp	.L679
	.p2align 6,,7
.L10318:
	movq	global_binding_level(%rip), %rax
	jmp	.L659
.L12509:
	testl	%esi, %esi
	jg	.L10932
	testl	%ebx, %ebx
	je	.L665
	movq	%rax, %rdx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10932
	.p2align 6,,7
.L673:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L665
.L12508:
	xorl	%ecx, %ecx
	movq	%r14, %rsi
	cmpl	$-1, %ebx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L665
	testq	%rax, %rax
	je	.L10028
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L663
	.p2align 6,,7
.L12507:
	movq	8(%rax), %rsi
	movq	8(%r13), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L656
	movq	%r13, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	40(%r14), %rdi
	call	warning_with_decl
	jmp	.L656
	.p2align 6,,7
.L10898:
	movq	8(%r13), %rdx
	movq	80(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, -1688(%rbp)
	je	.L520
	movzbl	16(%rbx), %eax
	cmpb	$32, %al
	je	.L519
.L520:
	movq	global_binding_level(%rip), %rbx
	movq	%r13, -1688(%rbp)
	cmpq	%rbx, current_binding_level(%rip)
	jne	.L10311
	movq	%r13, 80(%rdx)
.L10311:
	movzbl	16(%r13), %eax
.L523:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%bl
	xorl	%eax, %eax
	movzbl	%bl, %edi
	call	my_friendly_assert
	movq	-1688(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L647
	cmpq	$0, 72(%rax)
	je	.L12527
.L647:
	movq	8(%r13), %r12
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.L518
	cmpq	$0, 56(%rax)
	je	.L518
	movq	current_binding_level(%rip), %rbx
	movq	56(%r13), %rdi
	cmpq	global_binding_level(%rip), %rbx
	movq	%rdi, -272(%rbp)
	je	.L652
	movq	-272(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L10931:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L653:
	movq	-272(%rbp), %r8
	movq	%r12, 8(%r8)
	jmp	.L518
.L652:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L653
	movq	-272(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L10931
.L12527:
	movq	8(%r13), %rsi
	movq	current_class_name(%rip), %r12
	movq	%rsi, -264(%rbp)
	movl	$136, %esi
	cmpb	$32, 16(%r13)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	testq	%r12, %r12
	je	.L648
	cmpb	$1, 16(%r12)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%r9b
	xorl	%eax, %eax
	movzbl	%r9b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movq	32(%r12), %rdx
	movl	24(%rbx), %eax
	movl	$.LC35, %esi
	addl	24(%r12), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r13)
	movq	-264(%rbp), %r12
	movq	%r12, 8(%rax)
	jmp	.L647
.L648:
	movq	%rbx, 72(%r13)
	jmp	.L647
.L519:
	movq	-1688(%rbp), %r8
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%r8), %rcx
	movq	%rcx, -224(%rbp)
	jne	.L523
	movq	32(%rcx), %rcx
	cmpb	$36, (%rcx)
	jne	.L523
	cmpb	$95, 1(%rcx)
	jne	.L523
	movq	class_binding_level(%rip), %rcx
	movq	%r13, 80(%rdx)
	movq	8(%r13), %rdx
	testq	%rcx, %rcx
	movq	%rcx, -1672(%rbp)
	movq	%rdx, -232(%rbp)
	jne	.L527
	testb	$-128, 66(%rsi)
	movq	%rsi, -1672(%rbp)
	je	.L527
.L531:
	movq	-1672(%rbp), %rax
	movq	56(%rax), %r10
	testb	$-128, 66(%r10)
	movq	%r10, -1672(%rbp)
	jne	.L531
.L527:
	movq	-1672(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12528
	movq	-1672(%rbp), %r12
	movq	-224(%rbp), %rdi
	xorl	%eax, %eax
	movq	-232(%rbp), %rsi
	movq	8(%r12), %rdx
	call	saveable_tree_cons
	movq	-1672(%rbp), %r11
	movq	%rax, 8(%r11)
.L533:
	cmpq	$0, -224(%rbp)
	je	.L534
	movq	-232(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L535
	movq	-224(%rbp), %rdi
	movq	%rdi, 80(%rcx)
.L535:
	movq	-224(%rbp), %rbx
	movq	-232(%rbp), %rax
	cmpq	%rax, 8(%rbx)
	je	.L536
	cmpb	$21, 16(%rax)
	je	.L12529
.L537:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L539
	cmpq	$0, 32(%rax)
	je	.L538
.L539:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L12530
.L540:
	xorl	%ecx, %ecx
.L575:
	testq	%rcx, %rcx
	jne	.L576
.L10219:
	movq	-224(%rbp), %rsi
	movq	-232(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-232(%rbp), %rdi
	movq	%rax, -1680(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-1680(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L577
	movq	-224(%rbp), %r9
	movq	8(%r9), %rsi
	movq	%r9, %rdi
.L10928:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L578:
	movq	-232(%rbp), %r10
	movq	-224(%rbp), %rdx
	movq	%r10, 8(%rdx)
.L581:
	movq	-224(%rbp), %r11
	movq	32(%r11), %rax
	cmpb	$36, (%rax)
	je	.L12531
.L583:
	movq	-232(%rbp), %r12
	movq	80(%r12), %rdx
	testq	%rdx, %rdx
	je	.L614
	cmpb	$32, 16(%rdx)
	je	.L12532
.L584:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L603
	movq	-1680(%rbp), %rcx
	movq	56(%rcx), %r12
	testq	%r12, %r12
	je	.L10313
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L605
	movq	24(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L605:
	movq	decl_obstack+24(%rip), %rdx
	movq	-1680(%rbp), %rax
	leaq	8(%rdx), %rdi
	movq	%rax, 56(%r12)
	cmpq	decl_obstack+32(%rip), %rdi
	ja	.L12533
.L607:
	movq	-1680(%rbp), %rbx
	movq	%rdx, %r8
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r8)
	cmpb	$32, 16(%rbx)
	je	.L12534
.L10314:
	movq	-224(%rbp), %rbx
	movq	32(%rbx), %rax
.L614:
	cmpb	$36, (%rax)
	je	.L12535
.L628:
	movq	-232(%rbp), %rdx
	movq	-1680(%rbp), %rcx
	movq	%rcx, 80(%rdx)
	movq	current_class_type(%rip), %rdx
	testq	%rdx, %rdx
	jne	.L631
	cmpq	$0, current_function_decl(%rip)
	je	.L630
.L631:
	movq	lang_name_cplusplus(%rip), %rax
	cmpq	%rax, current_lang_name(%rip)
	je	.L629
.L630:
	movq	-224(%rbp), %rdi
	movq	-1680(%rbp), %r8
	movq	%rdi, 72(%r8)
.L536:
	movq	-1672(%rbp), %rax
	movzbl	66(%rax), %ecx
	andl	$15, %ecx
	cmpl	$2, %ecx
	je	.L12536
.L534:
	movq	-232(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L12537
	movq	-232(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-232(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-1688(%rbp), %r8
	movzbl	16(%r8), %eax
	jmp	.L523
.L12537:
	movq	%rax, (%rdx)
	movq	-1688(%rbp), %r11
	movzbl	16(%r11), %eax
	jmp	.L523
.L12536:
	movq	-232(%rbp), %rdi
	orb	$64, 18(%rdi)
	movq	80(%rdi), %rsi
	movq	-224(%rbp), %r12
	movq	current_class_type(%rip), %rax
	movq	%rsi, 56(%r12)
	cmpq	$0, 32(%rax)
	jne	.L534
	movq	-1672(%rbp), %r10
	movq	144(%rax), %r9
	movq	8(%r10), %rdx
	movq	%rdx, 72(%r9)
	jmp	.L534
.L629:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L12538
	cmpq	$0, 32(%rdx)
	jne	.L536
	movq	-1680(%rbp), %r11
	movq	80(%rdx), %r8
	movl	$136, %esi
	cmpb	$32, 16(%r11)
	movq	72(%r8), %rbx
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L639
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-224(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-224(%rbp), %r12
	movl	24(%r12), %eax
	movq	32(%r12), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1680(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-232(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L641:
	movq	-1680(%rbp), %rbx
	movq	current_class_type(%rip), %r8
	movq	152(%rbx), %r11
	movq	%r8, 64(%rbx)
	movq	%r8, 16(%r11)
	jmp	.L536
.L639:
	movq	-224(%rbp), %r10
	movq	-1680(%rbp), %r9
	movq	%r10, 72(%r9)
	jmp	.L641
.L12538:
	movq	-1680(%rbp), %r9
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%r9)
	sete	%r12b
	xorl	%eax, %eax
	movzbl	%r12b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L634
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-224(%rbp), %rcx
	cmpb	$1, 16(%rcx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-224(%rbp), %r8
	movl	24(%r8), %eax
	movq	32(%r8), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1680(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-232(%rbp), %r10
	movq	%r10, 8(%rax)
.L636:
	movq	current_function_decl(%rip), %r9
	movq	-1680(%rbp), %r12
	movq	%r9, 64(%r12)
	jmp	.L536
.L634:
	movq	-224(%rbp), %rdi
	movq	-1680(%rbp), %rax
	movq	%rdi, 72(%rax)
	jmp	.L636
.L12535:
	cmpb	$95, 1(%rax)
	jne	.L628
	movq	-1680(%rbp), %rsi
	orb	$64, 53(%rsi)
	jmp	.L628
.L12534:
	cmpq	$0, 72(%rbx)
	je	.L12539
.L10315:
	movq	-224(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L614
.L12539:
	movq	-1680(%rbp), %r10
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movl	$136, %esi
	movq	8(%r10), %r9
	movq	%r9, -240(%rbp)
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L609
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$137, %esi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-1680(%rbp), %r11
	movq	%rax, 72(%r11)
	movq	-240(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L10312:
	movq	-224(%rbp), %r9
	movq	32(%r9), %rax
	jmp	.L614
.L609:
	movq	-1680(%rbp), %rcx
	movq	%r12, 72(%rcx)
	jmp	.L10314
.L12533:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L607
.L10313:
	movq	-224(%rbp), %r10
	movq	32(%r10), %rax
	jmp	.L614
.L603:
	movq	-1680(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -1680(%rbp)
	jmp	.L10315
.L12532:
	movq	global_binding_level(%rip), %rsi
	movl	$1, %r12d
	cmpq	%rsi, current_binding_level(%rip)
	je	.L585
	movq	-224(%rbp), %r8
	movq	48(%r8), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	jne	.L586
.L585:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L587
	movq	-224(%rbp), %rbx
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10025
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L588
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L12540
.L588:
	testq	%rcx, %rcx
	jne	.L10025
.L10026:
	movq	-224(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10025
	movq	-224(%rbp), %r10
	movq	40(%r10), %rcx
.L586:
	testq	%rcx, %rcx
	je	.L590
.L10025:
	cmpb	$32, 16(%rcx)
	je	.L590
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L590
	movq	-224(%rbp), %r11
	movq	8(%r11), %rax
	testq	%rax, %rax
	je	.L598
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10929
	testl	%r12d, %r12d
	jle	.L12541
.L10929:
	movq	%rax, %rcx
.L590:
	movq	-232(%rbp), %rsi
	cmpq	80(%rsi), %rcx
	jne	.L584
	jmp	.L10312
.L12541:
	testl	%edx, %edx
	jg	.L10929
	testl	%r12d, %r12d
	je	.L590
	movq	-224(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10929
.L598:
	movq	8(%rcx), %r12
	cmpq	error_mark_node(%rip), %r12
	cmove	%r12, %rcx
	jmp	.L590
.L12540:
	xorl	%ecx, %ecx
	movq	-224(%rbp), %rsi
	cmpl	$-1, %r12d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L590
	testq	%rax, %rax
	je	.L10026
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L588
.L587:
	movq	-224(%rbp), %rdx
	movq	40(%rdx), %rcx
	jmp	.L586
.L12531:
	cmpb	$95, 1(%rax)
	jne	.L583
	jmp	.L614
.L577:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L578
	movq	-224(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
	jmp	.L10928
.L576:
	movq	80(%rcx), %rax
	movq	%rax, -1680(%rbp)
	jmp	.L581
.L12530:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L542
	movq	80(%rax), %rbx
.L542:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L575
.L574:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L551
	cmpl	$32, %eax
	je	.L12542
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L545:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L575
	jmp	.L574
.L12542:
	movq	8(%rbx), %rdx
	movq	-232(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%rdx), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L10872
	movq	64(%rbx), %rbx
	jmp	.L545
.L10872:
	movq	32(%rax), %rcx
	jmp	.L575
.L551:
	movq	-232(%rbp), %rax
	movq	80(%rax), %r10
	movq	56(%r10), %rbx
	testq	%rbx, %rbx
	je	.L540
	movq	global_binding_level(%rip), %r11
	cmpq	%r11, current_binding_level(%rip)
	je	.L554
	movq	48(%rbx), %r12
	testq	%r12, %r12
	movq	%r12, %rcx
	jne	.L555
.L554:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L556
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L10023
	movq	32(%rdi), %r12
	testq	%r12, %r12
	jne	.L557
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L12543
.L557:
	testq	%rcx, %rcx
	jne	.L10023
.L10024:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L10023
.L556:
	movq	40(%rbx), %rcx
.L555:
	testq	%rcx, %rcx
	je	.L10219
.L10023:
	cmpb	$32, 16(%rcx)
	je	.L575
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L575
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L567
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L10927
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L12544
.L10927:
	movq	%rax, %rcx
	jmp	.L575
.L12544:
	testl	%edx, %edx
	jg	.L10927
	movl	$1, %r8d
	testl	%r8d, %r8d
	je	.L575
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L10927
.L567:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L575
	jmp	.L10927
.L12543:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L575
	testq	%rax, %rax
	je	.L10024
	cmpb	$32, 16(%rax)
	cmovne	%r12, %rcx
	jmp	.L557
.L538:
	movq	-224(%rbp), %rsi
	movq	-232(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -248(%rbp)
	je	.L615
	movq	-224(%rbp), %r12
	movq	8(%r12), %rsi
	movq	%r12, %rdi
.L10930:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L616:
	movq	-232(%rbp), %r9
	movq	-224(%rbp), %r8
	movq	%r9, 8(%r8)
	movq	-248(%rbp), %rdi
	movq	56(%rdi), %r12
	testq	%r12, %r12
	je	.L619
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.L620
	movq	class_binding_level(%rip), %rbx
	movq	%r12, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L620:
	movq	decl_obstack+24(%rip), %rdx
	movq	-248(%rbp), %rbx
	leaq	8(%rdx), %r10
	movq	%rbx, 56(%r12)
	cmpq	decl_obstack+32(%rip), %r10
	ja	.L12545
.L622:
	movq	%rdx, %r11
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	-248(%rbp), %rdx
	movq	%rdx, (%r11)
	cmpb	$32, 16(%rdx)
	je	.L12546
.L619:
	movq	-248(%rbp), %r12
	movq	%r12, -1680(%rbp)
	jmp	.L10315
.L12546:
	cmpq	$0, 72(%rdx)
	jne	.L619
	movq	8(%rdx), %rsi
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$1, %edi
	movq	%rsi, -256(%rbp)
	movl	$136, %esi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L624
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%r12)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%r12), %rcx
	movq	32(%rbx), %rdx
	movl	24(%r12), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-248(%rbp), %rcx
	movq	%rax, 72(%rcx)
	movq	-256(%rbp), %rdx
	movq	%rdx, 8(%rax)
	jmp	.L619
.L624:
	movq	-248(%rbp), %rdi
	movq	%r12, 72(%rdi)
	jmp	.L619
.L12545:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L622
.L615:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L616
	movq	-224(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L10930
.L12529:
	cmpq	$0, class_binding_level(%rip)
	je	.L537
	movq	144(%rax), %rsi
	testb	$16, 3(%rsi)
	jne	.L536
	jmp	.L537
.L12528:
	movq	-224(%rbp), %rdi
	movq	-232(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L533
	.p2align 6,,7
.L504:
	movq	-1704(%rbp), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L10306
	movl	flag_traditional(%rip), %esi
	testl	%esi, %esi
	jne	.L10310
	testb	$8, 18(%r14)
	je	.L10310
	testb	$8, 18(%r13)
	jne	.L10310
	testb	$9, 53(%r13)
	jne	.L10310
	cmpq	%r13, current_function_decl(%rip)
	je	.L12547
.L513:
	movq	72(%r14), %rax
	testq	%rax, %rax
	je	.L514
	cmpq	$0, 8(%rax)
	jne	.L12548
.L514:
	movq	-1704(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L10926:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	-1704(%rbp), %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	%ebx, %esi
	movq	%rax, %rcx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L10310
.L12548:
	movq	-1704(%rbp), %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L10926
.L12547:
	movq	-1704(%rbp), %r13
	movq	%r13, current_function_decl(%rip)
	jmp	.L513
	.p2align 6,,7
.L12506:
	cmpq	$0, 64(%rsi)
	jne	.L502
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L502
.L12505:
	movq	%r13, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	movq	$0, -1704(%rbp)
	call	error_with_decl
	jmp	.L500
.L493:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L495
.L499:
	cmpq	%r14, 56(%rax)
	je	.L495
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L499
.L495:
	movq	%rax, -1704(%rbp)
	jmp	.L492
.L12504:
	movq	40(%r14), %r11
	movq	%r11, -1704(%rbp)
	jmp	.L492
.L12503:
	movq	56(%r13), %r14
	jmp	.L489
.L12502:
	testb	$32, 53(%r13)
	jne	.L487
	jmp	.L488
.L10302:
	movzbl	16(%r13), %edx
	jmp	.L487
	.p2align 6,,7
.L482:
	movl	$8, %edi
	xorl	%eax, %eax
	call	make_unsigned_type
	jmp	.L483
.L11253:
	leal	(%rcx,%rcx), %eax
	movq	builtin_type_tdescs_arr(%rip), %rdi
	movslq	%eax,%rsi
	movl	%eax, builtin_type_tdescs_max(%rip)
	xorl	%eax, %eax
	salq	$3, %rsi
	call	xrealloc
	movl	builtin_type_tdescs_len(%rip), %edx
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L478
.L468:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L469
	movq	8(%r12), %rsi
	movq	%r12, %rdi
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	jmp	.L10924
.L467:
	movl	$32, %edi
	movq	%r12, %rsi
	movq	%r13, %rdx
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12549
.L10925:
	movq	8(%r12), %rsi
	movq	32(%rbx), %rdx
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L474:
	movq	$0, 8(%r12)
	jmp	.L466
.L12549:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L474
	jmp	.L10925
	.p2align 6,,7
.L178:
	movq	current_binding_level(%rip), %rbx
	movq	%rbx, 56(%rdi)
	jmp	.L179
.L176:
	movl	$72, %edi
	xorl	%eax, %eax
	call	xmalloc
	movq	%rax, %rdi
	jmp	.L177
.L11252:
	cmpq	$0, named_labels(%rip)
	movl	$134, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	jmp	.L175
.L11251:
	movl	$800, %edi
	xorl	%eax, %eax
	movl	$100, builtin_type_tdescs_max(%rip)
	call	xmalloc
	movq	%rax, builtin_type_tdescs_arr(%rip)
	jmp	.L174
.LFE2:
.Lfe2:
	.size	init_decl_processing,.Lfe2-init_decl_processing
	.align 2
	.p2align 4,,15
.globl define_function
	.type	define_function,@function
define_function:
.LFB3:
	subq	$40, %rsp
.LCFI8:
	xorl	%eax, %eax
	movq	%rbx, 8(%rsp)
.LCFI9:
	movq	%rbp, 16(%rsp)
.LCFI10:
	movq	%r12, 24(%rsp)
.LCFI11:
	movq	%rsi, %rbx
	movq	%rcx, %rbp
	movq	%r13, 32(%rsp)
.LCFI12:
	movq	%r8, %r12
	movl	%edx, %r13d
	call	get_identifier
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movl	$29, %edi
	xorl	%eax, %eax
	call	build_lang_decl
	orb	$1, 53(%rax)
	orb	$8, 18(%rax)
	movq	%rax, %rbx
	testq	%rbp, %rbp
	je	.L12551
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	*%rbp
.L12551:
	testq	%r12, %r12
	je	.L12552
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, 112(%rbx)
.L12552:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	make_function_rtl
	testl	%r13d, %r13d
	je	.L12553
	orb	$16, 53(%rbx)
	movl	%r13d, 128(%rbx)
.L12553:
	movq	%rbx, %rax
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	8(%rsp), %rbx
	movq	32(%rsp), %r13
	addq	$40, %rsp
	ret
.LFE3:
.Lfe3:
	.size	define_function,.Lfe3-define_function
.globl debug_temp_inits
	.data
	.align 4
	.type	debug_temp_inits,@object
	.size	debug_temp_inits,4
debug_temp_inits:
	.long	1
	.section	.rodata.str1.32
	.align 32
.LC136:
	.string	"parser lost in parsing declaration of `%s'"
	.align 32
.LC134:
	.string	"storage size of `%s' isn't known"
	.align 32
.LC135:
	.string	"storage size of `%s' isn't constant"
	.section	.rodata.str1.1
.LC133:
	.string	"zero-size array `%s'"
.LC132:
	.string	"array size missing in `%s'"
	.section	.rodata.str1.32
	.align 32
.LC131:
	.string	"initializer fails to determine size of `%s'"
	.align 32
.LC127:
	.string	"`%s' must be initialized by constructor, not by `{...}'"
	.section	.rodata.str1.1
.LC130:
	.string	"uninitialized const `%s'"
	.section	.rodata.str1.32
	.align 32
.LC129:
	.string	"structure `%s' with uninitialized reference members"
	.align 32
.LC128:
	.string	"structure `%s' with uninitialized const members"
	.align 32
.LC117:
	.string	"variable declared as reference not initialized"
	.align 32
.LC123:
	.string	"invalid type conversion for reference"
	.align 32
.LC126:
	.string	"type mismatch in initialization of `%s' (use `const')"
	.align 32
.LC125:
	.string	"constructor failed to build reference initializer"
	.section	.rodata.str1.1
.LC124:
	.string	"ambiguous pointer conversion"
	.section	.rodata.str1.32
	.align 32
.LC119:
	.string	"cannot initialize a reference to a volatile T with a const T"
	.align 32
.LC120:
	.string	"cannot initialize a reference to a const T with a volatile T"
	.align 32
.LC121:
	.string	"cannot initialize a reference to T with a const T"
	.align 32
.LC122:
	.string	"cannot initialize a reference to T with a volatile T"
	.align 32
.LC118:
	.string	"initialization of `%s' from dissimilar reference type"
	.align 32
.LC116:
	.string	"shadowing previous type declaration of `%s'"
	.align 32
.LC115:
	.string	"assignment (not initialization) in declaration"
	.text
	.align 2
	.p2align 4,,15
.globl finish_decl
	.type	finish_decl,@function
finish_decl:
.LFB4:
	pushq	%rbp
.LCFI13:
	xorl	%eax, %eax
	movq	%rsp, %rbp
.LCFI14:
	pushq	%r15
.LCFI15:
	pushq	%r14
.LCFI16:
	movq	%rsi, %r14
	pushq	%r13
.LCFI17:
	movq	%rdi, %r13
	pushq	%r12
.LCFI18:
	pushq	%rbx
.LCFI19:
	movq	%rdx, %rbx
	subq	$296, %rsp
.LCFI20:
	movq	$0, -48(%rbp)
	movl	%ecx, -52(%rbp)
	call	allocation_temporary_p
	testq	%r13, %r13
	movl	$0, -316(%rbp)
	movl	%eax, -60(%rbp)
	movq	$0, -224(%rbp)
	je	.L13135
	testq	%rbx, %rbx
	je	.L12557
	movq	40(%rbx), %rax
	movq	$0, 120(%r13)
	movq	%rax, -224(%rbp)
.L12557:
	xorl	%edx, %edx
	movq	8(%r13), %r15
	cmpq	$0, 40(%r13)
	sete	%dl
	movl	%edx, -56(%rbp)
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	movl	%edx, %ecx
	je	.L13136
	cmpq	error_mark_node(%rip), %r15
	je	.L12567
	testb	$64, 46(%r15)
	je	.L12567
	movq	144(%r15), %rbx
	testb	$16, 3(%rbx)
	jne	.L13137
.L12567:
	cmpb	$29, %cl
	je	.L12568
	movq	%r15, %rdi
	xorl	%eax, %eax
	call	target_type
	movzbl	16(%r13), %edx
	movq	%rax, -232(%rbp)
.L12568:
	movzbl	53(%r13), %edi
	testb	$1, %dil
	movl	%edi, %ecx
	jne	.L12569
	movzbl	17(%r13), %eax
	testb	$32, %al
	je	.L12569
	testb	$16, 46(%r15)
	je	.L12569
	andb	$-33, %al
	movl	$1, -316(%rbp)
	movb	%al, 17(%r13)
	.p2align 4,,7
.L12569:
	cmpb	$36, %dl
	je	.L13138
	testq	%r14, %r14
	je	.L12574
	cmpq	$0, 88(%r13)
	je	.L12583
.L12574:
	andb	$1, %cl
	jne	.L12573
	cmpb	$15, 16(%r15)
	je	.L13139
.L12573:
	movq	current_function_decl(%rip), %rdi
	xorl	%eax, %eax
	movq	%r13, %rsi
	call	GNU_xref_decl
	movzbl	16(%r13), %edx
	cmpb	$36, %dl
	movl	%edx, %ecx
	je	.L12665
	testb	$1, 53(%r13)
	jne	.L12665
	cmpb	$31, %dl
	je	.L13140
	testq	%r14, %r14
	je	.L12668
	testb	$16, 46(%r15)
	je	.L12669
	cmpb	$18, 16(%r15)
	je	.L13141
	cmpb	$43, 16(%r14)
	je	.L13142
.L12671:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L12690
.L13133:
	movq	error_mark_node(%rip), %rax
.L13116:
	movq	%rax, 88(%r13)
	.p2align 4,,7
.L12665:
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L13143
.L12694:
	cmpb	$18, 16(%r15)
	je	.L13144
.L13086:
	movzbl	16(%r13), %edx
.L12695:
	cmpb	$33, %dl
	je	.L13145
	cmpb	$34, %dl
	je	.L13146
.L12719:
	cmpb	$33, %dl
	je	.L12724
	cmpb	$29, %dl
	je	.L12724
	cmpb	$35, %dl
	je	.L12724
.L12566:
	movl	-52(%rbp), %eax
	testl	%eax, %eax
	jne	.L13147
.L13064:
	movl	-316(%rbp), %r14d
	testl	%r14d, %r14d
	je	.L13065
	orb	$32, 17(%r13)
.L13065:
	movl	flag_cadillac(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L13148
.L12554:
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	leave
	ret
	.p2align 6,,7
.L13148:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	cadillac_finish_decl
	jmp	.L12554
	.p2align 6,,7
.L13147:
	xorl	%eax, %eax
	call	pop_obstacks
	jmp	.L13064
	.p2align 6,,7
.L12724:
	movl	flag_traditional(%rip), %edi
	xorl	%ebx, %ebx
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	sete	%bl
	xorl	%r12d, %r12d
	testl	%edi, %edi
	jne	.L12726
	testb	$4, 18(%r13)
	je	.L12725
	testb	$32, 46(%r15)
	je	.L12725
.L12726:
	xorl	%eax, %eax
	call	allocation_temporary_p
	testl	%eax, %eax
	movl	$1, %eax
	cmovne	%eax, %r12d
.L12725:
	testl	%r12d, %r12d
	jne	.L13149
.L12727:
	cmpq	$0, -48(%rbp)
	je	.L12728
	movq	current_binding_level(%rip), %r10
	movzbl	66(%r10), %r9d
	andl	$15, %r9d
	cmpl	$3, %r9d
	je	.L13150
.L12728:
	movzbl	16(%r13), %edx
	cmpb	$33, %dl
	je	.L13151
.L12730:
	movzwl	16(%r13), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L13152
.L12732:
	cmpb	$33, %dl
	je	.L13153
.L12740:
	movq	%r13, %rdi
	movq	-224(%rbp), %rsi
	movl	%ebx, %edx
.L13118:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	call	rest_of_decl_compilation
.L12731:
	testl	%r12d, %r12d
	jne	.L13154
.L12747:
	cmpq	error_mark_node(%rip), %r15
	je	.L12748
	movq	144(%r15), %rax
	testq	%rax, %rax
	je	.L12748
	cmpq	$0, 128(%rax)
	jne	.L13155
.L12748:
	movzbl	16(%r15), %eax
	cmpb	$23, %al
	je	.L12751
	cmpb	$16, %al
	je	.L12751
.L12749:
	cmpb	$29, 16(%r13)
	je	.L13156
	testb	$1, 53(%r13)
	jne	.L12745
	movzbl	18(%r13), %eax
	testb	$4, %al
	je	.L13040
	cmpq	error_mark_node(%rip), %r15
	je	.L13040
	movzbl	46(%r15), %eax
	testb	$16, %al
	jne	.L13042
	testq	%r14, %r14
	je	.L13041
.L13042:
	movq	%r13, %rdi
	movq	%r14, %rsi
	xorl	%eax, %eax
	call	expand_static_init
.L13043:
	movl	flag_gc(%rip), %esi
	testl	%esi, %esi
	jne	.L13157
	.p2align 4,,7
.L12745:
	movq	64(%r13), %rdx
	testq	%rdx, %rdx
	je	.L12566
	movzbq	16(%rdx), %r12
	salq	$3, %r12
	addq	tree_code_type(%rip), %r12
	movq	(%r12), %r11
	cmpb	$116, (%r11)
	jne	.L12566
	movzbl	16(%r13), %eax
	cmpb	$33, %al
	je	.L13063
	cmpb	$29, %al
	jne	.L12566
	cmpq	$0, 32(%rdx)
	je	.L12566
	movq	80(%rdx), %rbx
	movq	current_class_name(%rip), %rdx
	cmpq	%rdx, 56(%rbx)
	jne	.L12566
.L13063:
	movl	$1, %edi
	xorl	%eax, %eax
	call	popclass
	jmp	.L12566
.L13157:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	type_needs_gc_entry
	testl	%eax, %eax
	je	.L12745
	movq	%r13, %rdi
	movq	%r15, %rsi
	xorl	%eax, %eax
	call	build_static_gc_entry
	jmp	.L12745
.L13041:
	testb	$32, %al
	je	.L13043
	movq	static_aggregates(%rip), %rdx
	xorl	%edi, %edi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	%rax, static_aggregates(%rip)
	jmp	.L13043
	.p2align 6,,7
.L13040:
	movq	global_binding_level(%rip), %r9
	cmpq	%r9, current_binding_level(%rip)
	je	.L12745
	movl	-56(%rbp), %r8d
	testl	%r8d, %r8d
	je	.L13048
	testb	$4, %al
	jne	.L13048
	movzbl	17(%r13), %r10d
	andb	$1, %al
	leal	0(,%rax,8), %edi
	andb	$-9, %r10b
	orb	%dil, %r10b
	cmpq	$0, 40(%r13)
	movb	%r10b, 17(%r13)
	jne	.L13130
	movq	$0, 88(%r13)
.L13130:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	expand_decl
.L13050:
	cmpq	$0, 40(%r13)
	je	.L12745
	cmpq	error_mark_node(%rip), %r15
	je	.L12745
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	expand_decl_init
	testq	%r14, %r14
	jne	.L13058
	movzbl	46(%r15), %eax
	testb	$16, %al
	je	.L13057
.L13058:
	movq	24(%r13), %rdi
	movl	32(%r13), %esi
	xorl	%eax, %eax
	call	emit_line_note
	xorl	%eax, %eax
	xorl	%edx, %edx
	movq	%r13, %rdi
	movq	%r14, %rsi
	call	expand_aggr_init
	movzbl	46(%r15), %eax
.L13057:
	testb	$16, %al
	je	.L13059
	andb	$-2, 18(%r13)
.L13059:
	movq	-48(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L12745
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	expand_decl_cleanup
	testl	%eax, %eax
	jne	.L12745
	movq	%r13, %rdi
	movl	$.LC136, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12745
	.p2align 6,,7
.L13048:
	testb	$-128, 17(%r13)
	jne	.L13050
	cmpq	$0, 32(%r15)
	jne	.L13052
	cmpb	$18, 16(%r15)
	jne	.L13050
.L13052:
	cmpq	$0, 120(%r13)
	je	.L13130
	movq	-48(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L13050
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	expand_decl_cleanup
	movq	$0, -48(%rbp)
	jmp	.L13050
	.p2align 6,,7
.L13156:
	testb	$32, 54(%r13)
	je	.L12745
	movq	24(%r15), %rbx
	movq	56(%r13), %rdx
	xorl	%eax, %eax
	movq	152(%r13), %r14
	movq	%r13, %rdi
	xorl	%r12d, %r12d
	movq	%rbx, -272(%rbp)
	movq	%rdx, -96(%rbp)
	movq	%r14, -104(%rbp)
	call	copy_decl_lang_specific
	testq	%rbx, %rbx
	je	.L12755
	cmpq	void_list_node(%rip), %rbx
	je	.L12755
.L13036:
	movq	-272(%rbp), %rbx
	cmpq	$0, 24(%rbx)
	jne	.L13158
.L12758:
	movq	-272(%rbp), %r12
	movq	(%r12), %rsi
	testq	%rsi, %rsi
	movq	%rsi, -272(%rbp)
	je	.L12755
	cmpq	void_list_node(%rip), %rsi
	jne	.L13036
.L12755:
	movq	-104(%rbp), %r15
	movq	%r15, 152(%r13)
	jmp	.L12745
.L13158:
	testq	%r12, %r12
	leaq	24(%r15), %r14
	movq	-96(%rbp), %rdi
	cmovne	%r12, %r14
	xorl	%eax, %eax
	xorl	%edx, %edx
	movq	$0, (%r14)
	movq	24(%r15), %rsi
	call	build_decl_overload
	movq	%r15, %rdx
	movq	-272(%rbp), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	movq	%rdi, (%r14)
	movl	$29, %edi
	call	build_decl
	movzbl	53(%r13), %ecx
	movzbl	53(%rax), %edx
	movq	%rax, %r14
	andb	$1, %cl
	andb	$-2, %dl
	orb	%cl, %dl
	movb	%dl, 53(%rax)
	movq	112(%r14), %r8
	andb	$-9, %dl
	movzbl	18(%r13), %eax
	movzbl	18(%r14), %r10d
	andb	$8, %al
	andb	$-9, %r10b
	orb	%al, %r10b
	movb	%r10b, 18(%r14)
	orb	$1, %r10b
	movzbl	53(%r13), %r11d
	orb	$-128, 17(%r14)
	movb	%r10b, 18(%r14)
	movq	$0, 88(%r14)
	andb	$8, %r11b
	orb	%r11b, %dl
	movb	%dl, 53(%r14)
	movq	current_function_decl(%rip), %rax
	movq	current_binding_level(%rip), %r12
	movq	152(%r13), %r9
	cmpq	%rax, %r14
	movq	%r9, 152(%r14)
	movq	%r12, -312(%rbp)
	movq	%r8, -280(%rbp)
	je	.L13092
	movzbl	16(%r14), %ecx
	cmpb	$29, %cl
	je	.L13159
.L12761:
	movq	%rax, 64(%r14)
.L12760:
	cmpb	$32, %cl
	je	.L13160
.L12762:
	cmpq	$0, -280(%rbp)
	je	.L12763
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L13161
	movq	-280(%rbp), %rdx
	cmpq	$0, 48(%rdx)
	jne	.L12766
	xorl	%r12d, %r12d
.L12765:
	testq	%r12, %r12
	je	.L12783
	cmpq	error_mark_node(%rip), %r12
	je	.L13162
.L12773:
	testq	%r12, %r12
	je	.L13096
	cmpb	$34, 16(%r12)
	je	.L13163
.L12775:
	movl	32(%r12), %eax
	testq	%r12, %r12
	movq	24(%r12), %rbx
	movl	%eax, -108(%rbp)
	je	.L13096
	movzbl	16(%r12), %eax
	movzbl	16(%r14), %ecx
	cmpb	%cl, %al
	je	.L12777
	cmpb	$32, %al
	je	.L12783
	cmpb	$32, %cl
	je	.L13109
	xorl	%eax, %eax
	movq	%r14, %rdi
	movq	%r12, %rsi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L13096
.L12782:
	movq	error_mark_node(%rip), %rcx
	movq	120(%r13), %r14
	movq	%rcx, 88(%r12)
	movq	%r14, 120(%r12)
	jmp	.L12758
.L13096:
	movzbl	16(%r14), %ecx
.L12783:
	cmpb	$32, %cl
	je	.L13109
.L12791:
	movzbl	53(%r14), %edx
	testb	$1, %dl
	je	.L12929
	movq	-280(%rbp), %r9
	movq	40(%r9), %rax
	testq	%rax, %rax
	je	.L12929
	testb	$1, 53(%rax)
	jne	.L12930
	testb	$8, 18(%rax)
	je	.L12929
.L12930:
	andb	$8, %dl
	je	.L13164
.L12929:
	movl	flag_traditional(%rip), %r10d
	testl	%r10d, %r10d
	je	.L13104
	testb	$1, 53(%r14)
	je	.L13104
	movq	global_binding_level(%rip), %rsi
	cmpq	%rsi, current_binding_level(%rip)
	je	.L12933
	movq	-280(%rbp), %rax
	movq	48(%rax), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L12934
.L12933:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L12935
	movq	-280(%rbp), %r8
	movq	56(%r8), %rcx
	testq	%rcx, %rcx
	jne	.L13079
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L12936
	movq	144(%rdi), %r9
	testb	$1, 3(%r9)
	jne	.L13165
.L12936:
	testq	%rcx, %rcx
	jne	.L13079
.L13080:
	movq	-280(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L13079
	movq	-280(%rbp), %rbx
	movq	40(%rbx), %rcx
.L12934:
	testq	%rcx, %rcx
	je	.L13082
.L13079:
	cmpb	$32, 16(%rcx)
	je	.L12938
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L12938
	movq	-280(%rbp), %r10
	movq	8(%r10), %rax
	testq	%rax, %rax
	je	.L12946
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L13127
	xorl	%r11d, %r11d
	testl	%r11d, %r11d
	jle	.L13166
.L13127:
	movq	%rax, %rcx
.L12938:
	testq	%rcx, %rcx
	jne	.L13104
.L13082:
	movq	global_binding_level(%rip), %r9
	movq	%r9, %rax
	movq	%r9, -312(%rbp)
.L12932:
	cmpq	%rax, -312(%rbp)
	je	.L13167
	movq	current_binding_level(%rip), %rbx
	movq	-280(%rbp), %rsi
	cmpq	%rax, %rbx
	movq	48(%rsi), %r12
	movq	40(%rsi), %r8
	movq	%rsi, %rdi
	je	.L13168
.L13128:
	movq	16(%rbx), %rdx
	movq	%r12, %rsi
	xorl	%eax, %eax
	movq	%r8, -328(%rbp)
	call	tree_cons
	movq	-328(%rbp), %r8
	movq	%rax, 16(%rbx)
.L12977:
	movq	-280(%rbp), %rcx
	testq	%r12, %r12
	movq	%r14, 48(%rcx)
	je	.L13169
.L12988:
	movq	96(%r14), %rax
	testq	%rax, %rax
	jne	.L12990
	testq	%r12, %r12
	je	.L12991
	testb	$1, 53(%r14)
	jne	.L12991
	cmpb	$34, 16(%r12)
	je	.L13170
.L12991:
	movl	warn_shadow(%rip), %r11d
	testl	%r11d, %r11d
	je	.L12990
	testb	$1, 53(%r14)
	jne	.L12990
	movl	32(%r14), %ecx
	testl	%ecx, %ecx
	je	.L12990
	testq	%rax, %rax
	jne	.L12990
	xorl	%edi, %edi
	testq	%r12, %r12
	je	.L12996
	cmpb	$34, 16(%r12)
	je	.L13171
.L12996:
	movq	-280(%rbp), %rsi
	cmpq	$0, 56(%rsi)
	je	.L12998
	movl	$.LC41, %edi
.L12997:
	testq	%rdi, %rdi
	jne	.L13172
.L12990:
	testq	%r12, %r12
	je	.L13105
	movq	-312(%rbp), %rax
	movq	-280(%rbp), %rdi
	movq	%r12, %rsi
	movq	16(%rax), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	-312(%rbp), %r12
	movq	%rax, 16(%r12)
.L13105:
	movzbl	16(%r14), %ecx
.L12975:
	leal	-128(%rcx), %edi
	cmpb	$1, %dil
	jbe	.L12763
	movq	8(%r14), %rdx
	cmpq	$0, 32(%rdx)
	jne	.L12763
	cmpb	$18, 16(%rdx)
	je	.L13173
.L13007:
	testb	$64, 46(%rdx)
	je	.L12763
.L13006:
	movq	-312(%rbp), %r10
	movzwl	64(%r10), %eax
	incl	%eax
	testw	%ax, %ax
	movw	%ax, 64(%r10)
	je	.L13174
.L13106:
	movzbl	16(%r14), %ecx
.L12763:
	cmpb	$32, %cl
	je	.L13175
.L13009:
	movq	-312(%rbp), %rdx
	cmpq	global_binding_level(%rip), %rdx
	movq	(%rdx), %rbx
	movq	%rbx, (%r14)
	movq	%r14, (%rdx)
	je	.L13176
.L13035:
	movq	%r14, %r12
	jmp	.L12782
.L13176:
	testb	$4, 17(%r14)
	jne	.L13035
	movl	$124, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L13035
.L13175:
	cmpq	$0, -280(%rbp)
	je	.L13009
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L13010
	cmpq	class_binding_level(%rip), %rax
	je	.L13011
	movq	-280(%rbp), %r11
	movq	48(%r11), %rax
	testq	%rax, %rax
	je	.L13015
	cmpb	$32, 16(%rax)
	je	.L13013
.L13015:
	cmpq	$0, current_class_type(%rip)
	je	.L13010
	movq	-280(%rbp), %rsi
	movq	56(%rsi), %rax
	testq	%rax, %rax
	je	.L13010
	cmpb	$32, 16(%rax)
	je	.L13013
.L13010:
	movq	-280(%rbp), %rsi
	movq	40(%rsi), %rax
	testq	%rax, %rax
	je	.L13014
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L13013
	cmpb	$-127, %dl
	je	.L13177
.L13014:
	movq	current_class_name(%rip), %r9
	testq	%r9, %r9
	movq	%r9, -216(%rbp)
	je	.L13009
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	je	.L13178
.L13021:
	movq	current_binding_level(%rip), %rax
	cmpq	global_binding_level(%rip), %rax
	je	.L13025
	cmpq	class_binding_level(%rip), %rax
	je	.L13026
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.L13030
	cmpb	$32, 16(%rax)
	je	.L13028
.L13030:
	cmpq	$0, current_class_type(%rip)
	je	.L13025
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.L13025
	cmpb	$32, 16(%rax)
	je	.L13028
.L13025:
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.L13009
	movzbl	16(%rax), %edx
	cmpb	$32, %dl
	je	.L13028
	cmpb	$-127, %dl
	jne	.L13009
	movq	$0, 8(%rbx)
	jmp	.L13009
.L13028:
	movq	8(%rax), %r12
	movq	%r12, 8(%rbx)
	jmp	.L13009
.L13026:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L13030
.L13178:
	cmpb	$32, 16(%r14)
	movl	$136, %esi
	movq	56(%r14), %rbx
	sete	%r11b
	movq	8(%r14), %r12
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movl	$137, %esi
	movq	-216(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	cmpb	$1, 16(%rbx)
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movl	$.LC35, %esi
	movq	-216(%rbp), %r8
	movl	24(%rbx), %eax
	addl	24(%r8), %eax
	movq	32(%r8), %rdx
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r14)
	movq	%r12, 8(%rax)
	movq	72(%r14), %rbx
	jmp	.L13021
.L13177:
	movq	$0, 8(%rsi)
	jmp	.L13014
.L13013:
	movq	8(%rax), %rcx
	movq	-280(%rbp), %rdx
	movq	%rcx, 8(%rdx)
	jmp	.L13014
.L13011:
	movl	$7, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
	jmp	.L13015
.L13174:
	movl	$.LC44, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L13106
.L13173:
	movq	8(%rdx), %r8
	testb	$64, 46(%r8)
	jne	.L13006
	jmp	.L13007
.L13172:
	movq	-280(%rbp), %rbx
	movq	32(%rbx), %rsi
.L13129:
	xorl	%eax, %eax
	call	warning
	jmp	.L12990
.L12998:
	testq	%r12, %r12
	je	.L13000
	movl	$.LC42, %edi
	jmp	.L12997
.L13000:
	testq	%r8, %r8
	movl	$.LC43, %r9d
	cmovne	%r9, %rdi
	jmp	.L12997
	.p2align 6,,7
.L13171:
	movl	$.LC40, %edi
	jmp	.L12997
.L13170:
	cmpb	$34, 16(%r14)
	je	.L12991
	cmpq	$0, cleanup_label(%rip)
	movq	current_binding_level(%rip), %r8
	movq	56(%r8), %rax
	je	.L12992
	movq	56(%rax), %rax
.L12992:
	movzbl	66(%rax), %r10d
	andl	$15, %r10d
	decl	%r10d
	jne	.L12990
	movq	-280(%rbp), %rdx
	movl	$.LC40, %edi
	movq	32(%rdx), %rsi
	jmp	.L13129
	.p2align 6,,7
.L13169:
	movzbl	53(%r14), %esi
	andb	$9, %sil
	decb	%sil
	je	.L13179
.L12980:
	testq	%r12, %r12
	jne	.L12988
	testq	%r8, %r8
	jne	.L12988
	testb	$1, 53(%r14)
	je	.L12988
	testb	$8, 18(%r14)
	je	.L12988
	movq	-280(%rbp), %rax
	orb	$8, 18(%rax)
	jmp	.L12988
	.p2align 6,,7
.L13179:
	testq	%r8, %r8
	je	.L12980
	cmpb	$29, 16(%r14)
	jne	.L12980
	cmpb	$29, 16(%r8)
	jne	.L12980
	movq	8(%r8), %rsi
	movq	8(%r14), %rdi
	xorl	%eax, %eax
	movl	$1, %edx
	movq	%r8, -328(%rbp)
	call	comptypes
	movq	-328(%rbp), %r8
	testl	%eax, %eax
	je	.L13180
	movzbl	53(%r8), %ecx
	movl	%ecx, %eax
	shrb	$3, %al
	andl	$1, %eax
	je	.L12983
	movzbl	53(%r14), %r9d
	leal	0(,%rax,8), %ebx
	leaq	88(%r14), %rdx
	andb	$-9, %r9b
	orb	%bl, %r9b
	movb	%r9b, 53(%r14)
	cmpq	%r8, current_function_decl(%rip)
	je	.L12984
	movq	88(%r8), %rax
.L12985:
	movq	80(%r8), %rcx
	movq	136(%r8), %r9
	movq	%rax, (%rdx)
	movq	72(%r8), %rsi
	movzbl	17(%r14), %edi
	movq	%r9, 136(%r14)
	movq	%rcx, 80(%r14)
	movq	%rsi, 72(%r14)
	movzbl	17(%r8), %r11d
	movq	%r8, 96(%r14)
	andb	$127, %dil
	shrb	$7, %r11b
	movzbl	%r11b, %edx
	movl	%edx, %r10d
	salb	$7, %r10b
	orb	%r10b, %dil
	movb	%dil, 17(%r14)
	movzbl	53(%r8), %ecx
.L12983:
	shrb	$4, %cl
	movl	%ecx, %eax
	andl	$1, %eax
	je	.L12986
	movzbl	53(%r14), %ebx
	salb	$4, %al
	andb	$-17, %bl
	orb	%al, %bl
	movb	%bl, 53(%r14)
	movl	128(%r8), %eax
	movl	%eax, 128(%r14)
.L12986:
	movq	8(%r8), %rdx
	cmpq	$0, 24(%rdx)
	je	.L12980
	cmpq	$0, 88(%r8)
	je	.L12980
	movq	8(%r14), %rdi
	cmpq	$0, 24(%rdi)
	jne	.L12980
	movq	%rdx, 8(%r14)
	jmp	.L12980
.L12984:
	xorl	%eax, %eax
	jmp	.L12985
.L13180:
	movq	%r14, %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	movq	-328(%rbp), %r8
	jmp	.L12980
	.p2align 6,,7
.L13168:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L12977
	movq	-280(%rbp), %rdi
	jmp	.L13128
.L13167:
	movzwl	16(%r14), %eax
	andl	$8447, %eax
	cmpw	$8225, %ax
	je	.L13181
.L12952:
	movq	-280(%rbp), %rbx
	cmpq	$0, 40(%rbx)
	jne	.L12953
	testb	$8, 18(%r14)
	je	.L12953
	orb	$8, 18(%rbx)
.L12953:
	movzbl	16(%r14), %ecx
	cmpb	$32, %cl
	je	.L13182
.L12955:
	movq	-280(%rbp), %r12
	movq	%r14, 40(%r12)
	movzbl	16(%r14), %ecx
.L12954:
	movq	-280(%rbp), %rbx
	movq	72(%rbx), %rax
	testq	%rax, %rax
	je	.L12966
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.L12956
	testb	$1, 18(%rdx)
	je	.L12956
	orb	$1, 18(%r14)
	movq	-280(%rbp), %rdx
	movq	72(%rdx), %rax
.L12956:
	testq	%rax, %rax
	je	.L12966
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.L12961
	testb	$8, 17(%rdx)
	je	.L12961
	orb	$8, 17(%r14)
	movq	-280(%rbp), %rdi
	movq	72(%rdi), %rax
.L12961:
	testq	%rax, %rax
	je	.L12966
	cmpq	$0, 8(%rax)
	je	.L12966
	cmpb	$29, %cl
	je	.L13183
.L12969:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC38, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	warning
	movzbl	16(%r14), %ecx
.L12966:
	movq	-280(%rbp), %rbx
	testb	$8, 18(%rbx)
	je	.L12975
	cmpb	$32, %cl
	je	.L12975
	testb	$8, 18(%r14)
	jne	.L12975
	testb	$1, 53(%r14)
	jne	.L12975
	movq	72(%rbx), %rax
	testq	%rax, %rax
	je	.L12971
	cmpq	$0, 8(%rax)
	jne	.L13184
.L12971:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L13134:
	xorl	%eax, %eax
	call	warning
	jmp	.L13105
.L13184:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L13134
.L13183:
	movq	8(%r14), %r11
	movq	integer_type_node(%rip), %r10
	cmpq	%r10, 8(%r11)
	jne	.L12969
	jmp	.L12966
	.p2align 6,,7
.L13182:
	testq	%r12, %r12
	je	.L12955
	cmpb	$32, 16(%r12)
	jne	.L12954
	jmp	.L12955
.L13181:
	testb	$8, 54(%r14)
	jne	.L12952
	andb	$-9, 18(%r14)
	jmp	.L12952
	.p2align 6,,7
.L13104:
	movq	global_binding_level(%rip), %rax
	jmp	.L12932
.L13166:
	testl	%edx, %edx
	jg	.L13127
	xorl	%esi, %esi
	testl	%esi, %esi
	je	.L12938
	movq	-280(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L13127
	.p2align 6,,7
.L12946:
	movq	8(%rcx), %r8
	cmpq	error_mark_node(%rip), %r8
	cmove	%r8, %rcx
	jmp	.L12938
.L13165:
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	movq	-280(%rbp), %rsi
	cmpl	$-1, %edx
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L12938
	testq	%rax, %rax
	je	.L13080
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L12936
	.p2align 6,,7
.L12935:
	movq	-280(%rbp), %rdi
	movq	40(%rdi), %rcx
	jmp	.L12934
.L13164:
	movq	8(%rax), %rsi
	movq	8(%r14), %rdi
	movl	$1, %edx
	xorl	%eax, %eax
	call	comptypes
	testl	%eax, %eax
	jne	.L12929
	movq	%r14, %rdi
	movl	$.LC36, %esi
	xorl	%eax, %eax
	call	warning_with_decl
	xorl	%eax, %eax
	movl	$.LC37, %esi
	movq	-280(%rbp), %rdx
	movq	40(%rdx), %rdi
	call	warning_with_decl
	jmp	.L12929
	.p2align 6,,7
.L13109:
	movq	8(%r14), %rdx
	movq	80(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, -304(%rbp)
	je	.L12793
	movzbl	16(%rbx), %eax
	cmpb	$32, %al
	je	.L12792
.L12793:
	movq	global_binding_level(%rip), %r11
	movq	%r14, -304(%rbp)
	cmpq	%r11, current_binding_level(%rip)
	jne	.L13097
	movq	%r14, 80(%rdx)
.L13097:
	movzbl	16(%r14), %eax
.L12796:
	cmpb	$32, %al
	movl	$140, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movq	-304(%rbp), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.L12920
	cmpq	$0, 72(%rax)
	je	.L13185
.L12920:
	movq	8(%r14), %rbx
	movq	%rbx, -200(%rbp)
	movq	80(%rbx), %rax
	testq	%rax, %rax
	je	.L12791
	cmpq	$0, 56(%rax)
	je	.L12791
	movq	current_binding_level(%rip), %rbx
	movq	56(%r14), %rax
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -208(%rbp)
	je	.L12925
	movq	8(%rax), %rsi
	movq	%rax, %rdi
.L13126:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L12926:
	movq	-200(%rbp), %rcx
	movq	-208(%rbp), %rdi
	movq	%rcx, 8(%rdi)
	jmp	.L12791
.L12925:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L12926
	movq	-208(%rbp), %r8
	movq	8(%r8), %rsi
	movq	%r8, %rdi
	jmp	.L13126
.L13185:
	movq	current_class_name(%rip), %rcx
	movl	$136, %esi
	movq	%rcx, -184(%rbp)
	movq	8(%r14), %rdi
	movq	%rdi, -192(%rbp)
	cmpb	$32, 16(%r14)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	cmpq	$0, -184(%rbp)
	je	.L12921
	movq	-184(%rbp), %rax
	movl	$137, %esi
	cmpb	$1, 16(%rax)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	cmpb	$1, 16(%rbx)
	sete	%sil
	xorl	%eax, %eax
	movzbl	%sil, %edi
	movl	$138, %esi
	call	my_friendly_assert
	movq	32(%rbx), %rcx
	movl	$.LC35, %esi
	movq	-184(%rbp), %r10
	movl	24(%rbx), %eax
	addl	24(%r10), %eax
	movq	32(%r10), %rdx
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	%rax, 72(%r14)
	movq	-192(%rbp), %rdx
	movq	%rdx, 8(%rax)
	jmp	.L12920
.L12921:
	movq	%rbx, 72(%r14)
	jmp	.L12920
.L12792:
	movq	-304(%rbp), %rdi
	movq	current_binding_level(%rip), %rsi
	cmpq	global_binding_level(%rip), %rsi
	movq	56(%rdi), %rcx
	movq	%rcx, -120(%rbp)
	jne	.L12796
	movq	32(%rcx), %rcx
	cmpb	$36, (%rcx)
	jne	.L12796
	cmpb	$95, 1(%rcx)
	jne	.L12796
	movq	class_binding_level(%rip), %rax
	movq	%r14, 80(%rdx)
	movq	8(%r14), %rcx
	testq	%rax, %rax
	movq	%rax, -288(%rbp)
	movq	%rcx, -128(%rbp)
	jne	.L12800
	testb	$-128, 66(%rsi)
	movq	%rsi, -288(%rbp)
	je	.L12800
.L12804:
	movq	-288(%rbp), %rbx
	movq	56(%rbx), %rsi
	testb	$-128, 66(%rsi)
	movq	%rsi, -288(%rbp)
	jne	.L12804
.L12800:
	movq	-288(%rbp), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L13186
	movq	-288(%rbp), %r10
	movq	-120(%rbp), %rdi
	xorl	%eax, %eax
	movq	-128(%rbp), %rsi
	movq	8(%r10), %rdx
	call	saveable_tree_cons
	movq	-288(%rbp), %r9
	movq	%rax, 8(%r9)
.L12806:
	cmpq	$0, -120(%rbp)
	je	.L12807
	movq	-128(%rbp), %rcx
	cmpq	$0, 80(%rcx)
	jne	.L12808
	movq	-120(%rbp), %rdx
	movq	%rdx, 80(%rcx)
.L12808:
	movq	-120(%rbp), %r11
	movq	-128(%rbp), %rax
	cmpq	%rax, 8(%r11)
	je	.L12809
	cmpb	$21, 16(%rax)
	je	.L13187
.L12810:
	movq	current_class_type(%rip), %rax
	testq	%rax, %rax
	je	.L12812
	cmpq	$0, 32(%rax)
	je	.L12811
.L12812:
	movq	lang_name_cplusplus(%rip), %rsi
	cmpq	%rsi, current_lang_name(%rip)
	je	.L13188
.L12813:
	xorl	%ecx, %ecx
.L12848:
	testq	%rcx, %rcx
	jne	.L12849
.L13081:
	movq	-120(%rbp), %rsi
	movq	-128(%rbp), %rdx
	movl	$32, %edi
	xorl	%eax, %eax
	call	build_decl
	movl	$1, %edx
	movl	$1, %esi
	movq	-128(%rbp), %rdi
	movq	%rax, -296(%rbp)
	xorl	%eax, %eax
	call	build_overload_name
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	-296(%rbp), %rbx
	movq	%rax, 112(%rbx)
	movl	$0, 32(%rbx)
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	je	.L12850
	movq	-120(%rbp), %rcx
	movq	8(%rcx), %rsi
	movq	%rcx, %rdi
.L13123:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L12851:
	movq	-128(%rbp), %r11
	movq	-120(%rbp), %rdx
	movq	%r11, 8(%rdx)
.L12854:
	movq	-120(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$36, (%rax)
	je	.L13189
.L12856:
	movq	-128(%rbp), %rsi
	movq	80(%rsi), %rdx
	testq	%rdx, %rdx
	je	.L12887
	cmpb	$32, 16(%rdx)
	je	.L13190
.L12857:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L12876
	movq	-296(%rbp), %rcx
	movq	56(%rcx), %rsi
	testq	%rsi, %rsi
	movq	%rsi, -144(%rbp)
	je	.L13099
	movq	%rsi, %rax
	movq	56(%rsi), %rsi
	testq	%rsi, %rsi
	je	.L12878
	movq	24(%rbx), %rdx
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 24(%rbx)
.L12878:
	movq	decl_obstack+24(%rip), %rdx
	movq	-296(%rbp), %r8
	movq	-144(%rbp), %rax
	leaq	8(%rdx), %r11
	cmpq	decl_obstack+32(%rip), %r11
	movq	%r8, 56(%rax)
	ja	.L13191
.L12880:
	movq	-296(%rbp), %rbx
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r9)
	cmpb	$32, 16(%rbx)
	je	.L13192
	movq	-120(%rbp), %rbx
	movq	32(%rbx), %rax
.L12887:
	cmpb	$36, (%rax)
	je	.L13193
.L12901:
	movq	current_class_type(%rip), %rdx
	movq	-296(%rbp), %rax
	movq	-128(%rbp), %rcx
	testq	%rdx, %rdx
	movq	%rax, 80(%rcx)
	jne	.L12904
	cmpq	$0, current_function_decl(%rip)
	je	.L12903
.L12904:
	movq	lang_name_cplusplus(%rip), %r9
	cmpq	%r9, current_lang_name(%rip)
	je	.L12902
.L12903:
	movq	-120(%rbp), %r10
	movq	-296(%rbp), %rdi
	movq	%r10, 72(%rdi)
.L12809:
	movq	-288(%rbp), %rax
	movzbl	66(%rax), %r11d
	andl	$15, %r11d
	cmpl	$2, %r11d
	je	.L13194
.L12807:
	movq	-128(%rbp), %rdx
	movq	80(%rdx), %rax
	cmpb	$32, 16(%rax)
	je	.L13195
	movq	-128(%rbp), %rdx
	movl	$32, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	-128(%rbp), %r11
	movq	%rax, (%r11)
	movq	-304(%rbp), %r10
	movzbl	16(%r10), %eax
	jmp	.L12796
.L13195:
	movq	%rax, (%rdx)
	movq	-304(%rbp), %rsi
	movzbl	16(%rsi), %eax
	jmp	.L12796
.L13194:
	movq	-128(%rbp), %rdx
	orb	$64, 18(%rdx)
	movq	80(%rdx), %rdi
	movq	-120(%rbp), %r8
	movq	current_class_type(%rip), %rax
	movq	%rdi, 56(%r8)
	cmpq	$0, 32(%rax)
	jne	.L12807
	movq	-288(%rbp), %r9
	movq	144(%rax), %rcx
	movq	8(%r9), %rbx
	movq	%rbx, 72(%rcx)
	jmp	.L12807
.L12902:
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.L13196
	cmpq	$0, 32(%rdx)
	jne	.L12809
	movq	-296(%rbp), %rsi
	movq	80(%rdx), %rcx
	cmpb	$32, 16(%rsi)
	movq	72(%rcx), %rbx
	movl	$136, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L12912
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-120(%rbp), %rdi
	cmpb	$1, 16(%rdi)
	sete	%r8b
	xorl	%eax, %eax
	movzbl	%r8b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movl	$.LC35, %esi
	movq	-120(%rbp), %r11
	movl	24(%r11), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-296(%rbp), %r10
	movq	%rax, 72(%r10)
	movq	-128(%rbp), %r9
	movq	%r9, 8(%rax)
.L12914:
	movq	-296(%rbp), %r10
	movq	current_class_type(%rip), %r9
	movq	152(%r10), %rbx
	movq	%r9, 64(%r10)
	movq	%r9, 16(%rbx)
	jmp	.L12809
.L12912:
	movq	-120(%rbp), %rcx
	movq	-296(%rbp), %rsi
	movq	%rcx, 72(%rsi)
	jmp	.L12914
.L13196:
	movq	-296(%rbp), %r8
	movq	112(%rax), %rbx
	movl	$136, %esi
	cmpb	$32, 16(%r8)
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L12907
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%r10b
	xorl	%eax, %eax
	movzbl	%r10b, %edi
	call	my_friendly_assert
	movl	$138, %esi
	movq	-120(%rbp), %r9
	cmpb	$1, 16(%r9)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	32(%rbx), %rdx
	movq	-120(%rbp), %rsi
	movl	24(%rsi), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	movq	-120(%rbp), %rax
	movq	%rsp, %rdi
	movq	32(%rax), %rcx
	xorl	%eax, %eax
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-296(%rbp), %rdx
	movq	%rax, 72(%rdx)
	movq	-128(%rbp), %rbx
	movq	%rbx, 8(%rax)
.L12909:
	movq	current_function_decl(%rip), %rbx
	movq	-296(%rbp), %r8
	movq	%rbx, 64(%r8)
	jmp	.L12809
.L12907:
	movq	-120(%rbp), %r11
	movq	-296(%rbp), %rdi
	movq	%r11, 72(%rdi)
	jmp	.L12909
.L13193:
	cmpb	$95, 1(%rax)
	jne	.L12901
	movq	-296(%rbp), %rdx
	orb	$64, 53(%rdx)
	jmp	.L12901
.L13192:
	cmpq	$0, 72(%rbx)
	je	.L13197
.L13101:
	movq	-120(%rbp), %rsi
	movq	32(%rsi), %rax
	jmp	.L12887
.L13197:
	movq	-296(%rbp), %r10
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	8(%r10), %rdi
	movq	%rdi, -152(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L12882
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movl	$138, %esi
	movq	-144(%rbp), %r8
	cmpb	$1, 16(%r8)
	sete	%dl
	xorl	%eax, %eax
	movzbl	%dl, %edi
	call	my_friendly_assert
	movq	-144(%rbp), %r11
	movq	32(%rbx), %rdx
	movq	-144(%rbp), %rcx
	movl	$.LC35, %esi
	movl	24(%rcx), %eax
	movq	32(%r11), %rcx
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-296(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-152(%rbp), %rsi
	movq	%rsi, 8(%rax)
.L13098:
	movq	-120(%rbp), %r11
	movq	32(%r11), %rax
	jmp	.L12887
.L12882:
	movq	-144(%rbp), %rdi
	movq	-296(%rbp), %r9
	movq	%rdi, 72(%r9)
	jmp	.L13101
.L13191:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L12880
.L13099:
	movq	-120(%rbp), %r8
	movq	32(%r8), %rax
	jmp	.L12887
.L12876:
	movq	-296(%rbp), %rdi
	xorl	%eax, %eax
	call	pushdecl
	movq	%rax, -296(%rbp)
	jmp	.L13098
.L13190:
	movq	global_binding_level(%rip), %rdi
	cmpq	%rdi, current_binding_level(%rip)
	je	.L12858
	movq	-120(%rbp), %rbx
	movq	48(%rbx), %r10
	testq	%r10, %r10
	movq	%r10, %rcx
	jne	.L12859
.L12858:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L12860
	movq	-120(%rbp), %rdx
	movq	56(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.L13077
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jne	.L12861
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L13198
.L12861:
	testq	%rcx, %rcx
	jne	.L13077
.L13078:
	movq	-120(%rbp), %rdi
	xorl	%eax, %eax
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L13077
	movq	-120(%rbp), %r8
	movq	40(%r8), %rcx
.L12859:
	testq	%rcx, %rcx
	je	.L12863
.L13077:
	cmpb	$32, 16(%rcx)
	je	.L12863
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L12863
	movq	-120(%rbp), %rsi
	movq	8(%rsi), %rax
	testq	%rax, %rax
	je	.L12871
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L13124
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L13199
.L13124:
	movq	%rax, %rcx
.L12863:
	movq	-128(%rbp), %rdx
	cmpq	80(%rdx), %rcx
	jne	.L12857
	jmp	.L13098
.L13199:
	testl	%edx, %edx
	jg	.L13124
	movl	$1, %r10d
	testl	%r10d, %r10d
	je	.L12863
	movq	-120(%rbp), %rdi
	movq	%rax, %rdx
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L13124
.L12871:
	movq	8(%rcx), %rbx
	cmpq	error_mark_node(%rip), %rbx
	cmove	%rbx, %rcx
	jmp	.L12863
.L13198:
	movl	$1, %eax
	xorl	%ecx, %ecx
	movq	-120(%rbp), %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L12863
	testq	%rax, %rax
	je	.L13078
	cmpb	$32, 16(%rax)
	cmovne	%rbx, %rcx
	jmp	.L12861
.L12860:
	movq	-120(%rbp), %r9
	movq	40(%r9), %rcx
	jmp	.L12859
.L13189:
	cmpb	$95, 1(%rax)
	jne	.L12856
	jmp	.L12887
.L12850:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L12851
	movq	-120(%rbp), %rax
	movq	8(%rax), %rsi
	movq	%rax, %rdi
	jmp	.L13123
.L12849:
	movq	80(%rcx), %r8
	movq	%r8, -296(%rbp)
	jmp	.L12854
.L13188:
	xorl	%ebx, %ebx
	testq	%rax, %rax
	je	.L12815
	movq	80(%rax), %rbx
.L12815:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L12848
.L12847:
	movzbl	16(%rbx), %eax
	cmpl	$29, %eax
	je	.L12824
	cmpl	$32, %eax
	je	.L13200
	movl	$12, %edi
	xorl	%eax, %eax
	call	my_friendly_abort
.L12818:
	xorl	%ecx, %ecx
	testq	%rbx, %rbx
	je	.L12848
	jmp	.L12847
.L13200:
	movq	8(%rbx), %r9
	movq	-128(%rbp), %rdi
	xorl	%eax, %eax
	movq	144(%r9), %rcx
	movq	72(%rcx), %rsi
	call	value_member
	testq	%rax, %rax
	jne	.L13107
	movq	64(%rbx), %rbx
	jmp	.L12818
.L13107:
	movq	32(%rax), %rcx
	jmp	.L12848
.L12824:
	movq	-128(%rbp), %rax
	movq	80(%rax), %r10
	movq	56(%r10), %rbx
	testq	%rbx, %rbx
	je	.L12813
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L12827
	movq	48(%rbx), %r11
	testq	%r11, %r11
	movq	%r11, %rcx
	jne	.L12828
.L12827:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L12829
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.L13075
	movq	32(%rdi), %r8
	testq	%r8, %r8
	movq	%r8, -136(%rbp)
	jne	.L12830
	movq	144(%rdi), %rsi
	testb	$1, 3(%rsi)
	jne	.L13201
.L12830:
	testq	%rcx, %rcx
	jne	.L13075
.L13076:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.L13075
.L12829:
	movq	40(%rbx), %rcx
.L12828:
	testq	%rcx, %rcx
	je	.L13081
.L13075:
	cmpb	$32, 16(%rcx)
	je	.L12848
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L12848
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.L12840
	movq	80(%rax), %rax
	cmpq	%rax, %rcx
	je	.L13122
	movl	$1, %edi
	testl	%edi, %edi
	jle	.L13202
.L13122:
	movq	%rax, %rcx
	jmp	.L12848
.L13202:
	testl	%edx, %edx
	jg	.L13122
	movl	$1, %r10d
	testl	%r10d, %r10d
	je	.L12848
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L13122
.L12840:
	movq	8(%rcx), %rax
	cmpq	error_mark_node(%rip), %rax
	jne	.L12848
	jmp	.L13122
.L13201:
	movl	$1, %r9d
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	cmpl	$-1, %r9d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rcx
	je	.L12848
	testq	%rax, %rax
	je	.L13076
	cmpb	$32, 16(%rax)
	cmovne	-136(%rbp), %rcx
	jmp	.L12830
.L12811:
	movq	-120(%rbp), %rsi
	movq	-128(%rbp), %rdx
	xorl	%eax, %eax
	movl	$32, %edi
	call	build_lang_field_decl
	movq	current_binding_level(%rip), %rbx
	cmpq	global_binding_level(%rip), %rbx
	movq	%rax, -160(%rbp)
	je	.L12888
	movq	-120(%rbp), %r10
	movq	8(%r10), %rsi
	movq	%r10, %rdi
.L13125:
	movq	32(%rbx), %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%rbx)
.L12889:
	movq	-120(%rbp), %rsi
	movq	-128(%rbp), %rcx
	movq	%rcx, 8(%rsi)
	movq	-160(%rbp), %rbx
	movq	56(%rbx), %rsi
	testq	%rsi, %rsi
	movq	%rsi, -168(%rbp)
	je	.L12892
	movq	%rsi, %rax
	movq	56(%rsi), %rsi
	testq	%rsi, %rsi
	je	.L12893
	movq	class_binding_level(%rip), %rbx
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rdx
	call	tree_cons
	movq	%rax, 24(%rbx)
.L12893:
	movq	-160(%rbp), %rdx
	movq	-168(%rbp), %rax
	movq	%rdx, 56(%rax)
	movq	decl_obstack+24(%rip), %rdx
	leaq	8(%rdx), %r8
	cmpq	decl_obstack+32(%rip), %r8
	ja	.L13203
.L12895:
	movq	-160(%rbp), %rbx
	movq	%rdx, %r9
	addq	$8, %rdx
	movq	%rdx, decl_obstack+24(%rip)
	movq	%rbx, (%r9)
	cmpb	$32, 16(%rbx)
	je	.L13204
.L12892:
	movq	-160(%rbp), %r10
	movq	%r10, -296(%rbp)
	jmp	.L13101
.L13204:
	cmpq	$0, 72(%rbx)
	jne	.L12892
	movq	-160(%rbp), %r10
	movq	current_class_name(%rip), %rbx
	xorl	%eax, %eax
	movl	$136, %esi
	movq	8(%r10), %rdi
	movq	%rdi, -176(%rbp)
	movl	$1, %edi
	call	my_friendly_assert
	testq	%rbx, %rbx
	je	.L12897
	cmpb	$1, 16(%rbx)
	movl	$137, %esi
	sete	%al
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	my_friendly_assert
	movl	$138, %esi
	movq	-168(%rbp), %rdx
	cmpb	$1, 16(%rdx)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %edi
	call	my_friendly_assert
	movq	-168(%rbp), %r8
	movq	32(%rbx), %rdx
	movq	-168(%rbp), %rsi
	movq	32(%r8), %rcx
	movl	24(%rsi), %eax
	movl	$.LC35, %esi
	addl	24(%rbx), %eax
	cltq
	addq	$19, %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	sprintf
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	get_identifier
	movq	-160(%rbp), %rbx
	movq	%rax, 72(%rbx)
	movq	-176(%rbp), %r11
	movq	%r11, 8(%rax)
	jmp	.L12892
.L12897:
	movq	-168(%rbp), %rdi
	movq	-160(%rbp), %r9
	movq	%rdi, 72(%r9)
	jmp	.L12892
.L13203:
	movl	$decl_obstack, %edi
	movl	$8, %esi
	xorl	%eax, %eax
	call	_obstack_newchunk
	movq	decl_obstack+24(%rip), %rdx
	jmp	.L12895
.L12888:
	movq	class_binding_level(%rip), %rbx
	testq	%rbx, %rbx
	je	.L12889
	movq	-120(%rbp), %r11
	movq	8(%r11), %rsi
	movq	%r11, %rdi
	jmp	.L13125
.L13187:
	cmpq	$0, class_binding_level(%rip)
	je	.L12810
	movq	144(%rax), %rdi
	testb	$16, 3(%rdi)
	jne	.L12809
	jmp	.L12810
.L13186:
	movq	-120(%rbp), %rdi
	movq	-128(%rbp), %rsi
	xorl	%eax, %eax
	movq	8(%rbx), %rdx
	call	perm_tree_cons
	movq	%rax, 8(%rbx)
	jmp	.L12806
	.p2align 6,,7
.L12777:
	xorl	%eax, %eax
	movq	%r14, %rdi
	movq	%r12, %rsi
	call	duplicate_decls
	testl	%eax, %eax
	je	.L13096
	movl	flag_traditional(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L12782
	movq	-280(%rbp), %r9
	testb	$8, 18(%r9)
	je	.L12782
	testb	$8, 18(%r14)
	jne	.L12782
	testb	$9, 53(%r14)
	jne	.L12782
	cmpq	%r14, current_function_decl(%rip)
	je	.L13205
.L12786:
	movq	-280(%rbp), %r10
	movq	72(%r10), %rax
	testq	%rax, %rax
	je	.L12787
	cmpq	$0, 8(%rax)
	jne	.L13206
.L12787:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC34, %edi
	movq	%rax, %rsi
.L13121:
	xorl	%eax, %eax
	call	warning
	xorl	%eax, %eax
	movq	%r12, %rdi
	call	lang_printable_name
	movl	$.LC6, %edx
	movl	-108(%rbp), %esi
	movq	%rax, %rcx
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	warning_with_file_and_line
	jmp	.L12782
.L13206:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC33, %edi
	movq	%rax, %rsi
	jmp	.L13121
.L13205:
	movq	%r12, current_function_decl(%rip)
	jmp	.L12786
	.p2align 6,,7
.L13163:
	cmpq	$0, 64(%r12)
	jne	.L12775
	movl	$.LC32, %edi
	xorl	%eax, %eax
	call	fatal
	jmp	.L12775
.L13162:
	movq	%r14, %rdi
	movl	$.LC31, %esi
	xorl	%eax, %eax
	xorl	%r12d, %r12d
	call	error_with_decl
	jmp	.L12773
.L12766:
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.L12768
.L12772:
	movq	-280(%rbp), %r12
	cmpq	%r12, 56(%rax)
	je	.L12768
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L12772
.L12768:
	movq	%rax, %r12
	jmp	.L12765
.L13161:
	movq	-280(%rbp), %rbx
	movq	40(%rbx), %r12
	jmp	.L12765
.L13160:
	movq	56(%r14), %rsi
	movq	%rsi, -280(%rbp)
	jmp	.L12762
.L13159:
	andb	$32, %dl
	jne	.L12760
	jmp	.L12761
.L13092:
	movzbl	16(%r14), %ecx
	jmp	.L12760
	.p2align 6,,7
.L12751:
	movq	8(%r15), %rsi
	movq	144(%rsi), %rax
	testq	%rax, %rax
	je	.L12749
	cmpq	$0, 128(%rax)
	je	.L12749
	movq	%r13, %rdi
.L13120:
	xorl	%eax, %eax
	call	abstract_virtuals_error
	jmp	.L12749
.L13155:
	movq	%r13, %rdi
	movq	%r15, %rsi
	jmp	.L13120
.L13154:
	xorl	%eax, %eax
	call	resume_temporary_allocation
	jmp	.L12747
	.p2align 6,,7
.L13153:
	cmpq	$0, 152(%r13)
	je	.L12740
	testb	$16, 54(%r13)
	je	.L12740
	testb	$4, 18(%r13)
	je	.L12745
	testq	%r14, %r14
	jne	.L12740
	orb	$1, 53(%r13)
	movq	%r13, %rdi
	movq	-224(%rbp), %rsi
	movl	$1, %edx
.L13119:
	xorl	%eax, %eax
	call	make_decl_rtl
	jmp	.L12731
.L13152:
	movq	88(%r13), %rdi
	testq	%rdi, %rdi
	je	.L12732
	cmpq	error_mark_node(%rip), %rdi
	je	.L12732
	cmpq	empty_init_node(%rip), %rdi
	je	.L12732
	xorl	%eax, %eax
	call	save_expr
	cmpq	$0, -224(%rbp)
	movq	%rax, 88(%r13)
	jne	.L13207
.L12733:
	testl	%ebx, %ebx
	jne	.L13074
	movl	16(%r13), %eax
	andl	$786688, %eax
	cmpl	$262144, %eax
	je	.L13208
.L12734:
	testl	%ebx, %ebx
	je	.L12736
.L13074:
	movzbl	18(%r13), %edx
	movl	%edx, %eax
	orb	$1, %al
	testb	$4, %al
	movb	%al, 18(%r13)
	je	.L12737
	movl	interface_unknown(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L12737
	orb	$9, %dl
	movzbl	53(%r13), %esi
	movb	%dl, 18(%r13)
	movzbl	interface_only(%rip), %edi
	andb	$-2, %sil
	andb	$1, %dil
	orb	%dil, %sil
	movb	%sil, 53(%r13)
.L12737:
	movq	-224(%rbp), %rsi
	movq	%r13, %rdi
	movl	%ebx, %edx
	jmp	.L13119
.L12736:
	movq	-224(%rbp), %rsi
	movq	%r13, %rdi
	xorl	%edx, %edx
	jmp	.L13118
.L13208:
	testb	$1, 53(%r13)
	jne	.L12734
	testb	$32, 46(%r15)
	jne	.L12734
	movzbl	52(%r13), %eax
	cmpb	$16, %al
	je	.L12734
	movzbl	%al, %edi
	xorl	%eax, %eax
	call	gen_reg_rtx
	xorl	%edx, %edx
	movq	88(%r13), %rdi
	movq	%rax, %rsi
	movq	%rax, 120(%r13)
	xorl	%eax, %eax
	call	store_expr
	orb	$-128, 17(%r13)
	jmp	.L12731
	.p2align 6,,7
.L13207:
	movq	-224(%rbp), %rdi
	xorl	%eax, %eax
	call	get_identifier
	movq	%rax, 112(%r13)
	jmp	.L12733
	.p2align 6,,7
.L13151:
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L12729
	testb	$4, 18(%r13)
	je	.L13209
.L12729:
	cmpb	$33, %dl
	jne	.L12730
	testb	$32, 53(%r13)
	je	.L12730
	movq	%r13, %rdi
	xorl	%esi, %esi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	make_decl_rtl
	jmp	.L12731
.L13209:
	xorl	%eax, %eax
	movq	%r15, %rdi
	call	type_needs_gc_entry
	testl	%eax, %eax
	jne	.L13210
.L13091:
	movzbl	16(%r13), %edx
	jmp	.L12729
.L13210:
	movl	current_function_obstack_index(%rip), %edi
	xorl	%eax, %eax
	incl	%edi
	movl	%edi, current_function_obstack_index(%rip)
	call	size_int
	movq	%rax, 80(%r13)
	jmp	.L13091
.L13150:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	pop_implicit_try_blocks
	movq	current_binding_level(%rip), %r11
	andb	$-5, 67(%r11)
	jmp	.L12728
.L13149:
	xorl	%eax, %eax
	call	end_temporary_allocation
	jmp	.L12727
	.p2align 6,,7
.L13146:
	testb	$32, 46(%r15)
	je	.L12719
	movl	-60(%rbp), %ebx
	testl	%ebx, %ebx
	jne	.L13211
.L12721:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	maybe_build_cleanup
	movl	-60(%rbp), %edx
	movq	%rax, -48(%rbp)
	testl	%edx, %edx
	jne	.L13212
.L13090:
	movzbl	16(%r13), %edx
	jmp	.L12719
.L13212:
	xorl	%eax, %eax
	call	resume_temporary_allocation
	jmp	.L13090
.L13211:
	xorl	%eax, %eax
	call	end_temporary_allocation
	jmp	.L12721
	.p2align 6,,7
.L13145:
	testb	$4, 18(%r13)
	je	.L12707
	cmpq	$0, 40(%r13)
	jne	.L12707
	movzbl	53(%r13), %eax
	testb	$1, %al
	je	.L12709
	cmpq	$0, 88(%r13)
	je	.L12708
.L12709:
	movq	%r13, %rdi
	movl	$.LC134, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movzbl	53(%r13), %eax
.L12708:
	xorl	%r14d, %r14d
.L12710:
	testb	$1, %al
	jne	.L12715
	testb	$4, 18(%r13)
	je	.L12714
.L12715:
	movq	40(%r13), %rdx
	testq	%rdx, %rdx
	je	.L12714
	testb	$2, 17(%rdx)
	je	.L13213
.L12714:
	testb	$1, %al
	jne	.L13090
	testb	$32, 46(%r15)
	je	.L13090
	xorl	%eax, %eax
	call	suspend_momentary
	testq	%r14, %r14
	movl	%eax, %ebx
	je	.L12717
	cmpb	$52, 16(%r14)
	je	.L13214
.L12717:
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	maybe_build_cleanup
	movq	%rax, -48(%rbp)
.L12718:
	movl	%ebx, %edi
	xorl	%eax, %eax
	call	resume_momentary
	jmp	.L13090
.L13214:
	movq	8(%r13), %rdi
	movq	8(%r14), %rsi
	xorl	%eax, %eax
	movl	$1, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L12717
	movq	48(%r14), %rsi
	movq	current_binding_level(%rip), %rcx
	movq	32(%r14), %r14
	movq	%rsi, -48(%rbp)
	orb	$2, 67(%rcx)
	movq	current_binding_level(%rip), %r11
	andb	$-5, 67(%r11)
	jmp	.L12718
	.p2align 6,,7
.L13213:
	movq	%r13, %rdi
	movl	$.LC135, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movzbl	53(%r13), %eax
	jmp	.L12714
	.p2align 6,,7
.L12707:
	movzbl	53(%r13), %eax
	testb	$1, %al
	movl	%eax, %edx
	jne	.L12711
	cmpq	$0, 40(%r13)
	je	.L13215
.L12711:
	andb	$1, %dl
	jne	.L12710
	movq	-232(%rbp), %rsi
	testb	$64, 46(%rsi)
	je	.L12710
	movq	%rsi, %rdi
	xorl	%eax, %eax
	call	note_debug_info_needed
.L13117:
	movzbl	53(%r13), %eax
	jmp	.L12710
.L13215:
	movq	%r13, %rdi
	movl	$.LC134, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movq	error_mark_node(%rip), %r10
	movq	%r10, 8(%r13)
	jmp	.L13117
	.p2align 6,,7
.L13144:
	cmpq	$0, 24(%r15)
	jne	.L13086
	movzbl	16(%r13), %edx
	cmpb	$32, %dl
	je	.L12695
	testb	$4, 18(%r13)
	je	.L12696
	movl	pedantic(%rip), %eax
	xorl	%r12d, %r12d
	testl	%eax, %eax
	je	.L12697
	testb	$1, 53(%r13)
	je	.L12697
	movl	$1, %r12d
.L12697:
	testq	%r14, %r14
	movq	%r14, %rsi
	jne	.L12699
	movq	88(%r13), %rsi
.L12699:
	xorl	%eax, %eax
	movq	%r15, %rdi
	movl	%r12d, %edx
	call	complete_array_type
	cmpl	$1, %eax
	movl	%eax, %ebx
	je	.L13216
.L12700:
	cmpl	$2, %ebx
	je	.L13217
.L13087:
	movl	pedantic(%rip), %eax
.L12701:
	testl	%eax, %eax
	je	.L12705
.L13108:
	movq	24(%r15), %rax
	testq	%rax, %rax
	jne	.L13218
.L12705:
	movq	%r13, %rdi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	layout_decl
	jmp	.L13086
.L13218:
	movq	96(%rax), %rdi
	movq	integer_zero_node(%rip), %rsi
	xorl	%eax, %eax
	call	tree_int_cst_lt
	testl	%eax, %eax
	je	.L12705
	movq	%r13, %rdi
	movl	$.LC133, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12705
.L13217:
	testl	%r12d, %r12d
	jne	.L13219
	movl	pedantic(%rip), %eax
	testl	%eax, %eax
	jne	.L13108
	testb	$4, 18(%r13)
	je	.L12701
	orb	$1, 53(%r13)
	jmp	.L13087
.L13219:
	movq	%r13, %rdi
	movl	$.LC132, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L13087
.L13216:
	movq	%r13, %rdi
	movl	$.LC131, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12700
.L12696:
	testb	$1, 53(%r13)
	sete	%r9b
	movzbl	%r9b, %r12d
	jmp	.L12697
	.p2align 6,,7
.L13143:
	movl	-60(%rbp), %eax
	testl	%eax, %eax
	je	.L12694
	xorl	%eax, %eax
	call	end_temporary_allocation
	jmp	.L12694
.L12690:
	movq	empty_init_node(%rip), %rsi
	movq	%r15, %rdi
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	digest_init
	jmp	.L13116
.L13142:
	cmpq	$0, 40(%r14)
	je	.L12671
	movq	%r13, %rdi
	movl	$.LC127, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movq	error_mark_node(%rip), %r14
	jmp	.L12671
.L13141:
	movq	%r14, %rsi
	movq	%r15, %rdi
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	digest_init
	movq	%rax, %r14
	jmp	.L12671
.L12669:
	cmpb	$4, 16(%r14)
	je	.L12676
	movq	%r14, %rsi
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	store_init_value
	movq	%rax, %r14
.L12676:
	testq	%r14, %r14
	je	.L12665
	jmp	.L13133
.L12668:
	movzbl	16(%r15), %ebx
	movzbl	%bl, %r8d 
	salq	$3, %r8
	addq	tree_code_type(%rip), %r8
	movq	(%r8), %rdi
	cmpb	$116, (%rdi)
	je	.L13220
.L12679:
	cmpb	$33, %cl
	jne	.L12665
	cmpb	$15, %bl
	je	.L12665
	testb	$32, 17(%r15)
	jne	.L12693
	testb	$32, 17(%r13)
	je	.L12665
.L12693:
	movq	%r13, %rdi
	movl	$.LC130, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12665
.L13220:
	testb	$80, 46(%r15)
	je	.L12679
	cmpb	$18, %bl
	movq	%r15, %r12
	je	.L12683
.L13068:
	movq	144(%r12), %rax
	movzbl	1(%rax), %ecx
	testb	$1, %cl
	jne	.L12684
	andb	$8, %cl
	jne	.L13221
.L12685:
	testb	$16, 1(%rax)
	jne	.L13222
.L13084:
	movzbl	16(%r13), %edx
.L12684:
	cmpb	$33, %dl
	je	.L13223
.L12687:
	movl	flag_pic(%rip), %eax
	testl	%eax, %eax
	jne	.L12665
	movzbl	18(%r13), %r12d
	andb	$12, %r12b
	cmpb	$12, %r12b
	jne	.L12665
	testb	$1, 53(%r13)
	jne	.L12665
	cmpb	$33, 16(%r13)
	jne	.L12665
	testb	$16, 46(%r15)
	je	.L12665
	movq	88(%r13), %rax
	testq	%rax, %rax
	je	.L12690
	cmpq	error_mark_node(%rip), %rax
	jne	.L12665
	jmp	.L12690
	.p2align 6,,7
.L13223:
	testb	$16, 46(%r15)
	jne	.L12687
	testb	$32, 17(%r15)
	jne	.L12688
	testb	$32, 17(%r13)
	je	.L12687
.L12688:
	movq	%r13, %rdi
	movl	$.LC130, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12687
.L13222:
	movq	%r13, %rdi
	movl	$.LC129, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L13084
.L13221:
	movq	%r13, %rdi
	movl	$.LC128, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movq	144(%r12), %rax
	jmp	.L12685
	.p2align 6,,7
.L12683:
	movq	8(%r12), %r12
	cmpb	$18, 16(%r12)
	je	.L12683
	jmp	.L13068
.L13140:
	movl	$1, %edi
	movl	$148, %esi
	xorl	%eax, %eax
	call	my_friendly_assert
	movl	$149, %esi
	testq	%r14, %r14
	setne	%bl
	movq	%r14, 88(%r13)
	xorl	%eax, %eax
	xorl	%r14d, %r14d
	movzbl	%bl, %edi
	call	my_friendly_assert
	jmp	.L12665
.L13139:
	testq	%r14, %r14
	movq	$0, -248(%rbp)
	jne	.L12579
	cmpq	$0, 152(%r13)
	je	.L12581
	testb	$16, 54(%r13)
	jne	.L12583
.L12581:
	movl	$.LC117, %edi
	xorl	%eax, %eax
	call	error
.L13131:
	cmpb	$33, 16(%r13)
	je	.L13224
	.p2align 4,,7
.L12583:
	xorl	%r14d, %r14d
	jmp	.L12573
.L13224:
	movq	error_mark_node(%rip), %r14
	movq	%r14, 72(%r13)
	jmp	.L12583
.L12579:
	cmpb	$3, 16(%r14)
	je	.L13225
.L12584:
	movq	8(%r14), %rax
	cmpb	$15, 16(%rax)
	sete	%r9b
	movzbl	%r9b, %r12d
	testl	%r12d, %r12d
	jne	.L13226
	movq	%r14, %rbx
.L12586:
	testl	%r12d, %r12d
	jne	.L13227
	movq	8(%r15), %rcx
	cmpb	$18, 16(%rcx)
	je	.L13083
	movq	8(%r14), %rdx
	cmpb	$18, 16(%rdx)
	je	.L13228
.L12592:
	movzbl	16(%rdx), %r9d
	cmpb	%r9b, 16(%rcx)
	je	.L13229
	cmpq	error_mark_node(%rip), %r14
	je	.L12591
	movq	112(%rdx), %rsi
	movq	112(%rcx), %rdi
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	comptypes
	movl	$.LC123, %r11d
	testl	%eax, %eax
	cmovne	-248(%rbp), %r11
	movq	%r11, -248(%rbp)
.L12591:
	cmpq	$0, -248(%rbp)
	je	.L12632
	movq	8(%rbx), %rcx
	testb	$64, 46(%rcx)
	jne	.L13230
	movq	8(%r15), %rdi
	movzbl	46(%rdi), %ebx
	andb	$68, %bl
	cmpb	$68, %bl
	je	.L13231
.L12606:
	cmpq	$0, -248(%rbp)
	jne	.L13232
.L12632:
	movq	8(%r15), %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	testl	%r12d, %r12d
	movq	%rax, %rbx
	je	.L12634
	testb	$1, 17(%r14)
	jne	.L13233
	movq	%r14, 88(%r13)
.L12631:
	movq	8(%r15), %rsi
	cmpq	$0, 32(%rsi)
	jne	.L13234
.L12660:
	testb	$4, 18(%r13)
	je	.L12583
	movq	88(%r13), %rsi
	testb	$2, 17(%rsi)
	jne	.L12583
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	expand_static_init
	movq	$0, 88(%r13)
	jmp	.L12583
.L13234:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	convert_from_reference
	testb	$4, 17(%r13)
	jne	.L13235
.L12661:
	movq	%rax, 72(%r13)
	jmp	.L12660
.L13235:
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	copy_to_permanent
	jmp	.L12661
.L13233:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	save_expr
.L13113:
	movq	%rax, 88(%r13)
	jmp	.L12631
.L12634:
	xorl	%eax, %eax
	movq	%r14, %rdi
	call	lvalue_p
	testl	%eax, %eax
	je	.L12638
	movq	%r14, %rsi
	xorl	%edx, %edx
	xorl	%eax, %eax
	movl	$106, %edi
	call	build_unary_op
	cmpb	$106, 16(%rax)
	movq	%rax, %rsi
	je	.L13236
.L12639:
	movq	8(%rbx), %rdi
	testb	$64, 46(%rdi)
	je	.L12640
	xorl	%eax, %eax
	call	convert_pointer_to
.L13111:
	movq	%rax, 88(%r13)
	xorl	%eax, %eax
	movq	88(%r13), %rdi
	call	save_expr
	cmpq	current_class_decl(%rip), %rax
	movq	%rax, 88(%r13)
	je	.L13237
.L12642:
	movq	88(%r13), %rax
.L13112:
	movq	%r15, 8(%rax)
	jmp	.L12631
.L13237:
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	copy_node
	movq	%rax, 88(%r13)
	jmp	.L12642
.L12640:
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	convert
	jmp	.L13111
.L13236:
	movq	32(%rax), %rax
	cmpb	$52, 16(%rax)
	jne	.L12639
	movq	48(%rax), %rcx
	movq	error_mark_node(%rip), %r11
	movq	%rcx, -48(%rbp)
	movq	32(%rsi), %r10
	movq	%r11, 48(%r10)
	jmp	.L12639
	.p2align 6,,7
.L12638:
	xorl	%eax, %eax
	movl	$106, %edi
	movq	%r14, %rsi
	call	unary_complex_lvalue
	testq	%rax, %rax
	movq	%rax, %rdx
	je	.L12644
	cmpb	$106, 16(%rax)
	je	.L13238
.L12645:
	movq	8(%rbx), %rdi
	movq	%rdx, %rsi
	xorl	%eax, %eax
	call	convert_pointer_to
	movq	%rax, %rdi
	movq	%rax, 88(%r13)
	xorl	%eax, %eax
	call	save_expr
	movq	%rax, 88(%r13)
	jmp	.L13112
.L13238:
	movq	32(%rax), %r12
	cmpb	$47, 16(%r12)
	jne	.L12645
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	save_expr
	movq	%rax, %rdx
	jmp	.L12645
	.p2align 6,,7
.L12644:
	movq	8(%r15), %rsi
	testb	$32, 17(%rsi)
	je	.L12647
	movq	8(%r13), %rdi
	movq	global_binding_level(%rip), %rax
	movq	$0, -264(%rbp)
	cmpq	%rax, current_binding_level(%rip)
	sete	%dl
	xorl	%eax, %eax
	movq	%rdi, -88(%rbp)
	movq	8(%rdi), %r12
	movzbl	%dl, %esi
	movq	%r12, %rdi
	call	get_temp_name
	xorl	%edx, %edx
	movl	$106, %edi
	movq	%rax, %rsi
	movq	%rax, -256(%rbp)
	xorl	%eax, %eax
	call	build_unary_op
	movq	%r12, %rdi
	movq	%rax, %rbx
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%rbx, %rsi
	movq	%rax, 8(%rbx)
	xorl	%eax, %eax
	movq	56(%r12), %rdi
	call	convert
	movq	-88(%rbp), %rbx
	movq	%rax, 88(%r13)
	movq	%rbx, 8(%rax)
	testb	$16, 46(%r12)
	je	.L12651
	movq	global_binding_level(%rip), %r8
	cmpq	%r8, current_binding_level(%rip)
	je	.L13239
	cmpq	$0, -264(%rbp)
	je	.L13240
.L12655:
	movq	-88(%rbp), %rsi
	movq	-264(%rbp), %rdx
	movl	$44, %edi
	movq	88(%r13), %rcx
	xorl	%eax, %eax
	call	build
	movq	-256(%rbp), %rdi
	movq	%rax, 88(%r13)
	xorl	%eax, %eax
	call	maybe_build_cleanup
	movq	%rax, -48(%rbp)
.L12656:
	movq	-256(%rbp), %r14
	testb	$4, 18(%r14)
.L13132:
	je	.L12631
	xorl	%eax, %eax
	call	preserve_initializer
	jmp	.L12631
.L13240:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	constructor_name
	movq	%r14, %rsi
	xorl	%edi, %edi
	movq	%rax, %r12
	xorl	%eax, %eax
	call	build_tree_list
	movl	$3, %r8d
	xorl	%ecx, %ecx
	movq	-256(%rbp), %rdi
	movq	%rax, %rdx
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	build_method_call
	movq	%rax, -264(%rbp)
	jmp	.L12655
.L13239:
	movq	-256(%rbp), %rdi
	xorl	%esi, %esi
	movl	$1, %edx
	xorl	%eax, %eax
	call	make_decl_rtl
	xorl	%eax, %eax
	movq	static_aggregates(%rip), %rdx
	movq	-256(%rbp), %rsi
	movq	%r14, %rdi
	call	perm_tree_cons
	movq	%rax, static_aggregates(%rip)
	jmp	.L12656
.L12651:
	movq	-256(%rbp), %r9
	movq	global_binding_level(%rip), %rcx
	movq	%r14, %rsi
	cmpq	%rcx, current_binding_level(%rip)
	sete	%r11b
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	movzbl	18(%r9), %r10d
	movq	%r14, 88(%r9)
	movq	%r9, %rdi
	xorl	%edx, %edx
	salb	$2, %r11b
	andb	$-5, %r10b
	orb	%r11b, %r10b
	movb	%r10b, 18(%r9)
	call	finish_decl
	jmp	.L12656
.L12647:
	movq	%r13, %rdi
	movl	$.LC126, %esi
	xorl	%eax, %eax
	call	error_with_decl
	movq	error_mark_node(%rip), %rax
	jmp	.L13113
.L13232:
	movq	-248(%rbp), %rsi
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L13131
.L13231:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	sete	%al
	movzbl	%al, %esi
	xorl	%eax, %eax
	call	get_temp_name
	movq	8(%r15), %rdi
	movq	%rax, %rbx
	xorl	%eax, %eax
	call	constructor_name
	movq	%r14, %rsi
	xorl	%edi, %edi
	movq	%rax, %r12
	xorl	%eax, %eax
	call	build_tree_list
	movl	$3, %r8d
	xorl	%ecx, %ecx
	movq	%rax, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	call	build_method_call
	testq	%rax, %rax
	movq	%rax, -240(%rbp)
	je	.L12618
	cmpq	error_mark_node(%rip), %rax
	je	.L12618
	movq	8(%r13), %r8
	cmpq	$0, -240(%rbp)
	movq	%r8, -72(%rbp)
	movq	8(%r8), %rdi
	movq	%rdi, -80(%rbp)
	je	.L12620
	movq	-240(%rbp), %r10
	movq	40(%r10), %r9
	movq	32(%r9), %rbx
	movzbl	16(%rbx), %eax
	cmpb	$102, %al
	je	.L13241
.L12621:
	cmpb	$106, %al
	movl	$146, %esi
	sete	%r11b
	xorl	%eax, %eax
	movzbl	%r11b, %edi
	call	my_friendly_assert
	movq	32(%rbx), %r12
.L12622:
	movq	-80(%rbp), %rdi
	xorl	%eax, %eax
	call	build_pointer_type
	movq	%rbx, %rsi
	movq	%rax, 8(%rbx)
	xorl	%eax, %eax
	movq	-80(%rbp), %rdx
	movq	56(%rdx), %rdi
	call	convert
	movq	-72(%rbp), %rbx
	movq	%rax, 88(%r13)
	movq	%rbx, 8(%rax)
	movq	-80(%rbp), %rsi
	testb	$16, 46(%rsi)
	je	.L12623
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L13242
	cmpq	$0, -240(%rbp)
	je	.L13243
.L12627:
	movq	-72(%rbp), %rsi
	movq	-240(%rbp), %rdx
	movl	$44, %edi
	movq	88(%r13), %rcx
	xorl	%eax, %eax
	call	build
	movq	%r12, %rdi
	movq	%rax, 88(%r13)
	xorl	%eax, %eax
	call	maybe_build_cleanup
	movq	%rax, -48(%rbp)
.L12628:
	testb	$4, 18(%r12)
	jmp	.L13132
.L13243:
	movq	-80(%rbp), %rdi
	xorl	%eax, %eax
	call	constructor_name
	movq	%r14, %rsi
	xorl	%edi, %edi
	movq	%rax, %rbx
	xorl	%eax, %eax
	call	build_tree_list
	movl	$3, %r8d
	xorl	%ecx, %ecx
	movq	%rax, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	build_method_call
	movq	%rax, -240(%rbp)
	jmp	.L12627
.L13242:
	movq	%r12, %rdi
	xorl	%esi, %esi
	movl	$1, %edx
	xorl	%eax, %eax
	call	make_decl_rtl
	xorl	%eax, %eax
	movq	static_aggregates(%rip), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	call	perm_tree_cons
	movq	%rax, static_aggregates(%rip)
	jmp	.L12628
.L12623:
	movq	global_binding_level(%rip), %r9
	movzbl	18(%r12), %edi
	movq	%r14, 88(%r12)
	cmpq	%r9, current_binding_level(%rip)
	movq	%r14, %rsi
	sete	%r8b
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	andb	$-5, %dil
	salb	$2, %r8b
	orb	%r8b, %dil
	movb	%dil, 18(%r12)
	movq	%r12, %rdi
	call	finish_decl
	jmp	.L12628
.L13241:
	movq	32(%rbx), %rbx
	movzbl	16(%rbx), %eax
	jmp	.L12621
.L12620:
	movq	global_binding_level(%rip), %r12
	movq	-80(%rbp), %rdi
	cmpq	%r12, current_binding_level(%rip)
	sete	%cl
	xorl	%eax, %eax
	movzbl	%cl, %esi
	call	get_temp_name
	xorl	%edx, %edx
	movl	$106, %edi
	movq	%rax, %rsi
	movq	%rax, %r12
	xorl	%eax, %eax
	call	build_unary_op
	movq	%rax, %rbx
	jmp	.L12622
.L12618:
	cmpb	$33, 16(%r13)
	je	.L13244
.L12619:
	movq	%r13, %rdi
	movl	$.LC125, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L12583
.L13244:
	movq	error_mark_node(%rip), %r14
	movq	%r14, 72(%r13)
	jmp	.L12619
	.p2align 6,,7
.L13230:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	movl	$101, %edi
	movq	%r15, %rsi
	movq	%r14, %rdx
	call	build_type_conversion
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L12608
	cmpq	error_mark_node(%rip), %rax
	movq	$0, -248(%rbp)
	movl	$.LC124, %r12d
	cmovne	-248(%rbp), %r12
	movq	%rax, %r14
	movq	%r12, -248(%rbp)
	movl	$1, %r12d
	jmp	.L12606
.L12608:
	movq	8(%r15), %rsi
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	movl	$101, %edi
	movq	%r14, %rdx
	call	build_type_conversion
	testq	%rax, %rax
	je	.L12606
	cmpq	error_mark_node(%rip), %rax
	movl	$.LC124, %esi
	movq	%rax, %r14
	cmove	%rsi, %rbx
	xorl	%r12d, %r12d
	movq	%rbx, -248(%rbp)
	jmp	.L12606
.L13229:
	movq	112(%rdx), %rsi
	movq	112(%rcx), %rdi
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	comptypes
	testl	%eax, %eax
	je	.L12594
	movq	8(%r15), %r10
	testb	$16, 17(%r10)
	movq	%r10, %rax
	je	.L12595
	testb	$32, 17(%r14)
	je	.L12595
	movq	$.LC119, -248(%rbp)
	jmp	.L12591
.L12595:
	testb	$32, 17(%rax)
	je	.L12597
	testb	$16, 17(%r14)
	je	.L12597
	movq	$.LC120, -248(%rbp)
	jmp	.L12591
.L12597:
	testb	$48, 17(%rax)
	jne	.L12591
	movzbl	17(%r14), %eax
	testb	$32, %al
	je	.L12600
	movq	$.LC121, -248(%rbp)
	jmp	.L12591
.L12600:
	testb	$16, %al
	je	.L12591
	movq	$.LC122, -248(%rbp)
	jmp	.L12591
.L12594:
	movq	8(%r15), %rdi
	movq	%r14, %rsi
	xorl	%eax, %eax
	call	convert
	movq	%rax, %r14
	jmp	.L12591
.L13228:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	default_conversion
	movq	8(%r15), %rcx
	movq	8(%rax), %rdx
	movq	%rax, %r14
	jmp	.L12592
.L13083:
	movq	8(%r14), %rdx
	jmp	.L12592
.L13227:
	movq	8(%r15), %r11
	movq	8(%rbx), %r10
	xorl	%edx, %edx
	xorl	%eax, %eax
	movq	112(%r11), %rdi
	movq	112(%r10), %rsi
	call	comptypes
	testl	%eax, %eax
	jne	.L12588
	movq	$.LC118, -248(%rbp)
	jmp	.L12591
.L12588:
	movq	8(%r14), %rdi
	movq	8(%r15), %r8
	movq	8(%rdi), %rdx
	movzbl	17(%r8), %esi
	movzbl	17(%rdx), %ecx
	shrb	$5, %sil
	andl	$1, %esi
	shrb	$5, %cl
	andl	$1, %ecx
	cmpl	%ecx, %esi
	jl	.L12591
	xorl	%r12d, %r12d
	movq	%rbx, %r14
	jmp	.L12591
.L13226:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	convert_from_reference
	movq	%rax, %rbx
	jmp	.L12586
.L13225:
	movq	%r14, %rdi
	xorl	%eax, %eax
	call	build_compound_expr
	movq	%rax, %r14
	jmp	.L12584
	.p2align 6,,7
.L13138:
	testq	%r14, %r14
	je	.L12571
	cmpq	error_mark_node(%rip), %r14
	je	.L12571
	movzbl	17(%r14), %edi
	movl	$147, %esi
	xorl	%eax, %eax
	shrb	$2, %dil
	andl	$1, %edi
	call	my_friendly_assert
.L12571:
	cmpq	$0, -224(%rbp)
	je	.L12573
	movq	8(%r13), %r8
	xorl	%eax, %eax
	movq	$0, 120(%r8)
	movq	-224(%rbp), %rdi
	call	get_identifier
	xorl	%edx, %edx
	movq	-224(%rbp), %rsi
	movq	%rax, 112(%r13)
	movq	%r13, %rdi
	xorl	%eax, %eax
	call	make_decl_rtl
	jmp	.L12573
.L13137:
	movq	%r13, %rsi
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	finish_exception_decl
	movq	144(%r15), %rsi
	orb	$32, 5(%rsi)
	jmp	.L12566
	.p2align 6,,7
.L13136:
	testq	%r14, %r14
	je	.L12559
	cmpq	$0, 88(%r13)
	je	.L12559
	movq	8(%r14), %r15
	movq	$0, 88(%r13)
	movq	%r15, 8(%r13)
.L12559:
	testb	$64, 46(%r15)
	je	.L12560
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	je	.L12560
	cmpq	$0, 8(%rbx)
	je	.L12561
	cmpq	%r15, 8(%r13)
	je	.L12561
	movq	32(%rbx), %rsi
	movl	$.LC116, %edi
	xorl	%eax, %eax
	call	warning
	movq	56(%r13), %rbx
.L12561:
	movq	current_binding_level(%rip), %r12
	cmpq	global_binding_level(%rip), %r12
	je	.L13245
.L13110:
	movq	8(%rbx), %rsi
	movq	32(%r12), %rdx
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 32(%r12)
.L12563:
	movq	%r15, 8(%rbx)
	movq	144(%r15), %rcx
	orb	$32, 5(%rcx)
.L12560:
	movq	current_function_decl(%rip), %rdi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	GNU_xref_decl
	movq	%r13, %rdi
	xorl	%edx, %edx
	cmpq	$0, 64(%r13)
	sete	%dl
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	xorl	%esi, %esi
	call	rest_of_decl_compilation
	jmp	.L12566
.L13245:
	movq	class_binding_level(%rip), %r12
	testq	%r12, %r12
	je	.L12563
	jmp	.L13110
	.p2align 6,,7
.L13135:
	testq	%r14, %r14
	je	.L12554
	movl	$.LC115, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L12554
.LFE4:
.Lfe4:
	.size	finish_decl,.Lfe4-finish_decl
	.section	.rodata.str1.32
	.align 32
.LC137:
	.string	"multiple initializations given for `%s'"
	.text
	.align 2
	.p2align 4,,15
	.type	expand_static_init,@function
expand_static_init:
.LFB5:
	subq	$24, %rsp
.LCFI21:
	xorl	%eax, %eax
	movq	%rbp, 8(%rsp)
.LCFI22:
	movq	%rbx, (%rsp)
.LCFI23:
	movq	%r12, 16(%rsp)
.LCFI24:
	movq	%rsi, %rbp
	movq	%rdi, %rbx
	movq	static_aggregates(%rip), %rsi
	call	value_member
	testq	%rax, %rax
	je	.L13247
	cmpq	$0, 24(%rax)
	jne	.L13256
.L13246:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
	.p2align 6,,7
.L13256:
	movq	%rbx, %rdi
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	movq	(%rsp), %rbx
	movl	$.LC137, %esi
	xorl	%eax, %eax
	addq	$24, %rsp
	jmp	error_with_decl
	.p2align 6,,7
.L13247:
	movq	global_binding_level(%rip), %rdx
	cmpq	%rdx, current_binding_level(%rip)
	je	.L13250
	movl	$permanent_obstack, %edi
	movl	$permanent_obstack, %esi
	xorl	%eax, %eax
	call	push_obstacks
	xorl	%eax, %eax
	movl	$1, %esi
	movq	integer_type_node(%rip), %rdi
	call	get_temp_name
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	movq	%rax, %r12
	movq	%rax, %rdi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	rest_of_decl_compilation
	xorl	%eax, %eax
	movq	integer_zero_node(%rip), %rdx
	movl	$95, %edi
	movq	%r12, %rsi
	call	build_binary_op
	xorl	%esi, %esi
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	expand_start_cond
	movq	integer_one_node(%rip), %rsi
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	movq	%r12, %rdi
	call	expand_assignment
	movq	8(%rbx), %rdx
	testb	$16, 46(%rdx)
	je	.L13251
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	expand_aggr_init
	xorl	%eax, %eax
	call	do_pending_stack_adjust
.L13252:
	xorl	%eax, %eax
	call	expand_end_cond
	movq	8(%rbx), %rcx
	testb	$32, 46(%rcx)
	jne	.L13257
.L13253:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	xorl	%eax, %eax
	movq	16(%rsp), %r12
	addq	$24, %rsp
	jmp	pop_obstacks
.L13257:
	movq	static_aggregates(%rip), %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	%rax, static_aggregates(%rip)
	orb	$4, 18(%rax)
	jmp	.L13253
.L13251:
	movq	%rbx, %rdi
	movq	%rbp, %rsi
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	call	expand_assignment
	jmp	.L13252
.L13250:
	movq	8(%rbx), %rsi
	testb	$16, 46(%rsi)
	je	.L13258
.L13255:
	movq	static_aggregates(%rip), %rdx
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	perm_tree_cons
	movq	%rax, static_aggregates(%rip)
	jmp	.L13246
.L13258:
	xorl	%eax, %eax
	call	preserve_initializer
	jmp	.L13255
.LFE5:
.Lfe5:
	.size	expand_static_init,.Lfe5-expand_static_init
	.align 2
	.p2align 4,,15
.globl complete_array_type
	.type	complete_array_type,@function
complete_array_type:
.LFB6:
	pushq	%r13
.LCFI25:
	movl	%edx, %r13d
	pushq	%r12
.LCFI26:
	movq	%rdi, %r12
	pushq	%rbp
.LCFI27:
	xorl	%ebp, %ebp
	pushq	%rbx
.LCFI28:
	xorl	%ebx, %ebx
	subq	$8, %rsp
.LCFI29:
	testq	%rsi, %rsi
	je	.L13260
	movzbl	16(%rsi), %eax
	cmpb	$28, %al
	je	.L13272
	cmpb	$43, %al
	je	.L13273
	cmpq	error_mark_node(%rip), %rsi
	movl	$1, %eax
	movl	$1, %edi
	cmovne	%eax, %ebp
.L13271:
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_int_2_wide
	movq	%rax, %rbx
.L13260:
	testq	%rbx, %rbx
	jne	.L13270
	testl	%r13d, %r13d
	jne	.L13274
.L13267:
	testq	%rbx, %rbx
	movl	$2, %ebp
	je	.L13268
.L13270:
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	build_index_type
	movq	%rax, 24(%r12)
	cmpq	$0, 8(%rbx)
	jne	.L13268
	movq	%rax, 8(%rbx)
.L13268:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	layout_type
	addq	$8, %rsp
	movl	%ebp, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 6,,7
.L13274:
	movl	$1, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	call	build_int_2_wide
	movq	%rax, %rbx
	jmp	.L13267
	.p2align 6,,7
.L13273:
	movq	40(%rsi), %rdi
	xorl	%eax, %eax
	call	list_length
	leal	-1(%rax), %edi
	jmp	.L13271
	.p2align 6,,7
.L13272:
	movl	32(%rsi), %edi
	decl	%edi
	jmp	.L13271
.LFE6:
.Lfe6:
	.size	complete_array_type,.Lfe6-complete_array_type
	.section	.rodata.str1.1
.LC164:
	.string	"type name"
	.section	.rodata.str1.32
	.align 32
.LC176:
	.string	"short, signed or unsigned invalid for `%s'"
	.align 32
.LC177:
	.string	"long, short, signed or unsigned invalid for `%s'"
	.align 32
.LC200:
	.string	"storage class specified for parameter `%s'"
	.align 32
.LC201:
	.string	"storage class specified for typename"
	.align 32
.LC243:
	.string	"invalid type qualifier for non-method type"
	.align 32
.LC287:
	.string	"cannot explicitly declare member `%s' to have extern linkage"
	.align 32
.LC286:
	.string	"cannot declare member `%s' to have static linkage"
	.align 32
.LC283:
	.string	"`%s' is not a static member of class `%s'"
	.align 32
.LC284:
	.string	"redeclaration of type for `%s'"
	.align 32
.LC285:
	.string	"multiple initializations of static member `%s::%s'"
	.section	.rodata.str1.1
.LC251:
	.string	"invalid raises declaration"
.LC250:
	.string	"invalid friend declaration"
	.section	.rodata.str1.32
	.align 32
.LC267:
	.string	"`const' and `volatile' function specifiers invalid in field declaration"
	.section	.rodata.str1.1
.LC282:
	.string	"variable declared `inline'"
.LC281:
	.string	"variable declared `virtual'"
.LC274:
	.string	"main"
	.section	.rodata.str1.32
	.align 32
.LC279:
	.string	"inline declaration ignored for function with `...'"
	.align 32
.LC280:
	.string	"ANSI C++ does not permit `extern inline'"
	.section	.rodata.str1.1
.LC278:
	.string	"cannot inline function `main'"
	.section	.rodata.str1.32
	.align 32
.LC277:
	.string	"cannot declare function `main' to have static linkage"
	.align 32
.LC276:
	.string	"cannot declare member function `%s' to have static linkage"
	.align 32
.LC260:
	.string	"method `%s' may not be declared static"
	.align 32
.LC261:
	.string	"(since `%s' declared virtual in base class.)"
	.align 32
.LC259:
	.string	"inconsistent declarations for `%s'"
	.align 32
.LC257:
	.string	"global operator delete must be declared as taking a single argument of type void*"
	.align 32
.LC258:
	.string	"operator delete cannot be overloaded"
	.align 32
.LC256:
	.string	"functions cannot have method qualifiers"
	.section	.rodata.str1.1
.LC275:
	.string	"builtin_"
	.section	.rodata.str1.32
	.align 32
.LC273:
	.string	"virtual non-class function `%s'"
	.align 32
.LC272:
	.string	"invalid storage class for function `%s'"
	.align 32
.LC262:
	.string	"field `%s' has incomplete type"
	.align 32
.LC265:
	.string	"member functions are implicitly friends of their class"
	.section	.rodata.str1.1
.LC270:
	.string	"const member"
	.section	.rodata.str1.32
	.align 32
.LC269:
	.string	"ANSI C++ forbids initialization of %s `%s'"
	.section	.rodata.str1.1
.LC271:
	.string	"member"
	.section	.rodata.str1.32
	.align 32
.LC268:
	.string	"static member `%s' must be defined separately from its declaration"
	.section	.rodata.str1.1
.LC266:
	.string	"field declared `virtual'"
.LC263:
	.string	"field has incomplete type"
	.section	.rodata.str1.32
	.align 32
.LC264:
	.string	"`%s' is neither function nor method; cannot be declared friend"
	.section	.rodata.str1.1
.LC253:
	.string	"operator"
	.section	.rodata.str1.32
	.align 32
.LC252:
	.string	"can't make %s `%s' into a method -- not in a class"
	.section	.rodata.str1.1
.LC254:
	.string	"function"
	.section	.rodata.str1.32
	.align 32
.LC255:
	.string	"function `%s' declared virtual inside a union"
	.align 32
.LC249:
	.string	"`const' and `volatile' function specifiers invalid in parameter declaration"
	.section	.rodata.str1.1
.LC248:
	.string	"parameter declared `virtual'"
	.section	.rodata.str1.32
	.align 32
.LC247:
	.string	"cannot use `::' in parameter declaration"
	.align 32
.LC246:
	.string	"variable or field declared void"
	.align 32
.LC245:
	.string	"variable or field `%s' declared void"
	.align 32
.LC244:
	.string	"trying to make class `%s' a friend of global scope"
	.section	.rodata.str1.1
.LC184:
	.string	"duplicate `volatile'"
.LC183:
	.string	"duplicate `const'"
.LC233:
	.string	"reference"
.LC234:
	.string	"pointer"
	.section	.rodata.str1.32
	.align 32
.LC232:
	.string	"invalid type modifier within %s declarator"
	.section	.rodata.str1.1
.LC231:
	.string	"invalid type: `void &'"
	.section	.rodata.str1.32
	.align 32
.LC230:
	.string	"cannot declare references to functions; use pointer to function instead"
	.section	.rodata.str1.1
.LC229:
	.string	"pointers"
.LC228:
	.string	"references"
	.section	.rodata.str1.32
	.align 32
.LC227:
	.string	"cannot declare %s to references"
	.align 32
.LC175:
	.string	"Tried to globalize already-global type "
	.align 32
.LC226:
	.string	"destructors cannot be specified with parameters"
	.align 32
.LC225:
	.string	"friend declaration not in class definition"
	.align 32
.LC224:
	.string	"virtual functions cannot be friends"
	.align 32
.LC223:
	.string	"constructor for alien class `%s' cannot be member"
	.align 32
.LC222:
	.string	"return value type specifier for constructor ignored"
	.align 32
.LC221:
	.string	"constructors cannot be declared `volatile'"
	.align 32
.LC220:
	.string	"constructors cannot be declared `const'"
	.align 32
.LC219:
	.string	"constructors cannot be declared virtual"
	.align 32
.LC218:
	.string	"constructor cannot be static member function"
	.align 32
.LC217:
	.string	"destructor for alien class `%s' cannot be a member"
	.align 32
.LC216:
	.string	"destructors cannot be declared `volatile'"
	.align 32
.LC215:
	.string	"destructors cannot be declared `const'"
	.align 32
.LC214:
	.string	"destructor cannot be static member function"
	.align 32
.LC213:
	.string	"`%s' declared as function returning an array"
	.align 32
.LC212:
	.string	"`%s' declared as function returning a function"
	.align 32
.LC241:
	.string	"type conversion function declared to return incongruent type"
	.align 32
.LC242:
	.string	"return type specified for type conversion function"
	.align 32
.LC236:
	.string	"extra qualification `%s' on member `%s' ignored"
	.align 32
.LC235:
	.string	"cannot declare member `%s::%s' within this class"
	.align 32
.LC239:
	.string	"field `%s' is not a member of structure `%s'"
	.align 32
.LC238:
	.string	"type conversion is not a member of structure `%s'"
	.align 32
.LC237:
	.string	"class `%s' does not have any constructors"
	.align 32
.LC240:
	.string	"structure `%s' not yet defined"
	.align 32
.LC208:
	.string	"size of array `%s' has non-integer type"
	.align 32
.LC210:
	.string	"size of array `%s' is negative"
	.align 32
.LC211:
	.string	"ANSI C++ forbids variable-size array `%s'"
	.align 32
.LC209:
	.string	"ANSI C++ forbids zero-size array `%s'"
	.align 32
.LC207:
	.string	"declaration of `%s' as array of functions"
	.align 32
.LC206:
	.string	"declaration of `%s' as array of voids"
	.align 32
.LC143:
	.string	"variable `%s' has initializer but incomplete type"
	.align 32
.LC196:
	.string	"typedef of `%s' in class scope hides previous declaration"
	.align 32
.LC195:
	.string	"label `%s' defined but not used"
	.align 32
.LC190:
	.string	"label `%s' used but not defined"
	.section	.rodata.str1.1
.LC192:
	.string	"_$tmp_"
.LC193:
	.string	"invalid jump to label `%s'"
	.section	.rodata.str1.32
	.align 32
.LC194:
	.string	"crosses initialization of `%s'"
	.section	.rodata.str1.1
.LC191:
	.string	"duplicate label `%s'"
	.section	.rodata.str1.32
	.align 32
.LC153:
	.string	"non-function templates not yet supported"
	.section	.rodata.str1.1
.LC151:
	.string	"non-function declaration `%s'"
	.section	.rodata.str1.32
	.align 32
.LC152:
	.string	"conflicts with function declaration `%s'"
	.align 32
.LC149:
	.string	"conflicting language contexts for declaration of `%s';"
	.align 32
.LC150:
	.string	"conflicts with previous declaration here"
	.align 32
.LC147:
	.string	"C-language function `%s' overloaded here"
	.align 32
.LC148:
	.string	"Previous C-language version of this function was `%s'"
	.align 32
.LC2:
	.string	"`%s' redeclared as non-member function"
	.align 32
.LC1:
	.string	"`%s' redeclared as member function"
	.align 32
.LC146:
	.string	"declaration of `%s' has `extern' and is initialized"
	.align 32
.LC145:
	.string	"aggregate `%s' has incomplete type and cannot be initialized"
	.align 32
.LC144:
	.string	"elements of array `%s' have incomplete type"
	.section	.rodata.str1.1
.LC141:
	.string	"typedef `%s' is initialized"
	.section	.rodata.str1.32
	.align 32
.LC142:
	.string	"function `%s' is initialized like a variable"
	.section	.rodata.str1.1
.LC140:
	.string	"extern"
.LC199:
	.string	"structure field"
.LC198:
	.string	"member operator"
	.section	.rodata.str1.32
	.align 32
.LC197:
	.string	"storage class specified for %s `%s'"
	.align 32
.LC203:
	.string	"`%s' has both `extern' and initializer"
	.align 32
.LC202:
	.string	"`%s' initialized and declared `extern'"
	.align 32
.LC204:
	.string	"nested function `%s' declared `extern'"
	.align 32
.LC205:
	.string	"top-level declaration of `%s' specifies `auto'"
	.align 32
.LC189:
	.string	"multiple storage classes in declaration of `%s'"
	.align 32
.LC188:
	.string	"virtual outside class declaration"
	.align 32
.LC187:
	.string	"typedef declaration invalid in parameter declaration"
	.align 32
.LC186:
	.string	"storage class specifiers invalid in parameter declarations"
	.align 32
.LC185:
	.string	"member `%s' cannot be declared both virtual and static"
	.align 32
.LC178:
	.string	"long and short specified together for `%s'"
	.align 32
.LC180:
	.string	"long or short specified with floating type for `%s'"
	.align 32
.LC182:
	.string	"long, short, signed or unsigned used invalidly for `%s'"
	.align 32
.LC181:
	.string	"signed and unsigned given together for `%s'"
	.align 32
.LC179:
	.string	"long or short specified with char for `%s'"
	.align 32
.LC174:
	.string	"return type specification for constructor invalid"
	.align 32
.LC173:
	.string	"return type specification for destructor invalid"
	.align 32
.LC172:
	.string	"`%s' fails to be a typedef or built in type"
	.align 32
.LC171:
	.string	"two or more data types in declaration of `%s'"
	.align 32
.LC168:
	.string	"multiple declarations `%s' and `%s'"
	.section	.rodata.str1.1
.LC169:
	.string	"duplicate `%s'"
	.section	.rodata.str1.32
	.align 32
.LC170:
	.string	"`long long long' is too long for GCC"
	.align 32
.LC167:
	.string	"extraneous `__wchar_t' ignored"
	.section	.rodata.str1.1
.LC166:
	.string	"extraneous `char' ignored"
.LC165:
	.string	"extraneous `int' ignored"
.LC158:
	.string	"declarator name missing"
.LC159:
	.string	"<nameless>"
	.section	.rodata.str1.32
	.align 32
.LC156:
	.string	"destructors must be member functions"
	.align 32
.LC157:
	.string	"destructor `%s' must match class name `%s'"
	.align 32
.LC163:
	.string	"type `%s' is not derived from type `%s'"
	.align 32
.LC162:
	.string	"multiple `::' terms in declarator invalid"
	.section	.rodata.str1.1
.LC161:
	.string	"operator <typename>"
	.section	.rodata.str1.32
	.align 32
.LC160:
	.string	"operator `%s' must be declared as a member"
	.align 32
.LC154:
	.string	"bad parameter list specification for function `%s'"
	.align 32
.LC155:
	.string	"bad parameter list specification for function"
	.align 32
.LC139:
	.string	"arrays cannot take initializers"
	.align 32
.LC138:
	.string	"initializer lists for field declarations"
	.text
	.align 2
	.p2align 4,,15
.globl grokdeclarator
	.type	grokdeclarator,@function
grokdeclarator:
.LFB7:
	pushq	%rbp
.LCFI30:
	movq	%rsp, %rbp
.LCFI31:
	pushq	%r15
.LCFI32:
	xorl	%r15d, %r15d
	pushq	%r14
.LCFI33:
	pushq	%r13
.LCFI34:
	pushq	%r12
.LCFI35:
	pushq	%rbx
.LCFI36:
	subq	$1032, %rsp
.LCFI37:
	movq	$0, -1032(%rbp)
	movl	$0, -444(%rbp)
	movl	%edx, -484(%rbp)
	movq	%rdi, -904(%rbp)
	movq	%rsi, -592(%rbp)
	decl	%edx
	movl	%ecx, -604(%rbp)
	movq	%r8, -1048(%rbp)
	movl	$0, -580(%rbp)
	movq	$0, -1072(%rbp)
	movl	$0, -576(%rbp)
	movl	$0, -572(%rbp)
	movl	$0, -500(%rbp)
	movq	$0, -496(%rbp)
	movl	$0, -376(%rbp)
	movl	$0, -472(%rbp)
	movl	$0, -372(%rbp)
	movq	$0, -440(%rbp)
	movl	$0, -468(%rbp)
	movq	$0, -464(%rbp)
	movq	current_class_type(%rip), %r14
	movq	$0, -456(%rbp)
	movl	$0, -448(%rbp)
	je	.L17509
	cmpl	$6, -484(%rbp)
	je	.L17510
	cmpl	$4, -484(%rbp)
	je	.L17511
.L13277:
	movl	flag_traditional(%rip), %eax
	testl	%eax, %eax
	jne	.L17512
.L13281:
	movq	-904(%rbp), %rbx
	xorl	%r12d, %r12d
	movq	$0, -480(%rbp)
	testq	%rbx, %rbx
	je	.L13287
	cmpb	$42, 16(%rbx)
	je	.L13286
.L13283:
	testq	%rbx, %rbx
	je	.L13287
	cmpq	$0, -592(%rbp)
	je	.L13287
	cmpb	$50, 16(%rbx)
	je	.L17513
.L13314:
	.p2align 4,,7
.L13287:
	movq	-904(%rbp), %rbx
.L13764:
	testq	%rbx, %rbx
	je	.L16962
	.p2align 4,,7
.L13817:
	movzbl	16(%rbx), %r11d
	movzbl	%r11b, %eax
	cmpl	$126, %eax
	ja	.L13815
	mov	%eax, %edi
	jmp	*.L13816(,%rdi,8)
	.section	.rodata
	.align 8
	.align 4
.L13816:
	.quad	.L13814
	.quad	.L13784
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13787
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13787
	.quad	.L13787
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13780
	.quad	.L13815
	.quad	.L13815
	.quad	.L13780
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13769
	.quad	.L13815
	.quad	.L13781
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13770
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13780
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13815
	.quad	.L13793
	.quad	.L13815
	.quad	.L13791
	.text
.L13769:
	xorl	%r14d, %r14d
.L17337:
	movq	32(%rbx), %rbx
	.p2align 4,,7
.L13765:
	testq	%rbx, %rbx
	jne	.L13817
.L16962:
	cmpq	$0, -480(%rbp)
	movl	$.LC164, %ebx
	movl	-376(%rbp), %eax
	cmovne	-480(%rbp), %rbx
	testl	%eax, %eax
	movq	%rbx, -480(%rbp)
	je	.L13819
	cmpl	$50, -472(%rbp)
	je	.L13819
.L17287:
	movq	$0, -1072(%rbp)
.L13275:
	movq	-1072(%rbp), %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	leave
	ret
	.p2align 6,,7
.L13819:
	movl	-484(%rbp), %eax
	testl	%eax, %eax
	jne	.L13820
	movq	current_binding_level(%rip), %r9
	movq	global_binding_level(%rip), %r14
	movl	$2, %eax
	cmpq	%r14, 56(%r9)
	cmovne	-484(%rbp), %eax
	movl	%eax, -484(%rbp)
.L13820:
	movq	-592(%rbp), %r13
	testq	%r13, %r13
	je	.L16964
	.p2align 4,,7
.L13879:
	cmpb	$3, 16(%r13)
	movq	32(%r13), %rbx
	jne	.L17287
	movzbl	16(%rbx), %edx
	cmpb	$1, %dl
	je	.L17514
.L13826:
	cmpq	$0, -1072(%rbp)
	jne	.L17515
	cmpb	$1, %dl
	je	.L17516
	testb	%dl, %dl
	cmove	-1072(%rbp), %rbx
	movq	%rbx, -1072(%rbp)
.L13830:
.L13823:
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.L13879
.L16964:
	movq	-1072(%rbp), %r13
	testq	%r13, %r13
	movq	%r13, -512(%rbp)
	jne	.L13880
	cmpl	$2, -468(%rbp)
	movl	$-1, -572(%rbp)
	je	.L17517
	cmpl	$1, -468(%rbp)
	je	.L17518
	movl	-376(%rbp), %eax
	testl	%eax, %eax
	je	.L13885
	movl	explicit_warn_return_type(%rip), %r10d
	testl	%r10d, %r10d
	je	.L13885
	movl	-468(%rbp), %edi
	testl	%edi, %edi
	jne	.L13885
	testl	$1077248, %r15d
	jne	.L13885
	movl	$1, warn_about_return_type(%rip)
.L13885:
	movq	integer_type_node(%rip), %r11
	movq	%r11, -1072(%rbp)
.L13886:
	testl	$16384, %r15d
	movq	$0, -1000(%rbp)
	je	.L14231
	movq	-1072(%rbp), %rdx
	movq	double_type_node(%rip), %r14
	cmpq	%r14, 112(%rdx)
	je	.L17519
.L14231:
	testl	$1077248, %r15d
	je	.L14232
	movq	-1072(%rbp), %r13
	xorl	%ebx, %ebx
	movl	$.LC176, %edi
	movzbl	16(%r13), %edx
	cmpb	$8, %dl
	movl	%edx, %ecx
	je	.L17352
	cmpb	$7, %dl
	je	.L17520
.L14236:
	movl	$.LC177, %edi
.L17352:
	movq	-480(%rbp), %rsi
	xorl	%eax, %eax
	call	error
.L14234:
	testl	%ebx, %ebx
	jne	.L14232
	andl	$-1077249, %r15d
	movl	$0, -576(%rbp)
.L14232:
	testl	$4096, %r15d
	jne	.L14250
	movl	-372(%rbp), %ebx
	testl	%ebx, %ebx
	je	.L14249
	movl	flag_traditional(%rip), %edx
	testl	%edx, %edx
	jne	.L14250
	movl	flag_signed_bitfields(%rip), %r9d
	testl	%r9d, %r9d
	jne	.L14249
	movl	-572(%rbp), %r14d
	testl	%r14d, %r14d
	jne	.L14252
	movl	-500(%rbp), %ecx
	testl	%ecx, %ecx
	jne	.L14252
	cmpq	$0, -496(%rbp)
	je	.L14252
	movq	-496(%rbp), %r10
	testb	$4, 54(%r10)
	jne	.L14249
.L14252:
	movq	-1072(%rbp), %r13
	cmpb	$10, 16(%r13)
	je	.L14249
	testl	$1048576, %r15d
	jne	.L14249
.L14250:
	movl	-576(%rbp), %edi
	testl	%edi, %edi
	je	.L14253
	movq	long_long_unsigned_type_node(%rip), %rcx
.L17353:
	movq	%rcx, -1072(%rbp)
.L14263:
	movq	-1072(%rbp), %rcx
	movl	%r15d, %r8d
	movzbl	17(%rcx), %r9d
	movl	$0, -892(%rbp)
	movl	%r9d, %edx
	shrb	$5, %dl
	andl	$1, %edx
	testl	$2097152, %r15d
	leal	1(%rdx), %r14d
	cmovne	%r14d, %edx
	shrb	$4, %r9b
	andl	$1, %r9d
	movl	%edx, -1060(%rbp)
	testl	$4194304, %r15d
	leal	1(%r9), %ebx
	cmovne	%ebx, %r9d
	shrl	$23, %r8d
	andl	$1, %r8d
	movl	%r9d, -1056(%rbp)
	cmpl	$1, -1060(%rbp)
	movl	%r8d, -212(%rbp)
	jg	.L17521
.L14275:
	cmpl	$1, -1056(%rbp)
	jg	.L17522
.L14276:
	movl	%r15d, %r10d
	andl	$67108864, %r10d
	testl	$65536, %r15d
	movl	%r10d, -1052(%rbp)
	je	.L14277
	xorl	%r13d, %r13d
	cmpl	$3, -484(%rbp)
	sete	%r13b
	incl	%r13d
	movl	%r13d, -892(%rbp)
.L14277:
	movl	-1052(%rbp), %eax
	testl	%eax, %eax
	je	.L14280
	cmpl	$2, -892(%rbp)
	je	.L17523
.L14280:
	movl	%r15d, %esi
	movl	%r15d, %edi
	andl	$33554432, %esi
	andl	$-100663297, %edi
	movl	%esi, -1004(%rbp)
	movl	%edi, -1064(%rbp)
	je	.L14281
	testl	$65536, %r15d
	movl	$1, %eax
	cmove	-580(%rbp), %eax
	movl	%eax, -580(%rbp)
	incl	%eax
	andl	$131072, %r15d
	cmove	-580(%rbp), %eax
	cmpl	$2, -484(%rbp)
	movl	%eax, -580(%rbp)
	je	.L17524
.L14284:
	testl	$524288, -1064(%rbp)
	je	.L14285
	cmpl	$2, -484(%rbp)
	je	.L17525
.L14286:
	incl	-580(%rbp)
.L14285:
	movl	-580(%rbp), %eax
	incl	%eax
	cmpw	$0, -1064(%rbp)
	cmovns	-580(%rbp), %eax
	movl	%eax, -580(%rbp)
	incl	%eax
	testl	$262144, -1064(%rbp)
	cmove	-580(%rbp), %eax
	movl	%eax, -580(%rbp)
.L14281:
	movl	-1052(%rbp), %eax
	testl	%eax, %eax
	je	.L14289
	cmpq	$0, current_class_name(%rip)
	je	.L17526
.L14289:
	cmpl	$1, -580(%rbp)
	jg	.L17527
	movl	-484(%rbp), %eax
	testl	%eax, %eax
	je	.L14292
	movl	-580(%rbp), %eax
	testl	%eax, %eax
	jle	.L14292
	cmpl	$2, -484(%rbp)
	je	.L14291
	cmpl	$3, -484(%rbp)
	je	.L17528
	cmpl	$2, -484(%rbp)
	movq	-480(%rbp), %rsi
	movl	$.LC200, %edi
	movl	$.LC201, %r9d
	cmovne	%r9, %rdi
	xorl	%eax, %eax
	call	error
.L14916:
	andl	$-425985, -1064(%rbp)
.L14291:
	cmpq	$0, -904(%rbp)
	je	.L14931
	movq	-904(%rbp), %rdi
	movzbl	16(%rdi), %eax
	cmpb	$1, %al
	je	.L14931
	.p2align 4,,7
.L16483:
	movq	-1072(%rbp), %r14
	movzbl	16(%r14), %edx
	testb	%dl, %dl
	jne	.L14934
	cmpb	$124, %al
	je	.L17529
	movq	-904(%rbp), %r15
	movq	32(%r15), %r13
	movq	%r13, -904(%rbp)
.L14930:
	cmpq	$0, -904(%rbp)
	je	.L14931
	movq	-904(%rbp), %r14
	movzbl	16(%r14), %eax
	cmpb	$1, %al
	jne	.L16483
.L14931:
	testl	$524288, -1064(%rbp)
	je	.L16484
	movl	-1060(%rbp), %eax
	testl	%eax, %eax
	jne	.L16486
	movl	-1056(%rbp), %eax
	testl	%eax, %eax
	je	.L17069
.L16486:
	movq	-1072(%rbp), %rdi
	movl	-1060(%rbp), %esi
	xorl	%eax, %eax
	movl	-1056(%rbp), %edx
	call	build_type_variant
	movq	%rax, %rbx
.L16485:
	movq	80(%rbx), %rdx
	testq	%rdx, %rdx
	je	.L16487
	cmpb	$32, 16(%rdx)
	je	.L17530
.L16487:
	movq	-904(%rbp), %rsi
	xorl	%eax, %eax
	movl	$32, %edi
	movq	%rbx, %rdx
	call	build_decl
	cmpq	$0, -1032(%rbp)
	movq	%rax, -1072(%rbp)
	je	.L16501
	cmpq	$0, -1000(%rbp)
	jne	.L17126
	cmpb	$16, 16(%rbx)
	je	.L16503
	movq	%rax, %rdi
	movl	$.LC243, %esi
	xorl	%eax, %eax
	call	error_with_decl
.L16502:
	cmpq	$0, -1000(%rbp)
	je	.L16501
.L17126:
	movq	-1000(%rbp), %rdi
	movq	-1072(%rbp), %rsi
	xorl	%eax, %eax
	movq	-1032(%rbp), %rdx
	call	grok_method_quals
.L16501:
	testl	$1048576, -1064(%rbp)
	jne	.L16507
	cmpq	$0, -496(%rbp)
	je	.L13275
	movq	-496(%rbp), %r8
	testb	$4, 54(%r8)
	je	.L13275
.L16507:
	movq	-1072(%rbp), %r10
	orb	$4, 54(%r10)
	jmp	.L13275
.L16503:
	movq	96(%rbx), %rdx
	movq	%rdx, -1000(%rbp)
	jmp	.L16502
.L17530:
	movq	56(%rdx), %rcx
	movq	32(%rcx), %rax
	cmpb	$36, (%rax)
	jne	.L16487
	cmpb	$95, 1(%rax)
	jne	.L16487
	movq	current_binding_level(%rip), %rcx
	testq	%rcx, %rcx
	je	.L16498
.L16500:
	movq	8(%rcx), %rax
	testq	%rax, %rax
	je	.L17065
.L16499:
	cmpq	%rbx, 32(%rax)
	je	.L17531
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L16499
.L17065:
	movq	56(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.L16500
.L16498:
	movq	-904(%rbp), %rax
	movq	%rax, 56(%rdx)
	jmp	.L16487
.L17531:
	cmpq	$0, -904(%rbp)
	je	.L16498
	movq	-904(%rbp), %rdi
	movq	%rdi, 24(%rax)
	movq	80(%rbx), %rdx
	jmp	.L16498
	.p2align 6,,7
.L17069:
	movq	-1072(%rbp), %rbx
	jmp	.L16485
.L16484:
	movq	-512(%rbp), %r13
	cmpq	%r13, -1072(%rbp)
	je	.L17532
.L16508:
	cmpl	$5, -484(%rbp)
	je	.L17533
	movq	-1072(%rbp), %rax
	movq	void_type_node(%rip), %rdi
	cmpq	%rdi, 112(%rax)
	je	.L17534
.L16518:
	cmpl	$2, -484(%rbp)
	je	.L17127
	cmpl	$3, -484(%rbp)
	je	.L17535
	movq	-1072(%rbp), %r10
	movzbl	16(%r10), %eax
	cmpb	$23, %al
	je	.L16794
	cmpb	$16, %al
	je	.L16794
	movl	-1052(%rbp), %r12d
	testl	%r12d, %r12d
	jne	.L17536
.L16920:
	movl	-212(%rbp), %ecx
	testl	%ecx, %ecx
	jne	.L17537
.L16921:
	cmpq	$0, -1032(%rbp)
	jne	.L17538
.L16922:
	movl	-1004(%rbp), %edi
	testl	%edi, %edi
	jne	.L17539
.L16923:
	cmpq	$0, -1048(%rbp)
	jne	.L17540
.L16924:
	movq	-1072(%rbp), %rdx
	cmpb	$14, 16(%rdx)
	je	.L17541
	movq	-904(%rbp), %rsi
	movq	-1072(%rbp), %rdx
	movl	$33, %edi
	xorl	%eax, %eax
	call	build_decl
	movq	%rax, %r12
.L16938:
	testl	$131072, -1064(%rbp)
	je	.L16939
	orb	$8, 54(%r12)
	movzbl	53(%r12), %r14d
	movl	-604(%rbp), %ebx
	testl	%ebx, %ebx
	sete	%r8b
	andb	$-2, %r14b
	orb	%r8b, %r14b
	movb	%r14b, 53(%r12)
.L16939:
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.L16940
	testb	$64, 46(%rax)
	je	.L16940
	orb	$12, 18(%r12)
	movzbl	53(%r12), %r15d
	movl	-604(%rbp), %eax
	testl	%eax, %eax
	sete	%sil
	andb	$-2, %r15b
	orb	%sil, %r15b
	movb	%r15b, 53(%r12)
.L16941:
	cmpq	$0, -1000(%rbp)
	je	.L16543
	cmpl	$1, -892(%rbp)
	je	.L17542
.L16946:
	testl	$131072, -1064(%rbp)
	jne	.L17543
.L16543:
	testl	$262144, -1064(%rbp)
	je	.L16948
	orb	$4, 53(%r12)
.L16948:
	movl	-1060(%rbp), %eax
	testl	%eax, %eax
	je	.L16949
	movq	-1072(%rbp), %rax
	movzbl	17(%r12), %r15d
	cmpb	$15, 16(%rax)
	setne	%sil
	andb	$-33, %r15b
	salb	$5, %sil
	orb	%sil, %r15b
	movb	%r15b, 17(%r12)
.L16949:
	movl	-1056(%rbp), %eax
	testl	%eax, %eax
	je	.L16950
	orb	$17, 17(%r12)
.L16950:
	movq	%r12, -1072(%rbp)
	jmp	.L13275
.L17543:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC287, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	error
	andl	$-131073, -1064(%rbp)
	jmp	.L16543
.L17542:
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	lang_printable_name
	movl	$.LC286, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	error
	andl	$-65537, -1064(%rbp)
	jmp	.L16946
	.p2align 6,,7
.L16940:
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L17544
	movl	-1064(%rbp), %eax
	movzbl	18(%r12), %ecx
	shrl	$16, %eax
	movl	%eax, %edx
	andb	$-13, %cl
	movzbl	53(%r12), %eax
	andb	$1, %dl
	salb	$2, %dl
	andb	$1, %al
	salb	$3, %al
.L17444:
	orb	%dl, %cl
	orb	%al, %cl
	movb	%cl, 18(%r12)
	jmp	.L16941
.L17544:
	movl	-1064(%rbp), %eax
	movzbl	18(%r12), %ecx
	shrl	$16, %eax
	xorl	$1, %eax
	andb	$-13, %cl
	movl	%eax, %edx
	andb	$1, %dl
	salb	$3, %dl
	testb	$1, 53(%r12)
	sete	%al
	salb	$2, %al
	jmp	.L17444
.L17541:
	movq	96(%rdx), %rdi
	movq	-904(%rbp), %rsi
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	xorl	%eax, %eax
	call	lookup_field
	testq	%rax, %rax
	movq	%rax, %r12
	je	.L16927
	cmpb	$33, 16(%rax)
	je	.L16926
.L16927:
	movq	-1072(%rbp), %rax
	movq	-904(%rbp), %r15
	movl	$.LC283, %edi
	movq	96(%rax), %r14
	xorl	%eax, %eax
	movq	80(%r14), %rsi
	movq	56(%rsi), %rbx
	movq	32(%r15), %rsi
	movq	32(%rbx), %rdx
	call	error
	xorl	%eax, %eax
	movq	-904(%rbp), %rsi
	movq	-1072(%rbp), %r8
	movl	$33, %edi
	movq	8(%r8), %rdx
	call	build_lang_field_decl
	movq	152(%rax), %r11
	movq	%rax, %r12
	movq	%r14, 64(%rax)
	movq	%r14, 16(%r11)
	jmp	.L16938
.L16926:
	movq	8(%rax), %rax
	movq	-1072(%rbp), %r13
	cmpq	$0, 32(%rax)
	movq	8(%r13), %rdx
	jne	.L16929
	movzbl	16(%rax), %ecx
	cmpb	16(%rdx), %cl
	je	.L17545
.L16931:
	movq	-904(%rbp), %r9
	movl	$.LC284, %edi
	movq	32(%r9), %rsi
.L17443:
	xorl	%eax, %eax
	call	error
.L16934:
	movl	-604(%rbp), %edi
	testl	%edi, %edi
	je	.L16938
	movq	88(%r12), %rax
	testq	%rax, %rax
	je	.L16938
	cmpb	$43, 16(%rax)
	je	.L17546
.L16937:
	movq	56(%r12), %r11
	movq	64(%r12), %rdi
	movl	$.LC285, %esi
	xorl	%eax, %eax
	movq	32(%r11), %rdx
	call	error_with_aggr_type
	jmp	.L16938
.L17546:
	cmpq	$0, 40(%rax)
	je	.L16938
	jmp	.L16937
.L17545:
	cmpb	$18, %cl
	je	.L17547
.L16930:
	cmpq	$0, 32(%rdx)
	je	.L16934
	movq	-1072(%rbp), %rcx
	movq	%rcx, 8(%r12)
	jmp	.L16934
.L17547:
	movq	8(%rdx), %r10
	cmpq	%r10, 8(%rax)
	jne	.L16931
	jmp	.L16930
	.p2align 6,,7
.L16929:
	cmpq	%rdx, %rax
	je	.L16934
	movq	-904(%rbp), %rdx
	movl	$.LC284, %edi
	movq	32(%rdx), %rsi
	jmp	.L17443
	.p2align 6,,7
.L17540:
	movl	$.LC251, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16924
.L17539:
	movl	$.LC250, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16923
.L17538:
	movl	$.LC267, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16922
.L17537:
	movl	$.LC282, %edi
	xorl	%eax, %eax
	call	warning
	jmp	.L16921
.L17536:
	movl	$.LC281, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16920
.L16794:
	movq	-904(%rbp), %rcx
	movl	$0, -1008(%rbp)
	testq	%rcx, %rcx
	movq	%rcx, -360(%rbp)
	je	.L17287
	testl	$294912, -1064(%rbp)
	jne	.L17548
.L16796:
	movq	global_binding_level(%rip), %rax
	cmpq	%rax, current_binding_level(%rip)
	je	.L16797
	testl	$8454144, -1064(%rbp)
	je	.L16797
	movl	pedantic(%rip), %r12d
	testl	%r12d, %r12d
	jne	.L17549
.L16797:
	cmpq	$0, -1000(%rbp)
	jne	.L16798
	movl	-1052(%rbp), %eax
	testl	%eax, %eax
	jne	.L17550
.L16799:
	movq	lang_name_cplusplus(%rip), %rdx
	cmpq	%rdx, current_lang_name(%rip)
	je	.L17551
.L16803:
	movl	processing_template_decl(%rip), %eax
	movq	-1072(%rbp), %r15
	testl	%eax, %eax
	jne	.L16805
	movl	-1004(%rbp), %eax
	testl	%eax, %eax
	setne	%r13b
	movzbl	%r13b, %r14d
	incl	%r14d
.L16806:
	cmpq	$0, -1000(%rbp)
	movl	$0, -1012(%rbp)
	je	.L16810
	movq	-1072(%rbp), %rsi
	movl	$1, %eax
	movq	-1000(%rbp), %r8
	cmpb	$23, 16(%rsi)
	cmovne	-1012(%rbp), %eax
	movl	%eax, -1012(%rbp)
	movq	80(%r8), %rsi
	cmpb	$32, 16(%rsi)
	je	.L17552
.L16811:
	movq	%rsi, -1024(%rbp)
.L16813:
	cmpq	$0, -1048(%rbp)
	jne	.L17553
.L16814:
	movq	-360(%rbp), %rsi
	xorl	%eax, %eax
	movl	$29, %edi
	movq	%r15, %rdx
	call	build_lang_decl
	testb	$16, 17(%r15)
	movq	%rax, %r12
	je	.L16815
	orb	$16, 17(%rax)
.L16815:
	movl	-1012(%rbp), %eax
	testl	%eax, %eax
	je	.L16816
	movq	152(%r12), %rcx
	orb	$32, 1(%rcx)
	movq	-1000(%rbp), %rax
	movq	152(%r12), %rdx
	movq	%rax, 64(%r12)
	movq	%rax, 16(%rdx)
.L16816:
	orb	$1, 53(%r12)
	cmpq	$0, -1032(%rbp)
	je	.L16817
	cmpb	$23, 16(%r15)
	je	.L17554
.L16817:
	movq	-360(%rbp), %r9
	cmpq	ansi_opname+984(%rip), %r9
	je	.L17555
.L17314:
	movl	-448(%rbp), %eax
	testl	%eax, %eax
	jne	.L16828
	cmpq	$0, -1000(%rbp)
	jne	.L17556
.L16854:
	movq	56(%r12), %rsi
	testb	$1, 19(%rsi)
	jne	.L17557
.L16855:
	cmpq	$0, -1000(%rbp)
	je	.L17313
	testl	%r14d, %r14d
	jne	.L17558
.L16856:
	testl	%r14d, %r14d
	jne	.L16827
	movq	global_binding_level(%rip), %r10
	movq	112(%r12), %r13
	cmpq	%r10, current_binding_level(%rip)
	je	.L16859
	movq	48(%r13), %rdx
	testq	%rdx, %rdx
	movq	%rdx, %rbx
	jne	.L16860
.L16859:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L16861
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	jne	.L17138
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L16862
	movq	144(%rdi), %rcx
	testb	$1, 3(%rcx)
	jne	.L17559
.L16862:
	testq	%rbx, %rbx
	jne	.L17138
.L17139:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L17138
.L16861:
	movq	40(%r13), %rbx
.L16860:
	testq	%rbx, %rbx
	je	.L17159
.L17138:
	cmpb	$32, 16(%rbx)
	je	.L16864
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L16864
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.L16872
	movq	80(%rax), %rax
	cmpq	%rax, %rbx
	je	.L17440
	xorl	%r9d, %r9d
	testl	%r9d, %r9d
	jle	.L17560
.L17440:
	movq	%rax, %rbx
.L16864:
	testq	%rbx, %rbx
	jne	.L16877
.L17159:
	movq	112(%r12), %r11
	movq	%r12, 40(%r11)
.L16878:
	xorl	%esi, %esi
	xorl	%eax, %eax
	movq	%r12, %rdi
	movl	$1, %edx
	call	make_decl_rtl
	movq	-1000(%rbp), %rax
	movq	120(%rax), %rbx
	movq	64(%rbx), %rax
	movl	$0, -1036(%rbp)
	testq	%rax, %rax
	movq	%rax, -368(%rbp)
	je	.L16882
	movl	24(%rax), %r8d
	movl	%r8d, -1036(%rbp)
.L16882:
	xorl	%r14d, %r14d
	cmpl	-1036(%rbp), %r14d
	jge	.L16884
.L16895:
	movq	-368(%rbp), %r10
	movslq	%r14d,%rdx
	movq	32(%r10,%rdx,8), %rbx
	movq	8(%rbx), %rsi
	testb	$1, 19(%rsi)
	jne	.L16888
	cmpl	$1, flag_all_virtual(%rip)
	je	.L16888
.L16885:
	incl	%r14d
	cmpl	-1036(%rbp), %r14d
	jl	.L16895
.L16884:
	movl	-1052(%rbp), %r14d
	testl	%r14d, %r14d
	je	.L16827
	cmpq	$0, 144(%r12)
	jne	.L16897
	movq	error_mark_node(%rip), %r15
	movq	%r15, 144(%r12)
.L16897:
	movq	56(%r12), %r13
	orb	$-128, 18(%r13)
	cmpq	$0, -1000(%rbp)
	je	.L17313
	movq	-1000(%rbp), %rsi
	movq	144(%rsi), %rdx
	testb	$1, 2(%rdx)
	je	.L16827
	movq	80(%rsi), %r9
	movq	56(%r9), %r10
	cmpq	$0, 64(%r10)
	jne	.L16827
	movl	write_virtuals(%rip), %eax
	cmpl	$2, %eax
	je	.L16899
	cmpl	$3, %eax
	je	.L17561
.L16827:
	cmpq	$0, -1000(%rbp)
	jne	.L16900
.L17313:
	movq	-904(%rbp), %rdx
	movq	%rdx, 112(%r12)
.L16900:
	cmpl	$1, -892(%rbp)
	je	.L17562
.L16901:
	xorl	%edx, %edx
	cmpq	$0, -1000(%rbp)
	je	.L16908
	movq	-1000(%rbp), %rsi
	movq	144(%rsi), %r8
	testb	$6, 3(%r8)
	je	.L16907
.L16908:
	testl	$8454144, -1064(%rbp)
	jne	.L16906
.L16907:
	movl	$1, %edx
.L16906:
	movzbl	18(%r12), %r14d
	salb	$3, %dl
	andb	$-9, %r14b
	orb	%dl, %r14b
	movb	%r14b, 18(%r12)
	movl	-212(%rbp), %ebx
	testl	%ebx, %ebx
	jne	.L17563
.L16909:
	movl	-1008(%rbp), %edx
	testl	%edx, %edx
	je	.L16543
	orb	$32, 54(%r12)
	jmp	.L16543
.L17563:
	movq	-1072(%rbp), %r15
	xorl	%eax, %eax
	movq	24(%r15), %rdi
	call	tree_last
	movq	%rax, %r8
	movl	-1008(%rbp), %eax
	testl	%eax, %eax
	jne	.L16910
	cmpq	$0, -1000(%rbp)
	jne	.L16910
	movq	-360(%rbp), %r13
	movl	$.LC274, %edi
	movl	$5, %ecx
	cld
	movq	32(%r13), %rsi
	repz
	cmpsb
	je	.L17564
.L16910:
	testq	%r8, %r8
	je	.L16912
	cmpq	void_list_node(%rip), %r8
	je	.L16912
	movl	$.LC279, %edi
.L17442:
	xorl	%eax, %eax
	call	warning
.L16911:
	testl	$131072, -1064(%rbp)
	je	.L16909
	movl	pedantic(%rip), %r10d
	movl	$1, current_extern_inline(%rip)
	testl	%r10d, %r10d
	jne	.L17565
	movl	flag_ansi(%rip), %r9d
	testl	%r9d, %r9d
	je	.L16909
	movl	$.LC280, %edi
	xorl	%eax, %eax
	call	warning
	jmp	.L16909
.L17565:
	movl	$.LC280, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16909
.L16912:
	orb	$8, 53(%r12)
	jmp	.L16911
.L17564:
	movl	$.LC278, %edi
	jmp	.L17442
.L17562:
	movq	-1072(%rbp), %rdi
	xorl	%r8d, %r8d
	cmpb	$16, 16(%rdi)
	je	.L17566
	movl	-1008(%rbp), %ecx
	testl	%ecx, %ecx
	jne	.L16903
	cmpq	$0, -1000(%rbp)
	jne	.L16903
	movq	-360(%rbp), %r11
	movl	$.LC274, %edi
	movl	$5, %ecx
	cld
	movq	32(%r11), %rsi
	repz
	cmpsb
	je	.L17567
.L16903:
	movl	-1064(%rbp), %eax
	andl	$-65537, %eax
	testl	%r8d, %r8d
	cmove	-1064(%rbp), %eax
	movl	%eax, -1064(%rbp)
	jmp	.L16901
.L17567:
	movl	$.LC277, %edi
	xorl	%eax, %eax
	call	error
.L17441:
	movl	$1, %r8d
	jmp	.L16903
.L17566:
	movq	%r12, %rdi
	movl	$.LC276, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L17441
.L17561:
	testb	$4, 3(%rdx)
	jne	.L16827
.L16899:
	orb	$8, 18(%r12)
	jmp	.L16827
	.p2align 6,,7
.L16888:
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	testq	%rax, %rax
	movq	%rax, %r13
	je	.L16885
	movl	-1012(%rbp), %eax
	testl	%eax, %eax
	jne	.L17304
	movl	$1, -1052(%rbp)
	movq	8(%rbx), %rdx
	testb	$2, 19(%rdx)
	jne	.L16892
	movq	-1000(%rbp), %rdi
	movq	144(%rdi), %r9
	testb	$4, 1(%r9)
	je	.L16891
.L16892:
	movq	64(%r13), %rax
	cmpq	%rax, %rdx
	je	.L16891
	xorl	%edx, %edx
	movq	120(%rax), %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	movq	%rax, %r13
.L16891:
	movq	144(%r12), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	value_member
	testq	%rax, %rax
	jne	.L16885
	movq	8(%r12), %rbx
	movq	8(%r13), %r8
	xorl	%eax, %eax
	movq	24(%rbx), %rcx
	movq	24(%r8), %r11
	movq	32(%rcx), %rsi
	movq	(%r11), %rdi
	movq	8(%rsi), %rbx
	movq	(%rcx), %rsi
	call	commonparms
	movq	8(%r15), %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_cplus_method_type
	cmpq	$0, -1048(%rbp)
	movq	%rax, %r15
	jne	.L17568
.L16894:
	movq	144(%r12), %rdx
	movq	%r15, 8(%r12)
	xorl	%edi, %edi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 144(%r12)
	jmp	.L16885
.L17568:
	movq	-1000(%rbp), %rdi
	movq	-1048(%rbp), %rdx
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_exception_variant
	movq	%rax, %r15
	movq	128(%rax), %rax
	movq	%rax, -1048(%rbp)
	jmp	.L16894
.L17304:
	movq	%r12, %rdi
	movl	$.LC260, %esi
	xorl	%eax, %eax
	call	error_with_decl
	xorl	%eax, %eax
	movl	$.LC261, %esi
	movq	%r13, %rdi
	call	error_with_decl
	jmp	.L16884
	.p2align 6,,7
.L16877:
	movzbl	16(%r12), %r13d
	cmpb	%r13b, 16(%rbx)
	je	.L16879
	movq	%r12, %rdi
	movl	$.LC259, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L16878
.L16879:
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	duplicate_decls
	movq	%rbx, %r12
	jmp	.L16878
.L17560:
	testl	%edx, %edx
	jg	.L17440
	xorl	%r14d, %r14d
	testl	%r14d, %r14d
	je	.L16864
	movq	%rax, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L17440
	.p2align 6,,7
.L16872:
	movq	8(%rbx), %rdi
	cmpq	error_mark_node(%rip), %rdi
	cmove	%rdi, %rbx
	jmp	.L16864
.L17559:
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	movq	%r13, %rsi
	cmpl	$-1, %edx
	sete	%cl
	xorl	%eax, %eax
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rbx
	je	.L16864
	testq	%rax, %rax
	je	.L17139
	cmpb	$32, 16(%rax)
	cmovne	%r14, %rbx
	jmp	.L16862
	.p2align 6,,7
.L17558:
	movq	-1000(%rbp), %rdi
	movq	-1024(%rbp), %rsi
	movq	%r12, %rdx
	movl	-448(%rbp), %ecx
	xorl	%eax, %eax
	call	check_classfn
	jmp	.L16856
.L17557:
	movl	-1052(%rbp), %esi
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	grok_op_properties
	jmp	.L16855
.L17556:
	movq	-1024(%rbp), %rdi
	xorl	%eax, %eax
	call	constructor_name
	cmpq	-360(%rbp), %rax
	je	.L17569
.L16828:
	cmpq	$0, -1000(%rbp)
	je	.L16854
	movq	-1000(%rbp), %rdi
	movq	-1024(%rbp), %rsi
	movq	%r12, %rdx
	movl	-448(%rbp), %ecx
	movq	-1032(%rbp), %r8
	xorl	%eax, %eax
	call	grokclassfn
	jmp	.L16854
.L17569:
	movq	152(%r12), %r15
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	movq	%r12, %rdx
	orb	$2, 1(%r15)
	movq	-1000(%rbp), %rdi
	movq	-360(%rbp), %rsi
	movq	-1032(%rbp), %r8
	call	grokclassfn
	testl	%r14d, %r14d
	jne	.L17570
.L16829:
	movq	-1000(%rbp), %rdi
	xorl	%eax, %eax
	movq	%r12, %rsi
	call	grok_ctor_properties
	testl	%r14d, %r14d
	jne	.L16827
	xorl	%r15d, %r15d
	movq	global_binding_level(%rip), %r14
	movq	112(%r12), %r13
	cmpq	%r14, current_binding_level(%rip)
	je	.L16831
	movq	48(%r13), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.L16832
.L16831:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L16833
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	jne	.L17136
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L16834
	movq	144(%rdi), %r11
	testb	$1, 3(%r11)
	jne	.L17571
.L16834:
	testq	%rbx, %rbx
	jne	.L17136
.L17137:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L17136
.L16833:
	movq	40(%r13), %rbx
.L16832:
	testq	%rbx, %rbx
	je	.L17158
.L17136:
	cmpb	$32, 16(%rbx)
	je	.L16836
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L16836
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.L16844
	movq	80(%rax), %rax
	cmpq	%rax, %rbx
	je	.L17439
	testl	%r15d, %r15d
	jle	.L17572
.L17439:
	movq	%rax, %rbx
.L16836:
	testq	%rbx, %rbx
	jne	.L16849
.L17158:
	movq	112(%r12), %rbx
	movq	%r12, 40(%rbx)
.L16850:
	movq	%r12, %rdi
	xorl	%esi, %esi
	movl	$1, %edx
	xorl	%eax, %eax
	call	make_decl_rtl
	jmp	.L16827
.L16849:
	movzbl	16(%r12), %r8d
	cmpb	%r8b, 16(%rbx)
	je	.L16851
	movq	%r12, %rdi
	movl	$.LC259, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L16850
.L16851:
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	duplicate_decls
	movq	%rbx, %r12
	jmp	.L16850
.L17572:
	testl	%edx, %edx
	jg	.L17439
	testl	%r15d, %r15d
	je	.L16836
	movq	%rax, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L17439
.L16844:
	movq	8(%rbx), %r13
	cmpq	error_mark_node(%rip), %r13
	cmove	%r13, %rbx
	jmp	.L16836
.L17571:
	xorl	%ecx, %ecx
	movq	%r13, %rsi
	cmpl	$-1, %r15d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rbx
	je	.L16836
	testq	%rax, %rax
	je	.L17137
	cmpb	$32, 16(%rax)
	cmovne	%r14, %rbx
	jmp	.L16834
.L17570:
	movq	-1000(%rbp), %rdi
	movq	-360(%rbp), %rsi
	movq	%r12, %rdx
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	call	check_classfn
	jmp	.L16829
	.p2align 6,,7
.L17555:
	cmpq	$0, -1000(%rbp)
	movq	24(%r15), %r13
	je	.L16819
	testq	%r13, %r13
	je	.L16819
	cmpb	$16, 16(%r15)
	je	.L17573
.L16819:
	movq	ptr_type_node(%rip), %rsi
	movq	void_list_node(%rip), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	type_list_equal
	testl	%eax, %eax
	movl	%eax, %ebx
	je	.L17574
	xorl	%eax, %eax
.L16821:
	cmpq	$0, -1000(%rbp)
	jne	.L16822
	testl	%ebx, %ebx
	movl	$.LC257, %edi
	jne	.L17314
.L17438:
	xorl	%eax, %eax
	call	error
	jmp	.L17314
.L16822:
	testl	%ebx, %ebx
	jne	.L17314
	testl	%eax, %eax
	jne	.L17314
	movl	$.LC258, %edi
	jmp	.L17438
	.p2align 6,,7
.L17574:
	movq	sizetype(%rip), %rsi
	movq	void_list_node(%rip), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	type_list_equal
	jmp	.L16821
.L17573:
	movq	(%r13), %r13
	jmp	.L16819
	.p2align 6,,7
.L17554:
	movl	$.LC256, %edi
	xorl	%eax, %eax
	call	error
	movq	$0, -1032(%rbp)
	jmp	.L16817
.L17553:
	movq	-1000(%rbp), %rdi
	movq	-1072(%rbp), %rsi
	xorl	%eax, %eax
	movq	-1048(%rbp), %rdx
	call	build_exception_variant
	movq	128(%rax), %r10
	movq	%rax, %r15
	movq	%r10, -1048(%rbp)
	jmp	.L16814
.L17552:
	movq	56(%rsi), %rsi
	jmp	.L16811
.L16810:
	movq	$0, -1024(%rbp)
	jmp	.L16813
.L16805:
	xorl	%r14d, %r14d
	jmp	.L16806
.L17551:
	movq	-904(%rbp), %rbx
	movl	24(%rbx), %r8d
	cmpl	$4, %r8d
	je	.L17575
.L16801:
	cmpl	$10, %r8d
	jle	.L16802
	movq	-904(%rbp), %r9
	movq	32(%r9), %rax
	cmpb	$95, (%rax)
	je	.L17576
.L16802:
	movq	-1072(%rbp), %rbx
	movq	-464(%rbp), %rdi
	xorl	%edx, %edx
	xorl	%eax, %eax
	movq	24(%rbx), %rsi
	call	build_decl_overload
	movl	$1, -1008(%rbp)
	movq	%rax, -904(%rbp)
	jmp	.L16803
.L17576:
	cmpb	$95, 1(%rax)
	jne	.L16802
	leaq	2(%rax), %rsi
	movl	$.LC275, %edi
	movl	$8, %ecx
	cld
	repz
	cmpsb
	je	.L16803
	jmp	.L16802
	.p2align 6,,7
.L17575:
	movq	32(%rbx), %rsi
	cmpb	$109, (%rsi)
	jne	.L16801
	movl	$.LC274, %edi
	movl	$5, %ecx
	cld
	repz
	cmpsb
	je	.L16803
	jmp	.L16801
	.p2align 6,,7
.L17550:
	movq	-480(%rbp), %rsi
	movl	$.LC273, %edi
	xorl	%eax, %eax
	call	error
	movl	$0, -1052(%rbp)
	jmp	.L16799
.L16798:
	movq	-1072(%rbp), %rdi
	cmpb	$23, 16(%rdi)
	jne	.L16803
	cmpl	$1, -892(%rbp)
	jg	.L16803
	movq	-1000(%rbp), %rdi
	movl	-1060(%rbp), %esi
	xorl	%eax, %eax
	movl	-1056(%rbp), %edx
	call	build_type_variant
	movq	-1072(%rbp), %r11
	movq	%rax, %rdi
	xorl	%eax, %eax
	movq	8(%r11), %rsi
	movq	24(%r11), %rdx
	call	build_cplus_method_type
	movq	%rax, -1072(%rbp)
	jmp	.L16803
.L17549:
	movq	-480(%rbp), %rsi
	movl	$.LC272, %edi
	xorl	%eax, %eax
	call	pedwarn
	jmp	.L16797
.L17548:
	movq	-480(%rbp), %rsi
	movl	$.LC272, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16796
.L17535:
	movq	-1072(%rbp), %r12
	cmpq	error_mark_node(%rip), %r12
	je	.L16765
	movq	-1072(%rbp), %r9
	movzbl	16(%r9), %edx
	cmpb	$23, %dl
	je	.L17577
	cmpb	$16, %dl
	je	.L17578
	cmpb	$21, %dl
	je	.L17579
.L16753:
	movq	-1072(%rbp), %rdi
	cmpq	$0, 32(%rdi)
	jne	.L16760
	movl	-892(%rbp), %eax
	testl	%eax, %eax
	jne	.L16760
	cmpb	$18, %dl
	je	.L17580
.L16761:
	cmpq	$0, -904(%rbp)
	je	.L16762
	movq	-904(%rbp), %rbx
	movl	$.LC262, %edi
	xorl	%eax, %eax
	movq	32(%rbx), %rsi
	call	error
.L16763:
	movq	error_mark_node(%rip), %r11
	movq	%r11, -1072(%rbp)
.L16765:
	xorl	%r12d, %r12d
.L16546:
	movl	-1004(%rbp), %eax
	testl	%eax, %eax
	je	.L16768
	movq	-1000(%rbp), %r15
	cmpq	current_class_type(%rip), %r15
	je	.L17581
	testq	%r12, %r12
	je	.L17447
	cmpq	$0, 56(%r12)
	jne	.L17582
.L17447:
	movq	void_type_node(%rip), %rax
.L17448:
	movq	%rax, -1072(%rbp)
	jmp	.L13275
.L17582:
	movq	-1000(%rbp), %rdi
	movq	-904(%rbp), %rsi
	movq	%r12, %rdx
	movq	last_function_parms(%rip), %rcx
	movl	-448(%rbp), %r8d
	xorl	%eax, %eax
	movq	-1032(%rbp), %r9
	call	do_friend
	jmp	.L17448
.L17581:
	movl	$.LC265, %edi
	xorl	%eax, %eax
	call	warning
.L16768:
	testq	%r12, %r12
	jne	.L16543
	movl	-1052(%rbp), %eax
	testl	%eax, %eax
	jne	.L17583
.L16774:
	cmpq	$0, -1032(%rbp)
	jne	.L17584
.L16775:
	movl	-1004(%rbp), %r14d
	testl	%r14d, %r14d
	jne	.L17585
.L16776:
	cmpq	$0, -1048(%rbp)
	jne	.L17586
.L16777:
	movl	-604(%rbp), %esi
	testl	%esi, %esi
	je	.L16778
	movl	-892(%rbp), %r8d
	testl	%r8d, %r8d
	jne	.L17587
	movl	-1060(%rbp), %ecx
	testl	%ecx, %ecx
	je	.L16785
	movl	pedantic(%rip), %eax
	testl	%eax, %eax
	je	.L16778
	movl	-1060(%rbp), %r12d
	testl	%r12d, %r12d
	je	.L16785
	movl	pedantic(%rip), %edx
	testl	%edx, %edx
	je	.L16785
	movl	$.LC270, %esi
.L16786:
	movq	-904(%rbp), %r9
	movl	$.LC269, %edi
	xorl	%eax, %eax
	movq	32(%r9), %rdx
	call	error
.L16778:
	movl	-892(%rbp), %edi
	testl	%edi, %edi
	jne	.L16788
	movl	-1060(%rbp), %ebx
	testl	%ebx, %ebx
	je	.L16787
	movl	-604(%rbp), %r11d
	testl	%r11d, %r11d
	je	.L16787
.L16788:
	movq	-904(%rbp), %rsi
	movq	-1072(%rbp), %rdx
	xorl	%eax, %eax
	movl	$33, %edi
	call	build_lang_field_decl
	movl	-892(%rbp), %r13d
	movq	%rax, %r12
	testl	%r13d, %r13d
	jne	.L16790
	movq	-1072(%rbp), %r15
	cmpb	$18, 16(%r15)
	je	.L16790
.L16789:
	orb	$8, 18(%r12)
	movzbl	53(%r12), %r14d
	movl	-604(%rbp), %r8d
	testl	%r8d, %r8d
	sete	%sil
	andb	$-2, %r14b
	orb	%sil, %r14b
	movb	%r14b, 53(%r12)
	jmp	.L16543
.L16790:
	orb	$4, 18(%r12)
	jmp	.L16789
.L16787:
	movq	-904(%rbp), %rsi
	movq	-1072(%rbp), %rdx
	movl	$36, %edi
	xorl	%eax, %eax
	call	build_lang_field_decl
	movq	%rax, %r12
	jmp	.L16543
.L16785:
	movl	$.LC271, %esi
	jmp	.L16786
.L17587:
	movq	-904(%rbp), %r10
	movl	$.LC268, %edi
	xorl	%eax, %eax
	movq	32(%r10), %rsi
	call	error
	jmp	.L16778
.L17586:
	movl	$.LC251, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16777
.L17585:
	movl	$.LC250, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16776
.L17584:
	movl	$.LC267, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16775
.L17583:
	movl	$.LC266, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16774
.L16762:
	movl	$.LC263, %edi
	xorl	%eax, %eax
	call	error
	jmp	.L16763
.L17580:
	movl	-604(%rbp), %eax
	testl	%eax, %eax
	je	.L16761
.L16760:
	movl	-1004(%rbp), %eax
	testl	%eax, %eax
	je	.L16765
	cmpq	$0, -904(%rbp)
	jne	.L16951
	movl	$.LC250, %edi
.L17466:
	xorl	%eax, %eax
	call	error
.L17446:
	movq	void_type_node(%rip), %rbx
	movq	%rbx, -1072(%rbp)
	jmp	.L13275
.L16951:
	movq	-904(%rbp), %r13
	movl	$.LC264, %edi
	xorl	%eax, %eax
	movq	32(%r13), %rsi
	call	error
	movl	$0, -1004(%rbp)
	jmp	.L16765
.L17579:
	movq	-1072(%rbp), %rbx
	movq	144(%rbx), %r12
	testb	$16, 3(%r12)
	je	.L16753
	movq	-904(%rbp), %rsi
	xorl	%eax, %eax
	movl	$33, %edi
	movq	%rbx, %rdx
	call	build_lang_field_decl
	cmpq	$0, -1000(%rbp)
	movq	%rax, %rsi
	jne	.L16754
	movq	current_class_type(%rip), %rdx
	movq	%rdx, -1000(%rbp)
.L16754:
	movq	-1000(%rbp), %r9
	movq	80(%r9), %rdi
	cmpb	$32, 16(%rdi)
	je	.L17588
.L16758:
	xorl	%eax, %eax
	call	finish_exception_decl
	jmp	.L17446
.L17588:
	movq	56(%rdi), %rdi
	jmp	.L16758
.L17578:
	movl	-1004(%rbp), %esi
	movl	-1052(%rbp), %r13d
	movq	-1032(%rbp), %r8
	movq	-1048(%rbp), %rcx
	movq	-1072(%rbp), %r15
	testl	%esi, %esi
	movl	%r13d, -988(%rbp)
	sete	%r12b
	movq	%r8, -968(%rbp)
	movq	%rcx, -984(%rbp)
	movzbl	%r12b, %r14d
	decl	%r14d
	cmpq	$0, -1000(%rbp)
	je	.L16662
	movq	-1000(%rbp), %rdi
	movq	80(%rdi), %rsi
	cmpb	$32, 16(%rsi)
	je	.L17589
.L16663:
	movq	%rsi, -960(%rbp)
.L16665:
	cmpq	$0, -1048(%rbp)
	jne	.L17590
.L16666:
	movq	-904(%rbp), %rsi
	xorl	%eax, %eax
	movl	$29, %edi
	movq	%r15, %rdx
	call	build_lang_decl
	testb	$16, 17(%r15)
	movq	%rax, %r12
	je	.L16667
	orb	$16, 17(%rax)
.L16667:
	xorl	%r10d, %r10d
	testl	%r10d, %r10d
	je	.L16668
	movq	152(%r12), %r11
	orb	$32, 1(%r11)
	movq	-1000(%rbp), %rax
	movq	152(%r12), %r9
	movq	%rax, 64(%r12)
	movq	%rax, 16(%r9)
.L16668:
	orb	$1, 53(%r12)
	cmpq	$0, -1032(%rbp)
	je	.L16669
	cmpb	$23, 16(%r15)
	je	.L17591
.L16669:
	movq	-904(%rbp), %rdx
	cmpq	ansi_opname+984(%rip), %rdx
	je	.L17592
.L16670:
	testl	%r14d, %r14d
	js	.L16679
	movl	-448(%rbp), %esi
	testl	%esi, %esi
	jne	.L16680
	cmpq	$0, -1000(%rbp)
	jne	.L17593
.L16706:
	movq	56(%r12), %rbx
	testb	$1, 19(%rbx)
	jne	.L17594
.L16707:
	cmpq	$0, -1000(%rbp)
	je	.L16679
	testl	%r14d, %r14d
	jne	.L17595
.L16708:
	testl	%r14d, %r14d
	jne	.L16679
	movq	global_binding_level(%rip), %r9
	movq	112(%r12), %r13
	cmpq	%r9, current_binding_level(%rip)
	je	.L16711
	movq	48(%r13), %r11
	testq	%r11, %r11
	movq	%r11, %rbx
	jne	.L16712
.L16711:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L16713
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	jne	.L17134
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L16714
	movq	144(%rdi), %rdx
	testb	$1, 3(%rdx)
	jne	.L17596
.L16714:
	testq	%rbx, %rbx
	jne	.L17134
.L17135:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L17134
.L16713:
	movq	40(%r13), %rbx
.L16712:
	testq	%rbx, %rbx
	je	.L17157
.L17134:
	cmpb	$32, 16(%rbx)
	je	.L16716
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L16716
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.L16724
	movq	80(%rax), %rax
	cmpq	%rax, %rbx
	je	.L17436
	xorl	%r14d, %r14d
	testl	%r14d, %r14d
	jle	.L17597
.L17436:
	movq	%rax, %rbx
.L16716:
	testq	%rbx, %rbx
	jne	.L16729
.L17157:
	movq	112(%r12), %r8
	movq	%r12, 40(%r8)
.L16730:
	movq	%r12, %rdi
	xorl	%esi, %esi
	xorl	%eax, %eax
	movl	$1, %edx
	call	make_decl_rtl
	movq	-1000(%rbp), %r10
	movq	120(%r10), %rdi
	movq	64(%rdi), %r14
	movl	$0, -972(%rbp)
	testq	%r14, %r14
	je	.L16734
	movl	24(%r14), %r9d
	movl	%r9d, -972(%rbp)
.L16734:
	movl	$0, -976(%rbp)
	movl	-972(%rbp), %ebx
	cmpl	%ebx, -976(%rbp)
	jge	.L16736
.L16747:
	movslq	-976(%rbp),%rdx
	movq	32(%r14,%rdx,8), %rbx
	movq	8(%rbx), %r11
	testb	$1, 19(%r11)
	jne	.L16740
	cmpl	$1, flag_all_virtual(%rip)
	je	.L16740
.L16737:
	incl	-976(%rbp)
	movl	-972(%rbp), %r13d
	cmpl	%r13d, -976(%rbp)
	jl	.L16747
.L16736:
	movl	-988(%rbp), %eax
	testl	%eax, %eax
	je	.L16679
	cmpq	$0, 144(%r12)
	jne	.L16749
	movq	error_mark_node(%rip), %r15
	movq	%r15, 144(%r12)
.L16749:
	movq	56(%r12), %r14
	orb	$-128, 18(%r14)
	cmpq	$0, -1000(%rbp)
	je	.L16679
	movq	-1000(%rbp), %rax
	movq	144(%rax), %rdx
	testb	$1, 2(%rdx)
	je	.L16679
	movq	80(%rax), %r8
	movq	56(%r8), %rsi
	cmpq	$0, 64(%rsi)
	jne	.L16679
	movl	write_virtuals(%rip), %eax
	cmpl	$2, %eax
	je	.L16751
	cmpl	$3, %eax
	je	.L17598
.L16679:
	movzbl	-212(%rbp), %eax
	movzbl	53(%r12), %ecx
	salb	$3, %al
	andb	$-9, %cl
	orb	%al, %cl
	movb	%cl, 53(%r12)
.L17437:
	orb	$8, 18(%r12)
	jmp	.L16546
.L17598:
	testb	$4, 3(%rdx)
	jne	.L16679
.L16751:
	orb	$8, 18(%r12)
	jmp	.L16679
	.p2align 6,,7
.L16740:
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	testq	%rax, %rax
	movq	%rax, %r13
	je	.L16737
	xorl	%esi, %esi
	testl	%esi, %esi
	jne	.L17303
	movl	$1, -988(%rbp)
	movq	8(%rbx), %rdx
	testb	$2, 19(%rdx)
	jne	.L16744
	movq	-1000(%rbp), %rax
	movq	144(%rax), %r8
	testb	$4, 1(%r8)
	je	.L16743
.L16744:
	movq	64(%r13), %rax
	cmpq	%rax, %rdx
	je	.L16743
	xorl	%edx, %edx
	movq	120(%rax), %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	movq	%rax, %r13
.L16743:
	movq	144(%r12), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	value_member
	testq	%rax, %rax
	jne	.L16737
	movq	8(%r12), %rbx
	movq	8(%r13), %rdi
	xorl	%eax, %eax
	movq	24(%rbx), %rcx
	movq	24(%rdi), %r10
	movq	32(%rcx), %r9
	movq	(%rcx), %rsi
	movq	(%r10), %rdi
	movq	8(%r9), %rbx
	call	commonparms
	movq	8(%r15), %rsi
	movq	%rax, %rdx
	xorl	%eax, %eax
	movq	%rbx, %rdi
	call	build_cplus_method_type
	cmpq	$0, -984(%rbp)
	movq	%rax, %r15
	jne	.L17599
.L16746:
	movq	144(%r12), %rdx
	movq	%r15, 8(%r12)
	xorl	%edi, %edi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 144(%r12)
	jmp	.L16737
.L17599:
	movq	-1000(%rbp), %rdi
	movq	-984(%rbp), %rdx
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_exception_variant
	movq	128(%rax), %r11
	movq	%rax, %r15
	movq	%r11, -984(%rbp)
	jmp	.L16746
.L17303:
	movq	%r12, %rdi
	movl	$.LC260, %esi
	xorl	%eax, %eax
	call	error_with_decl
	xorl	%eax, %eax
	movl	$.LC261, %esi
	movq	%r13, %rdi
	call	error_with_decl
	jmp	.L16736
	.p2align 6,,7
.L16729:
	movzbl	16(%r12), %r13d
	cmpb	%r13b, 16(%rbx)
	je	.L16731
	movq	%r12, %rdi
	movl	$.LC259, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L16730
.L16731:
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	duplicate_decls
	movq	%rbx, %r12
	jmp	.L16730
.L17597:
	testl	%edx, %edx
	jg	.L17436
	xorl	%esi, %esi
	testl	%esi, %esi
	je	.L16716
	movq	%rax, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L17436
.L16724:
	movq	8(%rbx), %rcx
	cmpq	error_mark_node(%rip), %rcx
	cmove	%rcx, %rbx
	jmp	.L16716
.L17596:
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	movq	%r13, %rsi
	cmpl	$-1, %eax
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rbx
	je	.L16716
	testq	%rax, %rax
	je	.L17135
	cmpb	$32, 16(%rax)
	cmovne	%r14, %rbx
	jmp	.L16714
	.p2align 6,,7
.L17595:
	movq	-1000(%rbp), %rdi
	movq	-960(%rbp), %rsi
	movq	%r12, %rdx
	movl	-448(%rbp), %ecx
	xorl	%eax, %eax
	call	check_classfn
	jmp	.L16708
.L17594:
	movl	-1052(%rbp), %esi
	movq	%r12, %rdi
	xorl	%eax, %eax
	call	grok_op_properties
	jmp	.L16707
.L17593:
	movq	-960(%rbp), %rdi
	xorl	%eax, %eax
	call	constructor_name
	cmpq	-904(%rbp), %rax
	je	.L17600
.L16680:
	cmpq	$0, -1000(%rbp)
	je	.L16706
	movq	-1000(%rbp), %rdi
	movq	-960(%rbp), %rsi
	movq	%r12, %rdx
	movl	-448(%rbp), %ecx
	movq	-968(%rbp), %r8
	xorl	%eax, %eax
	call	grokclassfn
	jmp	.L16706
.L17600:
	movq	152(%r12), %r15
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	movq	%r12, %rdx
	orb	$2, 1(%r15)
	movq	-1000(%rbp), %rdi
	movq	-904(%rbp), %rsi
	movq	-968(%rbp), %r8
	call	grokclassfn
	testl	%r14d, %r14d
	jne	.L17601
.L16681:
	movq	-1000(%rbp), %rdi
	xorl	%eax, %eax
	movq	%r12, %rsi
	call	grok_ctor_properties
	testl	%r14d, %r14d
	jne	.L16679
	xorl	%r15d, %r15d
	movq	global_binding_level(%rip), %r14
	movq	112(%r12), %r13
	cmpq	%r14, current_binding_level(%rip)
	je	.L16683
	movq	48(%r13), %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.L16684
.L16683:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L16685
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	jne	.L17132
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L16686
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L17602
.L16686:
	testq	%rbx, %rbx
	jne	.L17132
.L17133:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L17132
.L16685:
	movq	40(%r13), %rbx
.L16684:
	testq	%rbx, %rbx
	je	.L17156
.L17132:
	cmpb	$32, 16(%rbx)
	je	.L16688
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L16688
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.L16696
	movq	80(%rax), %rax
	cmpq	%rax, %rbx
	je	.L17435
	testl	%r15d, %r15d
	jle	.L17603
.L17435:
	movq	%rax, %rbx
.L16688:
	testq	%rbx, %rbx
	jne	.L16701
.L17156:
	movq	112(%r12), %rdi
	movq	%r12, 40(%rdi)
.L16702:
	movq	%r12, %rdi
	xorl	%esi, %esi
	movl	$1, %edx
	xorl	%eax, %eax
	call	make_decl_rtl
	jmp	.L16679
.L16701:
	movzbl	16(%r12), %r10d
	cmpb	%r10b, 16(%rbx)
	je	.L16703
	movq	%r12, %rdi
	movl	$.LC259, %esi
	xorl	%eax, %eax
	call	error_with_decl
	jmp	.L16702
.L16703:
	movq	%r12, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	duplicate_decls
	movq	%rbx, %r12
	jmp	.L16702
.L17603:
	testl	%edx, %edx
	jg	.L17435
	testl	%r15d, %r15d
	je	.L16688
	movq	%rax, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	xorl	%eax, %eax
	call	arbitrate_lookup
	jmp	.L17435
.L16696:
	movq	8(%rbx), %r13
	cmpq	error_mark_node(%rip), %r13
	cmove	%r13, %rbx
	jmp	.L16688
.L17602:
	xorl	%ecx, %ecx
	movq	%r13, %rsi
	cmpl	$-1, %r15d
	sete	%cl
	xorl	%eax, %eax
	xorl	%edx, %edx
	call	lookup_field
	cmpq	error_mark_node(%rip), %rax
	movq	%rax, %rbx
	je	.L16688
	testq	%rax, %rax
	je	.L17133
	cmpb	$32, 16(%rax)
	cmovne	%r14, %rbx
	jmp	.L16686
.L17601:
	movq	-1000(%rbp), %rdi
	movq	-904(%rbp), %rsi
	movq	%r12, %rdx
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	call	check_classfn
	jmp	.L16681
	.p2align 6,,7
.L17592:
	cmpq	$0, -1000(%rbp)
	movq	24(%r15), %r13
	je	.L16671
	testq	%r13, %r13
	je	.L16671
	cmpb	$16, 16(%r15)
	je	.L17604
.L16671:
	movq	ptr_type_node(%rip), %rsi
	movq	void_list_node(%rip), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	type_list_equal
	testl	%eax, %eax
	movl	%eax, %ebx
	je	.L17605
	xorl	%eax, %eax
.L16673:
	cmpq	$0, -1000(%rbp)
	jne	.L16674
	testl	%ebx, %ebx
	movl	$.LC257, %edi
	jne	.L16670
.L17434:
	xorl	%eax, %eax
	call	error
	jmp	.L16670
	.p2align 6,,7
.L16674:
	testl	%ebx, %ebx
	jne	.L16670
	testl	%eax, %eax
	jne	.L16670
	movl	$.LC258, %edi
	jmp	.L17434
	.p2align 6,,7
.L17605:
	movq	sizetype(%rip), %rsi
	movq	void_list_node(%rip), %rdx
	xorl	%edi, %edi
	xorl	%eax, %eax
	call	tree_cons
	movq	ptr_type_node(%rip), %rsi
	xorl	%edi, %edi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	type_list_equal
	jmp	.L16673
.L17604:
	movq	(%r13), %r13
	jmp	.L16671
	.p2align 6,,7
.L17591:
	movl	$.LC256, %edi
	xorl	%eax, %eax
	call	error
	movq	$0, -968(%rbp)
	jmp	.L16669
.L17590:
	movq	-1000(%rbp), %rdi
	movq	-1072(%rbp), %rsi
	xorl	%eax, %eax
	movq	-1048(%rbp), %rdx
	call	build_exception_variant
	movq	128(%rax), %rbx
	movq	%rax, %r15
	movq	%rbx, -984(%rbp)
	jmp	.L16666
.L17589:
	movq	56(%rsi), %rsi
	jmp	.L16663
.L16662:
	movq	$0, -960(%rbp)
	jmp	.L16665
.L17577:
	movl	-1004(%rbp), %edx
	testl	%edx, %edx
	jne	.L16548
	cmpq	$0, -1000(%rbp)
	jne	.L16550
	movq	current_class_type(%rip), %r11
	testq	%r11, %r11
	movq	%r11, -1000(%rbp)
	jne	.L16550
	movq	-904(%rbp), %rcx
	movzbl	19(%rcx), %esi
	movl	%esi, %ebx
	andl	$1, %ebx
	jne	.L17606
	movq	-904(%rbp), %r15
	movq	32(%r15), %rdx
.L16556:
	testl	%ebx, %ebx
	movl	$.LC253, %esi
	movl	$.LC252, %edi
	movl	$.LC254, %ebx
	cmove	%rbx, %rsi
.L17471:
	xorl	%eax, %eax
	call	error
	jmp	.L17447
.L17606:
	movq	%rcx, %rdi
	xorl	%eax, %eax
	call	operator_name_string
	movq	%rax, %rdx
	jmp	.L16556
	.p2align 6,,7
.L16550:
	movl	-1052(%rbp), %r14d
	testl	%r14d, %r14d
	je	.L16559
	movq	-1000(%rbp), %rcx
	cmpb	$22, 16(%rcx)
	je	.L17607
.L16559:
	cmpl	$1, -892(%rbp)
	jle	.L17608
.L16548:
	movl	-1004(%rbp), %r10d
	movl	-1052(%rbp), %r9d
	movl	$0, -908(%rbp)
	movq	-1032(%rbp), %r12
	movq	-1048(%rbp), %r13
	movq	-1072(%rbp), %r15
	testl	%r10d, %r10d
	movl	%r9d, -948(%rbp)
	sete	%dil
	movq	%r12, -928(%rbp)
	movq	%r13, -944(%rbp)
	movzbl	%dil, %r14d
	decl	%r14d
	cmpq	$0, -1000(%rbp)
	je	.L16564
	cmpb	$23, 16(%r15)
	movl	$1, %eax
	movq	-1000(%rbp), %rdx
	cmovne	-908(%rbp), %eax
	movl	%eax, -908(%rbp)
	movq	80(%rdx), %rsi
	cmpb	$32, 16(%rsi)
	je	.L17609
.L16565:
	movq	%rsi, -920(%rbp)
.L16567:
	cmpq	$0, -1048(%rbp)
	jne	.L17610
.L16568:
	movq	-904(%rbp), %rsi
	xorl	%eax, %eax
	movl	$29, %edi
	movq	%r15, %rdx
	call	build_lang_decl
	testb	$16, 17(%r15)
	movq	%rax, %r12
	je	.L16569
	orb	$16, 17(%rax)
.L16569:
	movl	-908(%rbp), %esi
	testl	%esi, %esi
	je	.L16570
	movq	152(%r12), %rcx
	orb	$32, 1(%rcx)
	movq	-1000(%rbp), %rax
	movq	152(%r12), %rbx
	movq	%rax, 64(%r12)
	movq	%rax, 16(%rbx)
.L16570:
	orb	$1, 53(%r12)
	cmpq	$0, -1032(%rbp)
	je	.L16571
	cmpb	$23, 16(%r15)
	je	.L17611
.L16571:
	movq	-904(%rbp), %r8
	cmpq	ansi_opname+984(%rip), %r8
	je	.L17612
.L16572:
	testl	%r14d, %r14d
	js	.L16581
	movl	-448(%rbp), %edi
	testl	%edi, %edi
	jne	.L16582
	cmpq	$0, -1000(%rbp)
	jne	.L17613
.L16608:
	movq	56(%r12), %rsi
	testb	$1, 19(%rsi)
	jne	.L17614
.L16609:
	cmpq	$0, -1000(%rbp)
	je	.L16581
	testl	%r14d, %r14d
	jne	.L17615
.L16610:
	testl	%r14d, %r14d
	jne	.L16581
	movq	global_binding_level(%rip), %rbx
	movq	112(%r12), %r13
	cmpq	%rbx, current_binding_level(%rip)
	je	.L16613
	movq	48(%r13), %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.L16614
.L16613:
	movq	current_class_type(%rip), %rdi
	testq	%rdi, %rdi
	je	.L16615
	movq	56(%r13), %rbx
	testq	%rbx, %rbx
	jne	.L17130
	movq	32(%rdi), %r14
	testq	%r14, %r14
	jne	.L16616
	movq	144(%rdi), %r8
	testb	$1, 3(%r8)
	jne	.L17616
.L16616:
	testq	%rbx, %rbx
	jne	.L17130
.L17131:
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	lookup_nested_field
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.L17130
.L16615:
	movq	40(%r13), %rbx
.L16614:
	testq	%rbx, %rbx
	je	.L17155
.L17130:
	cmpb	$32, 16(%rbx)
	je	.L16618
	movl	looking_for_typename(%rip), %edx
	testl	%edx, %edx
	js	.L16618
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.L16626
	movq	80(%rax), %rax
	cmpq	%rax, %rbx
	je	.L17433
	xorl	%edi, %edi
	testl	%edi, %edi
	jle	.L17617
.L17433:
	movq	%rax, %rbx
.L16618:
	testq	%rbx, %rbx
	jne	.L16631
.L17155:
	movq	112(%r12), %r9
	movq	%r12, 40(%r9)
.L16632:
	xorl	%esi, %esi
	xorl	%eax, %eax
	movq	%r12, %rdi
	movl	$1, %edx
	call	make_decl_rtl
	movq	-1000(%rbp), %rax
	movq	120(%rax), %r11
	movq	64(%r11), %r14
	movl	$0, -932(%rbp)
	testq	%r14, %r14
	je	.L16636
	movl	24(%r14), %edx
	movl	%edx, -932(%rbp)
.L16636:
	movl	$0, -936(%rbp)
	movl	-932(%rbp), %esi
	cmpl	%esi, -936(%rbp)
	jge	.L16638
.L16649:
	movslq	-936(%rbp),%r8
	movq	32(%r14,%r8,8), %rbx
	movq	8(%rbx), %rcx
	testb	$1, 19(%rcx)
	jne	.L16642
	cmpl	$1, flag_all_virtual(%rip)
	je	.L16642
.L16639:
	incl	-936(%rbp)
	movl	-932(%rbp), %r13d
	cmpl	%r13d, -936(%rbp)
	jl	.L16649
.L16638:
	movl	-948(%rbp), %r15d
	testl	%r15d, %r15d
	je	.L16581
	cmpq	$0, 144(%r12)
	jne	.L16651
	movq	error_mark_node(%rip), %r14
	movq	%r14, 144(%r12)
.L16651:
	movq	56(%r12), %rdi
	orb	$-128, 18(%rdi)
	cmpq	$0, -1000(%rbp)
	je	.L16581
	movq	-1000(%rbp), %rcx
	movq	144(%rcx), %rdx
	testb	$1, 2(%rdx)
	je	.L16581
	movq	80(%rcx), %r10
	movq	56(%r10), %rbx
	cmpq	$0, 64(%rbx)
	jne	.L16581
	movl	write_virtuals(%rip), %eax
	cmpl	$2, %eax
	je	.L16653
	cmpl	$3, %eax
	je	.L17618
.L16581:
	movzbl	-212(%rbp), %eax
	movzbl	53(%r12), %r9d
	salb	$3, %al
	andb	$-9, %r9b
	orb	%al, %r9b
	movb	%r9b, 53(%r12)
	testl	$131072, -1064(%rbp)
	jne	.L17437
	cmpq	$0, -1000(%rbp)
	je	.L16656
	movl	-376(%rbp), %r11d
	testl	%r11d, %r11d
	jns	.L17437
.L16656:
	movl	-1004(%rbp), %edx
	testl	%edx, %edx
	je	.L16768
	testl	$8454144, -1064(%rbp)
	jne	.L16546
	jmp	.L17437
.L17618:
	testb	$4, 3(%rdx)
	jne	.L16581
.L16653:
	orb	$8, 18(%r12)
	jmp	.L16581
	.p2align 6,,7
.L16642:
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	testq	%rax, %rax
	movq	%rax, %r13
	je	.L16639
	movl	-908(%rbp), %edi
	testl	%edi, %edi
	jne	.L17302
	movl	$1, -948(%rbp)
	movq	8(%rbx), %rdx
	testb	$2, 19(%rdx)
	jne	.L16646
	movq	-1000(%rbp), %r10
	movq	144(%r10), %rbx
	testb	$4, 1(%rbx)
	je	.L16645
.L16646:
	movq	64(%r13), %rax
	cmpq	%rax, %rdx
	je	.L16645
	xorl	%edx, %edx
	movq	120(%rax), %rdi
	movq	%r12, %rsi
	cmpl	$1, -448(%rbp)
	sete	%dl
	xorl	%eax, %eax
	call	get_first_matching_virtual
	movq	%rax, %r13
.L16645:
	movq	144(%r12), %rsi
	xorl	%eax, %eax
	movq	%r13, %rdi
	call	value_member
	testq	%rax, %rax
	jne	.L16639
	movq	8(%r12), %rcx
	movq	8(%r13), %rdx
	xorl	%eax, %eax
	movq	24(%rcx), %r9
	movq	24(%rdx), %r11
	movq	32(%r9), %rsi
	movq	(%r11), %rdi
	movq	8(%rsi), %rbx
	movq	(%r9), %rsi
	call	commonparms
	movq	8(%r15), %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	build_cplus_method_type
	cmpq	$0, -944(%rbp)
	movq	%rax, %r15
	jne	.L17619
.L16648:
	movq	144(%r12), %rdx
	movq	%r15, 8(%r12)
	xorl	%edi, %edi
	movq	%r13, %rsi
	xorl	%eax, %eax
	call	tree_cons
	movq	%rax, 144(%r12)
	jmp	.L16639
.L17619:
	movq	-1000(%rbp), %rdi
	movq	-944(%rbp), %rdx
	movq	%rax, %rsi
	xorl	%eax, %eax
	call	build_exception_variant
	movq	128(%rax), %r8
	movq	%rax, %r15
	movq	%r8, -944(%rbp)
	jmp	.L16648
.L17302:
	movq	%r12, %rdi
	movl	$.LC260, %esi
	xorl	%eax, %eax
	call	error_with_decl
	xorl	%eax, %eax
	movl	$.LC261, %esi
.L16631: