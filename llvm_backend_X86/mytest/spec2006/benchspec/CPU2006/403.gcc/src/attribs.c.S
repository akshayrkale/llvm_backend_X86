	.file	"attribs.c.bc"
	.text
	.globl	decl_attributes
	.align	16, 0x90
	.type	decl_attributes,@function
decl_attributes:                        # @decl_attributes
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
	subq	$80, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 128
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
	movzbl	attributes_initialized(%rip), %eax
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movq	%rsi, 72(%rsp)
	testq	%rax, %rax
	jne	.LBB0_5
# BB#1:                                 # %if.then
	movl	lang_attribute_common(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_3
# BB#2:
	movabsq	$c_common_attribute_table, %rax
	jmp	.LBB0_4
.LBB0_3:                                # %select.mid
	movabsq	$empty_attribute_table, %rax
.LBB0_4:                                # %select.end
	movq	%rax, attribute_tables(%rip)
	movq	lang_attribute_table(%rip), %rax
	movq	%rax, attribute_tables+8(%rip)
	movq	format_attribute_table(%rip), %rax
	movq	targetm+256(%rip), %rcx
	movq	%rax, attribute_tables+16(%rip)
	movq	%rcx, attribute_tables+24(%rip)
	movq	$1, %rax
	movb	%al, attributes_initialized(%rip)
.LBB0_5:                                # %if.end
	movq	(%rbx), %rdi
	leaq	72(%rsp), %rsi
	callq	*targetm+280(%rip)
	movq	(%rbx), %rdi
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$100, %rcx
	jne	.LBB0_9
# BB#6:                                 # %if.end
	cmpq	$30, %rax
	jne	.LBB0_9
# BB#7:                                 # %if.end
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	$16, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_9
# BB#8:                                 # %if.then10
	callq	insert_default_attributes
.LBB0_9:                                # %for.cond.preheader
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB0_68
# BB#10:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_11:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_13 Depth 2
                                        #       Child Loop BB0_15 Depth 3
                                        #     Child Loop BB0_59 Depth 2
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	24(%rax), %r13
	movq	32(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movb	%al, 71(%rsp)
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_13:                               # %for.cond18.preheader
                                        #   Parent Loop BB0_11 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_15 Depth 3
	movq	attribute_tables(,%rbx,8), %rax
	movq	(%rax), %rdi
	xorq	%r14, %r14
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_14:                               # %for.cond18
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	32(%r12,%r14), %rdi
	addq	$32, %r14
.LBB0_15:                               # %for.cond18
                                        #   Parent Loop BB0_11 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rdi, %rdi
	je	.LBB0_12
# BB#16:                                # %for.body26
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	%r13, %rsi
	callq	is_attribute_p
	movq	attribute_tables(,%rbx,8), %r12
	testq	%r15, %rax
	je	.LBB0_14
# BB#17:                                # %for.end
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	%r12, %rax
	addq	%r14, %rax
	jne	.LBB0_18
.LBB0_12:                               # %for.cond14
                                        #   in Loop: Header=BB0_13 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jle	.LBB0_13
# BB#22:                                # %if.then48
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	32(%r13), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str, %rdi
	callq	warning
	jmp	.LBB0_67
	.align	16, 0x90
.LBB0_18:                               # %if.else
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	list_length
	movq	$32, %rcx
	movslq	8(%r12,%r14), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_21
# BB#19:                                # %lor.lhs.false
                                        #   in Loop: Header=BB0_11 Depth=1
	movslq	12(%r12,%r14), %rax
	testq	%rax, %rax
	js	.LBB0_24
# BB#20:                                # %land.lhs.true54
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	list_length
	movq	$32, %rcx
	movslq	12(%r12,%r14), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jle	.LBB0_24
	.align	16, 0x90
.LBB0_21:                               # %if.then59
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	32(%r13), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str1, %rdi
	callq	error
	jmp	.LBB0_67
.LBB0_24:                               # %if.end64
                                        #   in Loop: Header=BB0_11 Depth=1
	movzbl	16(%r12,%r14), %eax
	testq	%rax, %rax
	je	.LBB0_29
# BB#25:                                # %land.lhs.true67
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	16(%rax), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB0_26
.LBB0_29:                               # %if.end86
                                        #   in Loop: Header=BB0_11 Depth=1
	movzbl	17(%r12,%r14), %eax
	testq	%rax, %rax
	jne	.LBB0_31
# BB#30:                                #   in Loop: Header=BB0_11 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB0_34
.LBB0_31:                               # %land.lhs.true89
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rdi
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB0_32
# BB#33:                                # %if.then99
                                        #   in Loop: Header=BB0_11 Depth=1
	addq	$8, %rdi
	movabsq	$4294967287, %rax       # imm = 0xFFFFFFF7
	andq	%rax, 24(%rsp)          # 8-byte Folded Spill
	jmp	.LBB0_34
.LBB0_26:                               # %if.then77
                                        #   in Loop: Header=BB0_11 Depth=1
	testq	$7, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_27
# BB#28:                                # %if.else82
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	32(%r13), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str2, %rdi
	callq	warning
	jmp	.LBB0_67
.LBB0_32:                               #   in Loop: Header=BB0_11 Depth=1
	movq	%rcx, %rdi
.LBB0_34:                               # %if.end102
                                        #   in Loop: Header=BB0_11 Depth=1
	movzbl	18(%r12,%r14), %eax
	testq	%rax, %rax
	je	.LBB0_47
# BB#35:                                # %land.lhs.true105
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	(%rdi), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$23, %rdx
	je	.LBB0_47
# BB#36:                                # %land.lhs.true105
                                        #   in Loop: Header=BB0_11 Depth=1
	cmpq	$16, %rdx
	je	.LBB0_47
# BB#37:                                # %land.lhs.true105
                                        #   in Loop: Header=BB0_11 Depth=1
	cmpq	$13, %rdx
	jne	.LBB0_43
# BB#38:                                # %land.lhs.true126
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	8(%rax), %rdx
	movl	16(%rdx), %edx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$16, %rsi
	je	.LBB0_40
# BB#39:                                # %land.lhs.true126
                                        #   in Loop: Header=BB0_11 Depth=1
	cmpq	$23, %rsi
	jne	.LBB0_43
.LBB0_40:                               # %if.then144
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%rdi, %rbx
	testq	$8, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_42
# BB#41:                                # %if.then147
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%rax, %rdi
	callq	build_type_copy
	movq	%rax, (%rbx)
	movq	8(%rax), %rcx
	movl	16(%rcx), %edx
.LBB0_42:                               # %if.end149
                                        #   in Loop: Header=BB0_11 Depth=1
	addq	$8, %rax
	movq	%rdx, %rcx
	movq	%rax, %rdi
	jmp	.LBB0_44
.LBB0_43:                               # %if.else152
                                        #   in Loop: Header=BB0_11 Depth=1
	testq	$2, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_27
.LBB0_44:                               # %if.end158
                                        #   in Loop: Header=BB0_11 Depth=1
	andq	$255, %rcx
	cmpq	$16, %rcx
	je	.LBB0_47
# BB#45:                                # %if.end158
                                        #   in Loop: Header=BB0_11 Depth=1
	cmpq	$23, %rcx
	jne	.LBB0_46
.LBB0_47:                               # %if.end177
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	24(%r12,%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_49
# BB#48:                                #   in Loop: Header=BB0_11 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	jmp	.LBB0_50
.LBB0_49:                               # %if.then180
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	71(%rsp), %r8
	callq	*%rbx
	movq	%rax, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	chainon
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB0_50:                               # %if.end184
                                        #   in Loop: Header=BB0_11 Depth=1
	movzbl	17(%r12,%r14), %eax
	testq	%rax, %rax
	je	.LBB0_54
# BB#51:                                # %land.lhs.true188
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %edx
	cmpq	$100, %rdx
	jne	.LBB0_54
# BB#52:                                # %land.lhs.true188
                                        #   in Loop: Header=BB0_11 Depth=1
	addq	$-34, %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	ja	.LBB0_54
# BB#53:                                # %if.then219
                                        #   in Loop: Header=BB0_11 Depth=1
	andq	$-256, 48(%rax)
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	$0, 40(%rax)
	movq	(%rcx), %rdi
	xorq	%rsi, %rsi
	callq	layout_decl
.LBB0_54:                               # %if.end223
                                        #   in Loop: Header=BB0_11 Depth=1
	movzbl	71(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB0_67
# BB#55:                                # %if.then225
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB0_57
# BB#56:                                # %if.then236
                                        #   in Loop: Header=BB0_11 Depth=1
	addq	$136, %rax
	jmp	.LBB0_58
.LBB0_57:                               # %if.else239
                                        #   in Loop: Header=BB0_11 Depth=1
	addq	$48, %rax
.LBB0_58:                               # %if.end242
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	(%rax), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	(%r12,%r14), %rdi
	jmp	.LBB0_59
	.align	16, 0x90
.LBB0_61:                               # %for.inc256
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	(%r12,%r14), %rdi
	movq	(%rbx), %rsi
.LBB0_59:                               # %if.end242
                                        #   Parent Loop BB0_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	callq	lookup_attribute
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB0_62
# BB#60:                                # %for.body248
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	32(%rbx), %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	simple_cst_equal
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB0_61
	jmp	.LBB0_67
.LBB0_62:                               # %if.then263
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %r12
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB0_64
# BB#63:                                # %if.then273
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	tree_cons
	movq	(%rbx), %rcx
	movq	%rax, 136(%rcx)
	jmp	.LBB0_67
.LBB0_64:                               # %if.else277
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%rbx, %r14
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	tree_cons
	testq	$8, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_65
# BB#66:                                # %if.else284
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_type_attribute_variant
	movq	%rax, (%r14)
	jmp	.LBB0_67
.LBB0_27:                               # %if.then80
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	tree_cons
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB0_67
.LBB0_65:                               # %if.then280
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	(%r14), %rcx
	movq	%rax, 48(%rcx)
	jmp	.LBB0_67
.LBB0_46:                               # %if.then172
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	32(%r13), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rdi
	callq	warning
	.align	16, 0x90
.LBB0_67:                               # %for.inc291
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB0_11
.LBB0_68:                               # %for.end294
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	decl_attributes, .Ltmp17-decl_attributes
	.cfi_endproc

	.globl	split_specs_attrs
	.align	16, 0x90
	.type	split_specs_attrs,@function
split_specs_attrs:                      # @split_specs_attrs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 16
.Ltmp21:
	.cfi_offset %rbx, -16
	xorq	%r10, %r10
	testq	%rdi, %rdi
	je	.LBB1_1
# BB#12:                                # %land.lhs.true
	movzbl	16(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB1_14
# BB#13:
	xorq	%rbx, %rbx
	jmp	.LBB1_2
.LBB1_1:
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
.LBB1_2:                                # %for.cond.outer.outer
	xorq	%r8, %r8
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_20:                               #   in Loop: Header=BB1_3 Depth=1
	movq	%rax, %r10
.LBB1_3:                                # %for.cond.outer.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_26 Depth 2
                                        #       Child Loop BB1_24 Depth 3
	testq	%r10, %r10
	je	.LBB1_26
	jmp	.LBB1_4
.LBB1_25:                               #   in Loop: Header=BB1_26 Depth=2
	movq	%r9, %r8
	jmp	.LBB1_26
	.align	16, 0x90
.LBB1_19:                               # %for.body.us
                                        #   in Loop: Header=BB1_26 Depth=2
	movq	(%rax), %rdi
	cmpq	$0, 32(%rax)
	jne	.LBB1_20
# BB#21:                                # %if.else19.us
                                        #   in Loop: Header=BB1_26 Depth=2
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_22
.LBB1_26:                               # %for.cond.us
                                        #   Parent Loop BB1_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_24 Depth 3
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	jne	.LBB1_19
	jmp	.LBB1_27
.LBB1_22:                               # %if.then22.us
                                        #   in Loop: Header=BB1_26 Depth=2
	testq	%r8, %r8
	movq	%rcx, %r9
	je	.LBB1_24
# BB#23:                                # %if.else27.us
                                        #   in Loop: Header=BB1_26 Depth=2
	movq	%rcx, (%rbx)
	movq	24(%rax), %rcx
	movq	%r8, %r9
	.align	16, 0x90
.LBB1_24:                               # %while.cond.us
                                        #   Parent Loop BB1_3 Depth=1
                                        #     Parent Loop BB1_26 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %rbx
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_24
	jmp	.LBB1_25
.LBB1_27:
	movq	%r10, %r9
.LBB1_7:                                # %for.end
	testq	%r9, %r9
	je	.LBB1_9
# BB#8:                                 # %if.then43
	movq	$0, (%r9)
.LBB1_9:                                # %if.end46
	testq	%rbx, %rbx
	je	.LBB1_11
# BB#10:                                # %if.then48
	movq	$0, (%rbx)
.LBB1_11:                               # %if.end51
	movq	%rdi, (%rsi)
	movq	%r8, (%rdx)
	popq	%rbx
	retq
.LBB1_14:                               # %land.lhs.true
	cmpq	$25, %rax
	jne	.LBB1_18
# BB#15:                                # %if.then
	movq	$0, (%rsi)
	jmp	.LBB1_16
.LBB1_18:                               # %if.then9
	movq	%rdi, (%rsi)
.LBB1_16:                               # %return
	movq	$0, (%rdx)
	popq	%rbx
	retq
.LBB1_4:
	movq	%r10, %r9
	jmp	.LBB1_5
.LBB1_28:                               # %if.else
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	%r11, (%r9)
	movq	%r11, %r9
	.align	16, 0x90
.LBB1_5:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_33 Depth 2
	movq	%rdi, %r11
	testq	%r11, %r11
	je	.LBB1_6
# BB#29:                                # %for.body
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	(%r11), %rdi
	cmpq	$0, 32(%r11)
	jne	.LBB1_28
# BB#30:                                # %if.else19
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	24(%r11), %rcx
	testq	%rcx, %rcx
	je	.LBB1_5
# BB#31:                                # %if.then22
                                        #   in Loop: Header=BB1_5 Depth=1
	testq	%r8, %r8
	movq	%rcx, %rax
	je	.LBB1_33
# BB#32:                                # %if.else27
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	%rcx, (%rbx)
	movq	24(%r11), %rcx
	movq	%r8, %rax
	.align	16, 0x90
.LBB1_33:                               # %while.cond
                                        #   Parent Loop BB1_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rbx
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_33
# BB#34:                                #   in Loop: Header=BB1_5 Depth=1
	movq	%rax, %r8
	jmp	.LBB1_5
.LBB1_6:
	movq	%r10, %rdi
	jmp	.LBB1_7
.Ltmp22:
	.size	split_specs_attrs, .Ltmp22-split_specs_attrs
	.cfi_endproc

	.globl	strip_attrs
	.align	16, 0x90
	.type	strip_attrs,@function
strip_attrs:                            # @strip_attrs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp29:
	.cfi_def_cfa_offset 32
.Ltmp30:
	.cfi_offset %rbx, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	xorq	%rax, %rax
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	movabsq	$0, %rbx
	je	.LBB2_15
# BB#1:                                 # %land.lhs.true.i
	movzbl	16(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$2, %rcx
	jne	.LBB2_3
# BB#2:
	movq	%rdi, %rcx
	xorq	%rbx, %rbx
	jmp	.LBB2_15
.LBB2_14:                               #   in Loop: Header=BB2_15 Depth=1
	movq	%rsi, %rbx
	jmp	.LBB2_15
	.align	16, 0x90
.LBB2_8:                                # %for.body.us.i
                                        #   in Loop: Header=BB2_15 Depth=1
	movq	(%r15), %rcx
	cmpq	$0, 32(%r15)
	jne	.LBB2_9
# BB#10:                                # %if.else19.us.i
                                        #   in Loop: Header=BB2_15 Depth=1
	movq	24(%r15), %rdx
	testq	%rdx, %rdx
	jne	.LBB2_11
.LBB2_15:                               # %for.cond.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_13 Depth 2
	movq	%rcx, %r15
	testq	%r15, %r15
	jne	.LBB2_8
	jmp	.LBB2_16
.LBB2_11:                               # %if.then22.us.i
                                        #   in Loop: Header=BB2_15 Depth=1
	testq	%rbx, %rbx
	movq	%rdx, %rsi
	je	.LBB2_13
# BB#12:                                # %if.else27.us.i
                                        #   in Loop: Header=BB2_15 Depth=1
	movq	%rdx, (%rax)
	movq	24(%r15), %rdx
	movq	%rbx, %rsi
	.align	16, 0x90
.LBB2_13:                               # %while.cond.us.i
                                        #   Parent Loop BB2_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rax
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB2_13
	jmp	.LBB2_14
.LBB2_16:
	xorq	%r15, %r15
	jmp	.LBB2_24
.LBB2_9:
	movq	%r15, %r8
	jmp	.LBB2_5
.LBB2_7:                                # %if.else.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%rsi, (%r8)
	movq	%rsi, %r8
	.align	16, 0x90
.LBB2_5:                                # %for.cond.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_20 Depth 2
	movq	%rcx, %rsi
	testq	%rsi, %rsi
	je	.LBB2_22
# BB#6:                                 # %for.body.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	(%rsi), %rcx
	cmpq	$0, 32(%rsi)
	jne	.LBB2_7
# BB#17:                                # %if.else19.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	24(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB2_5
# BB#18:                                # %if.then22.i
                                        #   in Loop: Header=BB2_5 Depth=1
	testq	%rbx, %rbx
	movq	%rdi, %rdx
	je	.LBB2_20
# BB#19:                                # %if.else27.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%rdi, (%rax)
	movq	24(%rsi), %rdi
	movq	%rbx, %rdx
	.align	16, 0x90
.LBB2_20:                               # %while.cond.i
                                        #   Parent Loop BB2_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB2_20
# BB#21:                                #   in Loop: Header=BB2_5 Depth=1
	movq	%rdx, %rbx
	jmp	.LBB2_5
.LBB2_22:                               # %for.end.i
	testq	%r8, %r8
	je	.LBB2_24
# BB#23:                                # %if.then43.i
	movq	$0, (%r8)
.LBB2_24:                               # %if.end46.i
	testq	%rax, %rax
	je	.LBB2_26
# BB#25:                                # %if.then48.i
	movq	$0, (%rax)
.LBB2_26:                               # %split_specs_attrs.exit
	testq	%rbx, %rbx
	je	.LBB2_29
# BB#27:
	movabsq	$.L.str4, %r14
	.align	16, 0x90
.LBB2_28:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	warning
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_28
.LBB2_29:
	movq	%r15, %rax
	jmp	.LBB2_30
.LBB2_3:                                # %land.lhs.true.i
	cmpq	$25, %rcx
	je	.LBB2_30
# BB#4:                                 # %if.then9.i
	movq	%rdi, %rax
.LBB2_30:                               # %while.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	strip_attrs, .Ltmp33-strip_attrs
	.cfi_endproc

	.align	16, 0x90
	.type	handle_packed_attribute,@function
handle_packed_attribute:                # @handle_packed_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 32
.Ltmp40:
	.cfi_offset %rbx, -24
.Ltmp41:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movzbl	16(%rax), %edx
	movzbl	tree_code_type(%rdx), %edi
	movq	%r8, %r14
	cmpq	$100, %rdi
	jne	.LBB3_4
# BB#1:                                 # %if.then
	cmpq	$37, %rdx
	jne	.LBB3_2
# BB#7:                                 # %if.then26
	orq	$1024, 48(%rax)         # imm = 0x400
	jmp	.LBB3_9
.LBB3_2:                                # %if.then
	cmpq	$33, %rdx
	jne	.LBB3_8
# BB#3:
	addq	$8, %rax
	movq	%rax, %rbx
.LBB3_4:                                # %if.then12
	movq	(%rbx), %rax
	testq	$8, %rcx
	jne	.LBB3_6
# BB#5:                                 # %if.then14
	movq	%rax, %rdi
	callq	build_type_copy
	movq	%rax, (%rbx)
.LBB3_6:                                # %if.end15
	movl	60(%rax), %ecx
	orq	$1048576, %rcx          # imm = 0x100000
	movl	%ecx, 60(%rax)
	jmp	.LBB3_9
.LBB3_8:                                # %if.else30
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB3_9:                                # %if.end32
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp42:
	.size	handle_packed_attribute, .Ltmp42-handle_packed_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_nocommon_attribute,@function
handle_nocommon_attribute:              # @handle_nocommon_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 16
.Ltmp46:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%r8, %rbx
	cmpq	$34, %rcx
	jne	.LBB4_2
# BB#1:                                 # %if.then
	andq	$-131073, 48(%rax)      # imm = 0xFFFFFFFFFFFDFFFF
	jmp	.LBB4_3
.LBB4_2:                                # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB4_3:                                # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp47:
	.size	handle_nocommon_attribute, .Ltmp47-handle_nocommon_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_common_attribute,@function
handle_common_attribute:                # @handle_common_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 16
.Ltmp51:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%r8, %rbx
	cmpq	$34, %rcx
	jne	.LBB5_2
# BB#1:                                 # %if.then
	orq	$131072, 48(%rax)       # imm = 0x20000
	jmp	.LBB5_3
.LBB5_2:                                # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB5_3:                                # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp52:
	.size	handle_common_attribute, .Ltmp52-handle_common_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_noreturn_attribute,@function
handle_noreturn_attribute:              # @handle_noreturn_attribute
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
	movq	(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%r8, %r14
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$30, %rdx
	jne	.LBB6_2
# BB#1:                                 # %if.then
	orq	$2048, %rcx             # imm = 0x800
	movl	%ecx, 16(%rax)
	jmp	.LBB6_6
.LBB6_2:                                # %if.else
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB6_5
# BB#3:                                 # %land.lhs.true
	movq	8(%rax), %rdi
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	jne	.LBB6_5
# BB#4:                                 # %if.then18
	movq	$12, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$2, %rax
	movq	%rax, %rsi
	callq	build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB6_6
.LBB6_5:                                # %if.else29
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB6_6:                                # %if.end30
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp61:
	.size	handle_noreturn_attribute, .Ltmp61-handle_noreturn_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_noinline_attribute,@function
handle_noinline_attribute:              # @handle_noinline_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 16
.Ltmp65:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%r8, %rbx
	cmpq	$30, %rcx
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$68719476736, %rcx      # imm = 0x1000000000
	orq	%rcx, 48(%rax)
	jmp	.LBB7_3
.LBB7_2:                                # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB7_3:                                # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp66:
	.size	handle_noinline_attribute, .Ltmp66-handle_noinline_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_always_inline_attribute,@function
handle_always_inline_attribute:         # @handle_always_inline_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 16
.Ltmp70:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %eax
	movq	%r8, %rbx
	cmpq	$30, %rax
	je	.LBB8_2
# BB#1:                                 # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB8_2:                                # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp71:
	.size	handle_always_inline_attribute, .Ltmp71-handle_always_inline_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_used_attribute,@function
handle_used_attribute:                  # @handle_used_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp77:
	.cfi_def_cfa_offset 32
.Ltmp78:
	.cfi_offset %rbx, -24
.Ltmp79:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%r8, %r14
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$30, %rdx
	jne	.LBB9_4
# BB#1:                                 # %if.then
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movq	(%rbx), %rdi
	movq	120(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB9_3
# BB#2:                                 # %cond.false
	callq	*lang_set_decl_assembler_name(%rip)
	movq	(%rbx), %rax
	movq	120(%rax), %rax
.LBB9_3:                                # %cond.end
	movl	16(%rax), %ecx
	orq	$262144, %rcx           # imm = 0x40000
	movl	%ecx, 16(%rax)
	jmp	.LBB9_5
.LBB9_4:                                # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB9_5:                                # %if.end
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp80:
	.size	handle_used_attribute, .Ltmp80-handle_used_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_unused_attribute,@function
handle_unused_attribute:                # @handle_unused_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 32
.Ltmp88:
	.cfi_offset %rbx, -32
.Ltmp89:
	.cfi_offset %r14, -24
.Ltmp90:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rax
	movl	16(%rax), %edi
	movq	%rdi, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %ebx
	movq	%r8, %r14
	cmpq	$100, %rbx
	jne	.LBB10_4
# BB#1:                                 # %if.then
	cmpq	$35, %rdx
	ja	.LBB10_3
# BB#2:                                 # %if.then
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	movabsq	$63350767616, %rcx      # imm = 0xEC0000000
	testq	%rcx, %rbx
	jne	.LBB10_6
.LBB10_3:                               # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
	jmp	.LBB10_7
.LBB10_4:                               # %if.else39
	testq	$8, %rcx
	jne	.LBB10_6
# BB#5:                                 # %if.then40
	movq	%rax, %rdi
	callq	build_type_copy
	movq	%rax, (%r15)
	movl	16(%rax), %edi
.LBB10_6:                               # %if.end41
	orq	$65536, %rdi            # imm = 0x10000
	movl	%edi, 16(%rax)
.LBB10_7:                               # %if.end47
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp91:
	.size	handle_unused_attribute, .Ltmp91-handle_unused_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_const_attribute,@function
handle_const_attribute:                 # @handle_const_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp95:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp96:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp97:
	.cfi_def_cfa_offset 32
.Ltmp98:
	.cfi_offset %rbx, -24
.Ltmp99:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%r8, %r14
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$30, %rdx
	jne	.LBB11_2
# BB#1:                                 # %if.then
	orq	$4096, %rcx             # imm = 0x1000
	movl	%ecx, 16(%rax)
	jmp	.LBB11_6
.LBB11_2:                               # %if.else
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB11_5
# BB#3:                                 # %land.lhs.true
	movq	8(%rax), %rdi
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	jne	.LBB11_5
# BB#4:                                 # %if.then18
	movq	$10, %rcx
	shrq	%cl, %rax
	andq	$2, %rax
	orq	$1, %rax
	movq	%rax, %rsi
	callq	build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB11_6
.LBB11_5:                               # %if.else29
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB11_6:                               # %if.end30
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp100:
	.size	handle_const_attribute, .Ltmp100-handle_const_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_transparent_union_attribute,@function
handle_transparent_union_attribute:     # @handle_transparent_union_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp105:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp107:
	.cfi_def_cfa_offset 32
.Ltmp108:
	.cfi_offset %rbx, -32
.Ltmp109:
	.cfi_offset %r14, -24
.Ltmp110:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %ebx
	movq	%r8, %r14
	movq	%rcx, %r8
	cmpq	$116, %rbx
	jne	.LBB12_2
# BB#1:
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	jmp	.LBB12_5
.LBB12_2:                               # %entry
	cmpq	$100, %rbx
	jne	.LBB12_17
# BB#3:                                 # %if.end20
	leaq	8(%rdi), %r15
	cmpq	$33, %rax
	jne	.LBB12_12
# BB#4:                                 # %if.end20.land.lhs.true_crit_edge
	movq	(%r15), %rax
	movl	16(%rax), %edx
.LBB12_5:                               # %land.lhs.true
	andq	$255, %rdx
	cmpq	$21, %rdx
	jne	.LBB12_12
# BB#6:                                 # %land.lhs.true27
	testq	%rdi, %rdi
	je	.LBB12_9
# BB#7:                                 # %lor.lhs.false
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB12_13
# BB#8:                                 # %land.lhs.true33
	movl	60(%rax), %edx
	movzbl	48(%rcx), %ebx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB12_13
.LBB12_9:                               # %if.then45
	testq	$8, %r8
	jne	.LBB12_11
# BB#10:                                # %if.then47
	movq	%rax, %rdi
	callq	build_type_copy
	movq	%rax, (%r15)
.LBB12_11:                              # %if.end48
	movl	60(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 60(%rax)
	jmp	.LBB12_18
.LBB12_12:                              # %if.else52
	testq	%rdi, %rdi
	je	.LBB12_17
.LBB12_13:                              # %land.lhs.true55
	movzbl	16(%rdi), %eax
	cmpq	$35, %rax
	jne	.LBB12_17
# BB#14:                                # %land.lhs.true62
	movq	(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB12_17
# BB#15:                                # %land.lhs.true69
	movq	24(%rax), %rcx
	movl	60(%rax), %eax
	movzbl	48(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	%rdx, %rax
	jne	.LBB12_17
# BB#16:                                # %if.then84
	orq	$524288, 48(%rdi)       # imm = 0x80000
	jmp	.LBB12_18
.LBB12_17:                              # %if.else89
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB12_18:                              # %if.end91
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp111:
	.size	handle_transparent_union_attribute, .Ltmp111-handle_transparent_union_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_constructor_attribute,@function
handle_constructor_attribute:           # @handle_constructor_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp116:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp117:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 32
.Ltmp119:
	.cfi_offset %rbx, -32
.Ltmp120:
	.cfi_offset %r14, -24
.Ltmp121:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movzbl	16(%rbx), %eax
	movq	%r8, %r14
	movq	%rsi, %r15
	cmpq	$30, %rax
	jne	.LBB13_4
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB13_4
# BB#2:                                 # %land.lhs.true8
	movq	%rbx, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB13_3
.LBB13_4:                               # %if.else
	movq	32(%r15), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB13_5:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB13_3:                               # %if.then
	orq	$1048576, 48(%rbx)      # imm = 0x100000
	movl	16(%rbx), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	jmp	.LBB13_5
.Ltmp122:
	.size	handle_constructor_attribute, .Ltmp122-handle_constructor_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_destructor_attribute,@function
handle_destructor_attribute:            # @handle_destructor_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp127:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp128:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp129:
	.cfi_def_cfa_offset 32
.Ltmp130:
	.cfi_offset %rbx, -32
.Ltmp131:
	.cfi_offset %r14, -24
.Ltmp132:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movzbl	16(%rbx), %eax
	movq	%r8, %r14
	movq	%rsi, %r15
	cmpq	$30, %rax
	jne	.LBB14_4
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB14_4
# BB#2:                                 # %land.lhs.true8
	movq	%rbx, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB14_3
.LBB14_4:                               # %if.else
	movq	32(%r15), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%r14)
.LBB14_5:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB14_3:                               # %if.then
	orq	$2097152, 48(%rbx)      # imm = 0x200000
	movl	16(%rbx), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	jmp	.LBB14_5
.Ltmp133:
	.size	handle_destructor_attribute, .Ltmp133-handle_destructor_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_mode_attribute,@function
handle_mode_attribute:                  # @handle_mode_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp137:
	.cfi_def_cfa_offset 16
.Ltmp138:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp139:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp140:
	.cfi_offset %rbx, -56
.Ltmp141:
	.cfi_offset %r12, -48
.Ltmp142:
	.cfi_offset %r13, -40
.Ltmp143:
	.cfi_offset %r14, -32
.Ltmp144:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movq	(%r14), %rdi
	movq	$1, %rax
	movb	%al, (%r8)
	movq	32(%rdx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB15_1
# BB#2:                                 # %if.else
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	32(%rax), %r12
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB15_8
# BB#3:                                 # %land.lhs.true
	movzbl	(%r12), %eax
	cmpq	$95, %rax
	jne	.LBB15_8
# BB#4:                                 # %land.lhs.true11
	movzbl	1(%r12), %eax
	cmpq	$95, %rax
	jne	.LBB15_8
# BB#5:                                 # %land.lhs.true16
	movq	$32, %rcx
	shlq	%cl, %r15
	movabsq	$-4294967296, %rax      # imm = 0xFFFFFFFF00000000
	addq	%r15, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%r12,%rax), %ecx
	cmpq	$95, %rcx
	jne	.LBB15_8
# BB#6:                                 # %land.lhs.true21
	movabsq	$-8589934592, %rdx      # imm = 0xFFFFFFFE00000000
	addq	%r15, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%r12,%rdx), %ecx
	cmpq	$95, %rcx
	jne	.LBB15_8
# BB#7:                                 # %if.then28
	movq	%rsp, %rbx
	addq	$15, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	addq	$2, %r12
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	strcpy
	movabsq	$-17179869184, %rax     # imm = 0xFFFFFFFC00000000
	addq	%rax, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rax, %rax
	movb	%al, (%r15,%rbx)
	movq	%rbx, %r12
.LBB15_8:                               # %if.end
	movabsq	$.L.str44, %rsi
	movq	%r12, %rdi
	callq	strcmp
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB15_9
# BB#10:                                # %if.else38
	movabsq	$.L.str45, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_11
# BB#12:                                # %if.else42
	movabsq	$.L.str46, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB15_24
# BB#13:
	xorq	%rbx, %rbx
	xorq	%r13, %r13
	.align	16, 0x90
.LBB15_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	mode_name(,%rbx,8), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r15, %rax
	movq	%rbx, %rdi
	je	.LBB15_16
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	%r13, %rdi
.LBB15_16:                              # %for.body
                                        #   in Loop: Header=BB15_14 Depth=1
	incq	%rbx
	cmpq	$59, %rbx
	movq	%rdi, %r13
	jne	.LBB15_14
	jmp	.LBB15_17
.LBB15_1:                               # %if.then
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB15_23
.LBB15_9:                               # %if.then37
	movl	byte_mode(%rip), %edi
	jmp	.LBB15_17
.LBB15_11:                              # %if.then41
	movl	word_mode(%rip), %edi
	jmp	.LBB15_17
.LBB15_24:                              # %if.then45
	movl	ptr_mode(%rip), %edi
.LBB15_17:                              # %if.end57
	testq	%r15, %rdi
	je	.LBB15_18
# BB#20:                                # %if.else61
	movq	-48(%rbp), %rax         # 8-byte Reload
	movl	16(%rax), %esi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_mode
	testq	%rax, %rax
	je	.LBB15_21
# BB#22:                                # %if.else69
	movq	%rax, (%r14)
	jmp	.LBB15_23
.LBB15_18:                              # %if.then60
	movabsq	$.L.str47, %rdi
	jmp	.LBB15_19
.LBB15_21:                              # %if.then68
	movabsq	$.L.str48, %rdi
.LBB15_19:                              # %if.end72
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	error
.LBB15_23:                              # %if.end72
	xorq	%rax, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp145:
	.size	handle_mode_attribute, .Ltmp145-handle_mode_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_section_attribute,@function
handle_section_attribute:               # @handle_section_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp150:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp151:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp152:
	.cfi_def_cfa_offset 32
.Ltmp153:
	.cfi_offset %rbx, -32
.Ltmp154:
	.cfi_offset %r14, -24
.Ltmp155:
	.cfi_offset %r15, -16
	movzbl	targetm+328(%rip), %eax
	movq	(%rdi), %rbx
	movq	%r8, %r14
	testq	%rax, %rax
	je	.LBB16_13
# BB#1:                                 # %if.then
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB16_3
# BB#2:                                 # %if.then
	cmpq	$34, %rcx
	jne	.LBB16_12
.LBB16_3:                               # %land.lhs.true
	movq	32(%rdx), %r15
	movzbl	16(%r15), %edx
	cmpq	$29, %rdx
	jne	.LBB16_12
# BB#4:                                 # %if.then11
	cmpq	$34, %rcx
	jne	.LBB16_8
# BB#5:                                 # %if.then11
	movq	current_function_decl(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB16_8
# BB#6:                                 # %if.then11
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB16_8
# BB#7:                                 # %if.then24
	movabsq	$.L.str40, %rsi
	jmp	.LBB16_14
.LBB16_12:                              # %if.else42
	movabsq	$.L.str42, %rsi
	jmp	.LBB16_14
.LBB16_13:                              # %if.else44
	movabsq	$.L.str43, %rsi
.LBB16_14:                              # %if.end45
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	error_with_decl
	movq	$1, %rax
	movb	%al, (%r14)
.LBB16_15:                              # %if.end45
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB16_8:                               # %if.else
	movq	128(%rbx), %rax
	testq	%rax, %rax
	je	.LBB16_11
# BB#9:                                 # %land.lhs.true27
	movq	40(%rax), %rdi
	movq	40(%r15), %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_11
# BB#10:                                # %if.then35
	movabsq	$.L.str41, %rsi
	jmp	.LBB16_14
.LBB16_11:                              # %if.else36
	movq	%r15, 128(%rbx)
	jmp	.LBB16_15
.Ltmp156:
	.size	handle_section_attribute, .Ltmp156-handle_section_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_aligned_attribute,@function
handle_aligned_attribute:               # @handle_aligned_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp163:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp164:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp165:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp166:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp168:
	.cfi_def_cfa_offset 64
.Ltmp169:
	.cfi_offset %rbx, -48
.Ltmp170:
	.cfi_offset %r12, -40
.Ltmp171:
	.cfi_offset %r13, -32
.Ltmp172:
	.cfi_offset %r14, -24
.Ltmp173:
	.cfi_offset %r15, -16
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdi, %r15
	testq	%rdx, %rdx
	je	.LBB17_2
# BB#1:                                 # %cond.true
	movq	32(%rdx), %rdi
	jmp	.LBB17_3
.LBB17_2:                               # %cond.false
	movq	$16, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, %rdi
.LBB17_3:                               # %cond.end
	movq	(%r15), %rbx
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %ecx
	xorq	%r12, %r12
	cmpq	$116, %rcx
	jne	.LBB17_4
# BB#9:                                 # %if.then19
	movq	$1, %r12
	jmp	.LBB17_10
.LBB17_4:                               # %cond.end
	cmpq	$100, %rcx
	jne	.LBB17_5
# BB#6:                                 # %if.then
	leaq	8(%rbx), %r15
	cmpq	$33, %rax
	jne	.LBB17_7
# BB#8:                                 # %if.then
	movabsq	$1, %r12
	jmp	.LBB17_11
.LBB17_5:
	xorq	%r15, %r15
.LBB17_10:                              # %while.cond.preheader
	xorq	%rbx, %rbx
.LBB17_11:                              # %while.cond.preheader
	movzbl	16(%rdi), %eax
	leaq	-114(%rax), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB17_12
	.align	16, 0x90
.LBB17_34:                              # %while.body
                                        #   in Loop: Header=BB17_12 Depth=1
	movq	32(%rdi), %rdi
	movzbl	16(%rdi), %eax
	leaq	-114(%rax), %rcx
.LBB17_12:                              # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	andq	%r14, %rcx
	cmpq	$3, %rcx
	jb	.LBB17_34
# BB#13:                                # %lor.rhs
	cmpq	$25, %rax
	jne	.LBB17_14
# BB#17:                                # %if.else47
	callq	tree_log2
	movq	%rax, %r13
                                        # kill: RAX<def> R13<kill>
	andq	%r14, %rax
	cmpq	%r14, %rax
	je	.LBB17_18
# BB#19:                                # %if.else52
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$31, %rax
	jl	.LBB17_21
# BB#20:                                # %if.then55
	movabsq	$.L.str38, %rdi
	jmp	.LBB17_15
.LBB17_14:                              # %if.then46
	movabsq	$.L.str36, %rdi
	jmp	.LBB17_15
.LBB17_18:                              # %if.then51
	movabsq	$.L.str37, %rdi
.LBB17_15:                              # %if.end128
	xorq	%rax, %rax
	callq	error
.LBB17_16:                              # %if.end128
	movq	$1, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movb	%al, (%rcx)
.LBB17_33:                              # %if.end128
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_21:                              # %if.else56
	testq	%r12, %r12
	je	.LBB17_29
# BB#22:                                # %if.then58
	testq	%rbx, %rbx
	je	.LBB17_26
# BB#23:                                # %land.lhs.true
	movq	8(%rbx), %r14
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB17_26
# BB#24:                                # %land.lhs.true64
	cmpq	$0, 96(%rbx)
	je	.LBB17_25
.LBB17_26:                              # %if.else85
	testq	$8, (%rsp)              # 8-byte Folded Reload
	jne	.LBB17_28
# BB#27:                                # %if.then87
	movq	(%r15), %rdi
	callq	build_type_copy
	movq	%rax, (%r15)
	jmp	.LBB17_28
.LBB17_7:
	movabsq	$0, %r12
	jmp	.LBB17_11
.LBB17_29:                              # %if.else96
	movzbl	16(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB17_32
# BB#30:                                # %if.else96
	cmpq	$37, %rax
	jne	.LBB17_31
.LBB17_32:                              # %if.else111
	movq	$8, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	movl	56(%rbx), %ecx
	andq	$16777215, %rax         # imm = 0xFFFFFF
	movabsq	$4278190080, %rdx       # imm = 0xFF000000
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 56(%rbx)
	movabsq	$34359738368, %rax      # imm = 0x800000000
	orq	%rax, 48(%rbx)
	jmp	.LBB17_33
.LBB17_31:                              # %if.then110
	movabsq	$.L.str39, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	error_with_decl
	jmp	.LBB17_16
.LBB17_25:                              # %if.then68
	movq	(%r15), %rdi
	callq	build_type_copy
	movq	%rax, (%r15)
	movq	%r14, 96(%rbx)
	movq	(%r15), %rax
	movq	%rbx, 96(%rax)
	movq	(%r15), %rax
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$65536, %rcx            # imm = 0x10000
	movabsq	$4294901759, %rsi       # imm = 0xFFFEFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	(%r15), %rax
	movq	%rax, 8(%rbx)
.LBB17_28:                              # %if.end90
	movq	(%r15), %rax
	movq	$8, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	movl	%edx, 64(%rax)
	movl	60(%rax), %ecx
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	orq	%rcx, %rdx
	movl	%edx, 60(%rax)
	jmp	.LBB17_33
.Ltmp174:
	.size	handle_aligned_attribute, .Ltmp174-handle_aligned_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_weak_attribute,@function
handle_weak_attribute:                  # @handle_weak_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp176:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	declare_weak
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp177:
	.size	handle_weak_attribute, .Ltmp177-handle_weak_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_alias_attribute,@function
handle_alias_attribute:                 # @handle_alias_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp183:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp185:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp186:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp187:
	.cfi_def_cfa_offset 48
.Ltmp188:
	.cfi_offset %rbx, -40
.Ltmp189:
	.cfi_offset %r12, -32
.Ltmp190:
	.cfi_offset %r14, -24
.Ltmp191:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movzbl	16(%rbx), %eax
	movq	%r8, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	cmpq	$30, %rax
	jne	.LBB19_5
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 104(%rbx)
	jne	.LBB19_6
	jmp	.LBB19_2
.LBB19_5:                               # %land.lhs.true7
	movzbl	49(%rbx), %eax
	testq	$1, %rax
	je	.LBB19_6
.LBB19_2:                               # %if.else
	movq	%rbx, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB19_3
# BB#10:                                # %if.else38
	movq	32(%r12), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB19_11
.LBB19_6:                               # %if.then
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	error_with_decl
.LBB19_11:                              # %return
	movq	$1, %rax
	movb	%al, (%r14)
.LBB19_12:                              # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB19_3:                               # %if.then13
	movq	32(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$29, %rcx
	jne	.LBB19_4
# BB#7:                                 # %if.end
	movq	40(%rax), %rdi
	callq	get_identifier
	movl	16(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB19_9
# BB#8:                                 # %if.then29
	movq	global_trees(%rip), %rax
	movq	%rax, 104(%rbx)
	jmp	.LBB19_12
.LBB19_4:                               # %if.then19
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB19_11
.LBB19_9:                               # %if.else32
	andq	$-257, 48(%rbx)         # imm = 0xFFFFFFFFFFFFFEFF
	jmp	.LBB19_12
.Ltmp192:
	.size	handle_alias_attribute, .Ltmp192-handle_alias_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_no_instrument_function_attribute,@function
handle_no_instrument_function_attribute: # @handle_no_instrument_function_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 16
.Ltmp196:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rdi
	movzbl	16(%rdi), %eax
	movq	%r8, %rbx
	cmpq	$30, %rax
	jne	.LBB20_1
# BB#3:                                 # %if.else
	cmpq	$0, 104(%rdi)
	je	.LBB20_5
# BB#4:                                 # %if.then2
	movq	32(%rsi), %rdx
	movabsq	$.L.str33, %rsi
	jmp	.LBB20_2
.LBB20_1:                               # %if.then
	movq	32(%rsi), %rdx
	movabsq	$.L.str32, %rsi
.LBB20_2:                               # %if.end10
	xorq	%rax, %rax
	callq	error_with_decl
	movq	$1, %rax
	movb	%al, (%rbx)
	jmp	.LBB20_6
.LBB20_5:                               # %if.else6
	orq	$33554432, 48(%rdi)     # imm = 0x2000000
.LBB20_6:                               # %if.end10
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp197:
	.size	handle_no_instrument_function_attribute, .Ltmp197-handle_no_instrument_function_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_malloc_attribute,@function
handle_malloc_attribute:                # @handle_malloc_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 16
.Ltmp201:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%r8, %rbx
	cmpq	$30, %rcx
	jne	.LBB21_2
# BB#1:                                 # %if.then
	orq	$134217728, 48(%rax)    # imm = 0x8000000
	jmp	.LBB21_3
.LBB21_2:                               # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB21_3:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp202:
	.size	handle_malloc_attribute, .Ltmp202-handle_malloc_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_no_limit_stack_attribute,@function
handle_no_limit_stack_attribute:        # @handle_no_limit_stack_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp205:
	.cfi_def_cfa_offset 16
.Ltmp206:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rdi
	movzbl	16(%rdi), %eax
	movq	%r8, %rbx
	cmpq	$30, %rax
	jne	.LBB22_1
# BB#3:                                 # %if.else
	cmpq	$0, 104(%rdi)
	je	.LBB22_5
# BB#4:                                 # %if.then2
	movq	32(%rsi), %rdx
	movabsq	$.L.str33, %rsi
	jmp	.LBB22_2
.LBB22_1:                               # %if.then
	movq	32(%rsi), %rdx
	movabsq	$.L.str32, %rsi
.LBB22_2:                               # %if.end10
	xorq	%rax, %rax
	callq	error_with_decl
	movq	$1, %rax
	movb	%al, (%rbx)
	jmp	.LBB22_6
.LBB22_5:                               # %if.else6
	orq	$268435456, 48(%rdi)    # imm = 0x10000000
.LBB22_6:                               # %if.end10
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp207:
	.size	handle_no_limit_stack_attribute, .Ltmp207-handle_no_limit_stack_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_pure_attribute,@function
handle_pure_attribute:                  # @handle_pure_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp210:
	.cfi_def_cfa_offset 16
.Ltmp211:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%r8, %rbx
	cmpq	$30, %rcx
	jne	.LBB23_2
# BB#1:                                 # %if.then
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rcx, 48(%rax)
	jmp	.LBB23_3
.LBB23_2:                               # %if.else
	movq	32(%rsi), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB23_3:                               # %if.end
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp212:
	.size	handle_pure_attribute, .Ltmp212-handle_pure_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_deprecated_attribute,@function
handle_deprecated_attribute:            # @handle_deprecated_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp216:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp217:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp218:
	.cfi_def_cfa_offset 32
.Ltmp219:
	.cfi_offset %rbx, -24
.Ltmp220:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movl	16(%rax), %edi
	movq	%rdi, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %ebx
	cmpq	$100, %rbx
	jne	.LBB24_1
# BB#3:                                 # %if.then
	movq	8(%rax), %r9
	cmpq	$37, %rdx
	ja	.LBB24_8
# BB#4:                                 # %if.then
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	movabsq	$198642237440, %rcx     # imm = 0x2E40000000
	testq	%rcx, %rbx
	jne	.LBB24_5
.LBB24_8:                               # %if.then63
	movq	$1, %rcx
	movb	%cl, (%r8)
	testq	%r9, %r9
	je	.LBB24_16
# BB#9:                                 # %land.lhs.true
	movq	96(%r9), %rcx
	testq	%rcx, %rcx
	je	.LBB24_16
# BB#10:                                # %if.then68
	movzbl	16(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB24_11
# BB#15:                                # %land.lhs.true89
	movq	72(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB24_13
	jmp	.LBB24_16
.LBB24_1:                               # %entry
	cmpq	$116, %rbx
	jne	.LBB24_2
# BB#6:                                 # %if.then51
	testq	$8, %rcx
	jne	.LBB24_5
# BB#7:                                 # %if.then52
	movq	%rax, %rdi
	callq	build_type_copy
	movq	%rax, (%r14)
	movl	16(%rax), %edi
.LBB24_5:                               # %if.then37
	orq	$8388608, %rdi          # imm = 0x800000
	movl	%edi, 16(%rax)
	jmp	.LBB24_18
.LBB24_2:                               # %if.then63.thread
	movq	$1, %rax
	movb	%al, (%r8)
.LBB24_16:                              # %if.end105.thread
	movq	32(%rsi), %rsi
.LBB24_17:                              # %if.else111
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB24_18:                              # %if.end116
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB24_11:                              # %if.then68
	cmpq	$1, %rdx
	jne	.LBB24_16
# BB#12:                                # %if.then77
	movq	96(%rax), %rax
.LBB24_13:                              # %if.end105
	movq	32(%rax), %rdx
	movq	32(%rsi), %rsi
	testq	%rdx, %rdx
	je	.LBB24_17
# BB#14:                                # %if.then107
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB24_18
.Ltmp221:
	.size	handle_deprecated_attribute, .Ltmp221-handle_deprecated_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	handle_vector_size_attribute,@function
handle_vector_size_attribute:           # @handle_vector_size_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp230:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp233:
	.cfi_def_cfa_offset 64
.Ltmp234:
	.cfi_offset %rbx, -48
.Ltmp235:
	.cfi_offset %r12, -40
.Ltmp236:
	.cfi_offset %r13, -32
.Ltmp237:
	.cfi_offset %r14, -24
.Ltmp238:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rdi, %r13
	movq	(%r13), %rbx
	movq	$1, %rax
	movb	%al, (%r8)
	movq	32(%r12), %rdi
	movq	%rsi, %r15
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB25_1
# BB#2:                                 # %if.end
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	32(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r12
	jmp	.LBB25_3
	.align	16, 0x90
.LBB25_24:                              # %while.body
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	8(%rbx), %rbx
.LBB25_3:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$23, %rcx
	ja	.LBB25_6
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$8691712, %rax          # imm = 0x84A000
	jne	.LBB25_24
# BB#5:                                 # %while.cond
	cmpq	$20, %rcx
	je	.LBB25_8
.LBB25_6:                               # %lor.lhs.false31
	movl	60(%rbx), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %r13d
	leaq	-1(%r13), %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB25_8
# BB#7:                                 # %lor.lhs.false36
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r14, %rax
	je	.LBB25_8
# BB#9:                                 # %if.end44
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rcx
	movq	%rax, %r8
	andq	%r14, %r13
	cmpq	$1, %r13
	je	.LBB25_10
# BB#11:                                # %if.end44
	movabsq	$1, %rax
	jmp	.LBB25_12
.LBB25_8:                               # %if.then40
	movq	32(%r15), %rsi
	movabsq	$.L.str29, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB25_23
.LBB25_1:                               # %if.then
	movq	32(%r15), %rsi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB25_23
.LBB25_10:
	movabsq	$0, %rax
.LBB25_12:                              # %if.end44
	movq	8(%rsp), %r15           # 8-byte Reload
	movl	class_narrowest_mode+28(,%rax,4), %edi
	jmp	.LBB25_13
	.align	16, 0x90
.LBB25_18:                              # %for.inc
                                        #   in Loop: Header=BB25_13 Depth=1
	movzbl	mode_wider_mode(%rsi), %edi
.LBB25_13:                              # %if.end44
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB25_20
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB25_13 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	cmpq	%rax, %r12
	jne	.LBB25_18
# BB#15:                                # %land.lhs.true58
                                        #   in Loop: Header=BB25_13 Depth=1
	movzbl	mode_unit_size(%rsi), %ecx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB25_17
# BB#16:                                # %cond.false
                                        #   in Loop: Header=BB25_13 Depth=1
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %rdx
.LBB25_17:                              # %cond.end
                                        #   in Loop: Header=BB25_13 Depth=1
	cmpq	%rdx, %r8
	jne	.LBB25_18
# BB#19:                                # %for.end
	testq	%r14, %rdi
	je	.LBB25_20
# BB#21:                                # %if.else
	movl	16(%rbx), %esi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_mode
	testq	%rax, %rax
	je	.LBB25_20
# BB#22:                                # %if.else90
	movq	(%r15), %rdi
	movq	%rax, %rsi
	callq	vector_size_helper
	movq	%rax, (%r15)
	jmp	.LBB25_23
.LBB25_20:                              # %if.then82
	movabsq	$.L.str30, %rdi
	xorq	%rax, %rax
	callq	error
.LBB25_23:                              # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp239:
	.size	handle_vector_size_attribute, .Ltmp239-handle_vector_size_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	vector_size_helper,@function
vector_size_helper:                     # @vector_size_helper
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp242:
	.cfi_def_cfa_offset 16
.Ltmp243:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$22, %rax
	jg	.LBB26_5
# BB#1:                                 # %entry
	cmpq	$13, %rax
	je	.LBB26_7
# BB#2:                                 # %entry
	cmpq	$15, %rax
	jne	.LBB26_3
.LBB26_7:                               # %if.then
	movq	8(%rbx), %rdi
	callq	vector_size_helper
	movq	%rax, %rdi
	callq	build_pointer_type
	jmp	.LBB26_8
.LBB26_5:                               # %entry
	cmpq	$23, %rax
	jne	.LBB26_9
# BB#6:                                 # %if.then26
	movq	8(%rbx), %rdi
	callq	vector_size_helper
	movq	24(%rbx), %rsi
	movq	%rax, %rdi
	callq	build_function_type
	jmp	.LBB26_8
.LBB26_3:                               # %entry
	cmpq	$18, %rax
	jne	.LBB26_9
# BB#4:                                 # %if.then14
	movq	8(%rbx), %rdi
	callq	vector_size_helper
	movq	24(%rbx), %rsi
	movq	%rax, %rdi
	callq	build_array_type
.LBB26_8:                               # %if.end35
	movq	%rax, %rsi
	movl	16(%rbx), %eax
	movl	16(%rsi), %ecx
	andq	$4096, %rax             # imm = 0x1000
	andq	$-4097, %rcx            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rax, %rcx
	movl	%ecx, 16(%rsi)
	movl	16(%rbx), %eax
	andq	$2048, %rax             # imm = 0x800
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%rsi)
.LBB26_9:                               # %return
	movq	%rsi, %rax
	popq	%rbx
	retq
.Ltmp244:
	.size	vector_size_helper, .Ltmp244-vector_size_helper
	.cfi_endproc

	.type	empty_attribute_table,@object # @empty_attribute_table
	.section	.rodata,"a",@progbits
	.align	16
empty_attribute_table:
	.zero	32
	.size	empty_attribute_table, 32

	.type	format_attribute_table,@object # @format_attribute_table
	.data
	.globl	format_attribute_table
	.align	8
format_attribute_table:
	.quad	empty_attribute_table
	.size	format_attribute_table, 8

	.type	lang_attribute_table,@object # @lang_attribute_table
	.globl	lang_attribute_table
	.align	8
lang_attribute_table:
	.quad	empty_attribute_table
	.size	lang_attribute_table, 8

	.type	lang_attribute_common,@object # @lang_attribute_common
	.globl	lang_attribute_common
	.align	4
lang_attribute_common:
	.long	1                       # 0x1
	.size	lang_attribute_common, 4

	.type	attributes_initialized,@object # @attributes_initialized
	.local	attributes_initialized
	.comm	attributes_initialized,1,1
	.type	attribute_tables,@object # @attribute_tables
	.local	attribute_tables
	.comm	attribute_tables,32,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"`%s' attribute directive ignored"
	.size	.L.str, 33

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"wrong number of arguments specified for `%s' attribute"
	.size	.L.str1, 55

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"`%s' attribute does not apply to types"
	.size	.L.str2, 39

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"`%s' attribute only applies to function types"
	.size	.L.str3, 46

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"`%s' attribute ignored"
	.size	.L.str4, 23

	.type	c_common_attribute_table,@object # @c_common_attribute_table
	.section	.rodata,"a",@progbits
	.align	16
c_common_attribute_table:
	.quad	.L.str5
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_packed_attribute
	.quad	.L.str6
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_nocommon_attribute
	.quad	.L.str7
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_common_attribute
	.quad	.L.str8
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_noreturn_attribute
	.quad	.L.str9
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_noreturn_attribute
	.quad	.L.str10
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_noinline_attribute
	.quad	.L.str11
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_always_inline_attribute
	.quad	.L.str12
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_used_attribute
	.quad	.L.str13
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_unused_attribute
	.quad	.L.str14
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_const_attribute
	.quad	.L.str15
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_transparent_union_attribute
	.quad	.L.str16
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_constructor_attribute
	.quad	.L.str17
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_destructor_attribute
	.quad	.L.str18
	.long	1                       # 0x1
	.long	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_mode_attribute
	.quad	.L.str19
	.long	1                       # 0x1
	.long	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_section_attribute
	.quad	.L.str20
	.long	0                       # 0x0
	.long	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_aligned_attribute
	.quad	.L.str21
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_weak_attribute
	.quad	.L.str22
	.long	1                       # 0x1
	.long	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_alias_attribute
	.quad	.L.str23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_no_instrument_function_attribute
	.quad	.L.str24
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_malloc_attribute
	.quad	.L.str25
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_no_limit_stack_attribute
	.quad	.L.str26
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_pure_attribute
	.quad	.L.str27
	.long	0                       # 0x0
	.long	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_deprecated_attribute
	.quad	.L.str28
	.long	1                       # 0x1
	.long	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.zero	5
	.quad	handle_vector_size_attribute
	.zero	32
	.size	c_common_attribute_table, 800

	.type	.L.str5,@object         # @.str5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str5:
	.asciz	"packed"
	.size	.L.str5, 7

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"nocommon"
	.size	.L.str6, 9

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"common"
	.size	.L.str7, 7

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"noreturn"
	.size	.L.str8, 9

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"volatile"
	.size	.L.str9, 9

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"noinline"
	.size	.L.str10, 9

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"always_inline"
	.size	.L.str11, 14

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"used"
	.size	.L.str12, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"unused"
	.size	.L.str13, 7

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"const"
	.size	.L.str14, 6

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"transparent_union"
	.size	.L.str15, 18

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"constructor"
	.size	.L.str16, 12

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"destructor"
	.size	.L.str17, 11

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"mode"
	.size	.L.str18, 5

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"section"
	.size	.L.str19, 8

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"aligned"
	.size	.L.str20, 8

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"weak"
	.size	.L.str21, 5

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"alias"
	.size	.L.str22, 6

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"no_instrument_function"
	.size	.L.str23, 23

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"malloc"
	.size	.L.str24, 7

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"no_stack_limit"
	.size	.L.str25, 15

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"pure"
	.size	.L.str26, 5

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"deprecated"
	.size	.L.str27, 11

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"vector_size"
	.size	.L.str28, 12

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"invalid vector type for attribute `%s'"
	.size	.L.str29, 39

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"no vector mode with the size and type specified could be found"
	.size	.L.str30, 63

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"`%s' attribute ignored for `%s'"
	.size	.L.str31, 32

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"`%s' attribute applies only to functions"
	.size	.L.str32, 41

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"can't set `%s' attribute after definition"
	.size	.L.str33, 42

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"`%s' defined both normally and as an alias"
	.size	.L.str34, 43

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"alias arg not a string"
	.size	.L.str35, 23

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"requested alignment is not a constant"
	.size	.L.str36, 38

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"requested alignment is not a power of 2"
	.size	.L.str37, 40

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"requested alignment is too large"
	.size	.L.str38, 33

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"alignment may not be specified for `%s'"
	.size	.L.str39, 40

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"section attribute cannot be specified for local variables"
	.size	.L.str40, 58

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"section of `%s' conflicts with previous declaration"
	.size	.L.str41, 52

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"section attribute not allowed for `%s'"
	.size	.L.str42, 39

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"section attributes are not supported for this target"
	.size	.L.str43, 53

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"byte"
	.size	.L.str44, 5

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"word"
	.size	.L.str45, 5

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"pointer"
	.size	.L.str46, 8

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"unknown machine mode `%s'"
	.size	.L.str47, 26

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"no data type for mode `%s'"
	.size	.L.str48, 27


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
