	.file	"ASCIIRangeFactory.cpp.bc"
	.text
	.globl	_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev
	.align	16, 0x90
	.type	_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev,@function
_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev: # @_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	_ZN11xercesc_2_512RangeFactoryC2Ev
	movq	$_ZTVN11xercesc_2_517ASCIIRangeFactoryE+16, (%rbx)
	xorq	%rax, %rax
	movw	%ax, 8(%rbx)
	popq	%rbx
	retq
.Ltmp4:
	.size	_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev, .Ltmp4-_ZN11xercesc_2_517ASCIIRangeFactoryC2Ev
	.cfi_endproc

	.globl	_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev
	.align	16, 0x90
	.type	_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev,@function
_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev: # @_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	_ZN11xercesc_2_512RangeFactoryD2Ev  # TAILCALL
.Ltmp5:
	.size	_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev, .Ltmp5-_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev
	.cfi_endproc

	.globl	_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev
	.align	16, 0x90
	.type	_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev,@function
_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev: # @_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev
	.cfi_startproc
	.cfi_personality 0, __gxx_personality_v0
.Leh_func_begin2:
	.cfi_lsda 0, .Lexception2
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp17:
	.cfi_def_cfa_offset 32
.Ltmp18:
	.cfi_offset %rbx, -24
.Ltmp19:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
.Ltmp6:
                                        # kill: RDI<def> RBX<kill>
	callq	_ZN11xercesc_2_512RangeFactoryD2Ev
.Ltmp7:
# BB#1:                                 # %invoke.cont
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	_ZN11xercesc_2_57XMemorydlEPv  # TAILCALL
.LBB2_2:                                # %lpad
.Ltmp8:
	movq	%rax, %r14
.Ltmp9:
	movq	%rbx, %rdi
	callq	_ZN11xercesc_2_57XMemorydlEPv
.Ltmp10:
# BB#3:                                 # %eh.resume
	movq	%r14, %rdi
	callq	_Unwind_Resume
.LBB2_4:                                # %terminate.lpad
.Ltmp11:
	movq	%rax, %rdi
	callq	__clang_call_terminate
.Ltmp20:
	.size	_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev, .Ltmp20-_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev
	.cfi_endproc
.Leh_func_end2:
	.section	.gcc_except_table,"a",@progbits
	.align	4
GCC_except_table2:
.Lexception2:
	.byte	255                     # @LPStart Encoding = omit
	.byte	0                       # @TType Encoding = absptr
	.asciz	"\300"                  # @TType base offset
	.byte	3                       # Call site Encoding = udata4
	.byte	52                      # Call site table length
.Lset0 = .Ltmp6-.Leh_func_begin2        # >> Call Site 1 <<
	.long	.Lset0
.Lset1 = .Ltmp7-.Ltmp6                  #   Call between .Ltmp6 and .Ltmp7
	.long	.Lset1
.Lset2 = .Ltmp8-.Leh_func_begin2        #     jumps to .Ltmp8
	.long	.Lset2
	.byte	0                       #   On action: cleanup
.Lset3 = .Ltmp7-.Leh_func_begin2        # >> Call Site 2 <<
	.long	.Lset3
.Lset4 = .Ltmp9-.Ltmp7                  #   Call between .Ltmp7 and .Ltmp9
	.long	.Lset4
	.long	0                       #     has no landing pad
	.byte	0                       #   On action: cleanup
.Lset5 = .Ltmp9-.Leh_func_begin2        # >> Call Site 3 <<
	.long	.Lset5
.Lset6 = .Ltmp10-.Ltmp9                 #   Call between .Ltmp9 and .Ltmp10
	.long	.Lset6
.Lset7 = .Ltmp11-.Leh_func_begin2       #     jumps to .Ltmp11
	.long	.Lset7
	.byte	1                       #   On action: 1
.Lset8 = .Ltmp10-.Leh_func_begin2       # >> Call Site 4 <<
	.long	.Lset8
.Lset9 = .Leh_func_end2-.Ltmp10         #   Call between .Ltmp10 and .Leh_func_end2
	.long	.Lset9
	.long	0                       #     has no landing pad
	.byte	0                       #   On action: cleanup
	.byte	1                       # >> Action Record 1 <<
                                        #   Catch TypeInfo 1
	.byte	0                       #   No further actions
                                        # >> Catch TypeInfos <<
	.quad	0                       # TypeInfo 1
	.align	4

	.section	.text.__clang_call_terminate,"axG",@progbits,__clang_call_terminate,comdat
	.hidden	__clang_call_terminate
	.weak	__clang_call_terminate
	.align	16, 0x90
	.type	__clang_call_terminate,@function
__clang_call_terminate:                 # @__clang_call_terminate
# BB#0:
	pushq	%rax
	callq	__cxa_begin_catch
	callq	_ZSt9terminatev
.Ltmp21:
	.size	__clang_call_terminate, .Ltmp21-__clang_call_terminate

	.text
	.globl	_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv
	.align	16, 0x90
	.type	_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv,@function
_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv: # @_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp31:
	.cfi_def_cfa_offset 48
.Ltmp32:
	.cfi_offset %rbx, -40
.Ltmp33:
	.cfi_offset %r12, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	8(%r12), %eax
	testq	%rax, %rax
	jne	.LBB4_4
# BB#1:                                 # %if.end
	movzbl	9(%r12), %eax
	testq	%rax, %rax
	jne	.LBB4_3
# BB#2:                                 # %if.then3
	movq	(%r12), %rax
	movq	%r12, %rdi
	callq	*16(%rax)
.LBB4_3:                                # %if.end4
	callq	_ZN11xercesc_2_513RangeTokenMap8instanceEv
	movq	%rax, %r14
	movq	32(%r14), %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	_ZN11xercesc_2_512TokenFactory11createRangeEb
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	$9, %rsi
	movq	$9, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$10, %rsi
	movq	$10, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$12, %rsi
	movq	$12, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$13, %rsi
	movq	$13, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$32, %rsi
	movq	$32, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movabsq	$_ZN11xercesc_2_5L12fgASCIISpaceE, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13setRangeTokenEPKtPNS_10RangeTokenEb
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	_ZN11xercesc_2_512TokenFactory11createRangeEb
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	$48, %rsi
	movq	$57, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movabsq	$_ZN11xercesc_2_5L12fgASCIIDigitE, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13setRangeTokenEPKtPNS_10RangeTokenEb
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	_ZN11xercesc_2_512TokenFactory11createRangeEb
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	$48, %rsi
	movq	$57, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$65, %rsi
	movq	$90, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$95, %rsi
	movq	$95, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$97, %rsi
	movq	$122, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movabsq	$_ZN11xercesc_2_5L11fgASCIIWordE, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13setRangeTokenEPKtPNS_10RangeTokenEb
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	_ZN11xercesc_2_512TokenFactory11createRangeEb
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	$48, %rsi
	movq	$57, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$65, %rsi
	movq	$70, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movq	(%rbx), %rax
	movq	$97, %rsi
	movq	$97, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movabsq	$_ZN11xercesc_2_5L13fgASCIIXDigitE, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13setRangeTokenEPKtPNS_10RangeTokenEb
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	_ZN11xercesc_2_512TokenFactory11createRangeEb
	movq	%rax, %rbx
	movq	(%rbx), %rax
	xorq	%rsi, %rsi
	movq	$127, %rdx
	movq	%rbx, %rdi
	callq	*96(%rax)
	movabsq	$_ZN11xercesc_2_5L7fgASCIIE, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13setRangeTokenEPKtPNS_10RangeTokenEb
	movq	$1, %rax
	movb	%al, 8(%r12)
.LBB4_4:                                # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp36:
	.size	_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv, .Ltmp36-_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv
	.cfi_endproc

	.globl	_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv
	.align	16, 0x90
	.type	_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv,@function
_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv: # @_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp41:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 32
.Ltmp44:
	.cfi_offset %rbx, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	9(%r14), %eax
	testq	%rax, %rax
	jne	.LBB5_2
# BB#1:                                 # %if.end
	callq	_ZN11xercesc_2_513RangeTokenMap8instanceEv
	movq	%rax, %rbx
	movabsq	$_ZN11xercesc_2_5L12fgASCIISpaceE, %rsi
	movabsq	$_ZN11xercesc_2_5L15fgASCIICategoryE, %r15
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13addKeywordMapEPKtS2_
	movabsq	$_ZN11xercesc_2_5L12fgASCIIDigitE, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13addKeywordMapEPKtS2_
	movabsq	$_ZN11xercesc_2_5L11fgASCIIWordE, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13addKeywordMapEPKtS2_
	movabsq	$_ZN11xercesc_2_5L13fgASCIIXDigitE, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13addKeywordMapEPKtS2_
	movabsq	$_ZN11xercesc_2_5L7fgASCIIE, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	_ZN11xercesc_2_513RangeTokenMap13addKeywordMapEPKtS2_
	movq	$1, %rax
	movb	%al, 9(%r14)
.LBB5_2:                                # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp47:
	.size	_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv, .Ltmp47-_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv
	.cfi_endproc

	.type	_ZTVN11xercesc_2_517ASCIIRangeFactoryE,@object # @_ZTVN11xercesc_2_517ASCIIRangeFactoryE
	.section	.rodata,"a",@progbits
	.globl	_ZTVN11xercesc_2_517ASCIIRangeFactoryE
	.align	16
_ZTVN11xercesc_2_517ASCIIRangeFactoryE:
	.quad	0
	.quad	_ZTIN11xercesc_2_517ASCIIRangeFactoryE
	.quad	_ZN11xercesc_2_517ASCIIRangeFactoryD2Ev
	.quad	_ZN11xercesc_2_517ASCIIRangeFactoryD0Ev
	.quad	_ZN11xercesc_2_517ASCIIRangeFactory20initializeKeywordMapEv
	.quad	_ZN11xercesc_2_517ASCIIRangeFactory11buildRangesEv
	.size	_ZTVN11xercesc_2_517ASCIIRangeFactoryE, 48

	.type	_ZN11xercesc_2_5L12fgASCIISpaceE,@object # @_ZN11xercesc_2_5L12fgASCIISpaceE
	.align	16
_ZN11xercesc_2_5L12fgASCIISpaceE:
	.short	97                      # 0x61
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	58                      # 0x3a
	.short	105                     # 0x69
	.short	115                     # 0x73
	.short	83                      # 0x53
	.short	112                     # 0x70
	.short	97                      # 0x61
	.short	99                      # 0x63
	.short	101                     # 0x65
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L12fgASCIISpaceE, 28

	.type	_ZN11xercesc_2_5L12fgASCIIDigitE,@object # @_ZN11xercesc_2_5L12fgASCIIDigitE
	.align	16
_ZN11xercesc_2_5L12fgASCIIDigitE:
	.short	97                      # 0x61
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	58                      # 0x3a
	.short	105                     # 0x69
	.short	115                     # 0x73
	.short	68                      # 0x44
	.short	105                     # 0x69
	.short	103                     # 0x67
	.short	105                     # 0x69
	.short	116                     # 0x74
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L12fgASCIIDigitE, 28

	.type	_ZN11xercesc_2_5L11fgASCIIWordE,@object # @_ZN11xercesc_2_5L11fgASCIIWordE
	.align	16
_ZN11xercesc_2_5L11fgASCIIWordE:
	.short	97                      # 0x61
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	58                      # 0x3a
	.short	105                     # 0x69
	.short	115                     # 0x73
	.short	87                      # 0x57
	.short	111                     # 0x6f
	.short	114                     # 0x72
	.short	100                     # 0x64
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L11fgASCIIWordE, 26

	.type	_ZN11xercesc_2_5L13fgASCIIXDigitE,@object # @_ZN11xercesc_2_5L13fgASCIIXDigitE
	.align	16
_ZN11xercesc_2_5L13fgASCIIXDigitE:
	.short	97                      # 0x61
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	58                      # 0x3a
	.short	105                     # 0x69
	.short	115                     # 0x73
	.short	88                      # 0x58
	.short	68                      # 0x44
	.short	105                     # 0x69
	.short	103                     # 0x67
	.short	105                     # 0x69
	.short	116                     # 0x74
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L13fgASCIIXDigitE, 30

	.type	_ZN11xercesc_2_5L7fgASCIIE,@object # @_ZN11xercesc_2_5L7fgASCIIE
	.align	16
_ZN11xercesc_2_5L7fgASCIIE:
	.short	97                      # 0x61
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	58                      # 0x3a
	.short	105                     # 0x69
	.short	115                     # 0x73
	.short	65                      # 0x41
	.short	115                     # 0x73
	.short	99                      # 0x63
	.short	105                     # 0x69
	.short	105                     # 0x69
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L7fgASCIIE, 28

	.type	_ZN11xercesc_2_5L15fgASCIICategoryE,@object # @_ZN11xercesc_2_5L15fgASCIICategoryE
	.align	2
_ZN11xercesc_2_5L15fgASCIICategoryE:
	.short	65                      # 0x41
	.short	83                      # 0x53
	.short	67                      # 0x43
	.short	73                      # 0x49
	.short	73                      # 0x49
	.short	0                       # 0x0
	.size	_ZN11xercesc_2_5L15fgASCIICategoryE, 12

	.type	_ZTSN11xercesc_2_517ASCIIRangeFactoryE,@object # @_ZTSN11xercesc_2_517ASCIIRangeFactoryE
	.globl	_ZTSN11xercesc_2_517ASCIIRangeFactoryE
	.align	16
_ZTSN11xercesc_2_517ASCIIRangeFactoryE:
	.asciz	"N11xercesc_2_517ASCIIRangeFactoryE"
	.size	_ZTSN11xercesc_2_517ASCIIRangeFactoryE, 35

	.type	_ZTIN11xercesc_2_517ASCIIRangeFactoryE,@object # @_ZTIN11xercesc_2_517ASCIIRangeFactoryE
	.globl	_ZTIN11xercesc_2_517ASCIIRangeFactoryE
	.align	16
_ZTIN11xercesc_2_517ASCIIRangeFactoryE:
	.quad	_ZTVN10__cxxabiv120__si_class_type_infoE+16
	.quad	_ZTSN11xercesc_2_517ASCIIRangeFactoryE
	.quad	_ZTIN11xercesc_2_512RangeFactoryE
	.size	_ZTIN11xercesc_2_517ASCIIRangeFactoryE, 24


	.globl	_ZN11xercesc_2_517ASCIIRangeFactoryC1Ev
_ZN11xercesc_2_517ASCIIRangeFactoryC1Ev = _ZN11xercesc_2_517ASCIIRangeFactoryC2Ev
	.globl	_ZN11xercesc_2_517ASCIIRangeFactoryD1Ev
_ZN11xercesc_2_517ASCIIRangeFactoryD1Ev = _ZN11xercesc_2_517ASCIIRangeFactoryD2Ev
	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
