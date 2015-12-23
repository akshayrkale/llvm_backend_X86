//===-- Cse523ISelLowering.cpp - Cse523 DAG Lowering Implementation -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Cse523 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cse523-isel"
#include "Cse523ISelLowering.h"
#include "Utils/Cse523ShuffleDecode.h"
#include "Cse523.h"
#include "Cse523CallingConv.h"
#include "Cse523InstrBuilder.h"
#include "Cse523TargetMachine.h"
#include "Cse523TargetObjectFile.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/VariadicFunction.h"
#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Target/TargetOptions.h"
#include <bitset>
#include <cctype>
using namespace llvm;

STATISTIC(NumTailCalls, "Number of tail calls");

// Forward declarations.
static SDValue getMOVL(SelectionDAG &DAG, SDLoc dl, EVT VT, SDValue V1,
        SDValue V2);

static SDValue ExtractSubVector(SDValue Vec, unsigned IdxVal,
        SelectionDAG &DAG, SDLoc dl,
        unsigned vectorWidth) {
    assert((vectorWidth == 128 || vectorWidth == 256) &&
            "Unsupported vector width");
    EVT VT = Vec.getValueType();
    EVT ElVT = VT.getVectorElementType();
    unsigned Factor = VT.getSizeInBits()/vectorWidth;
    EVT ResultVT = EVT::getVectorVT(*DAG.getContext(), ElVT,
            VT.getVectorNumElements()/Factor);

    // Extract from UNDEF is UNDEF.
    if (Vec.getOpcode() == ISD::UNDEF)
        return DAG.getUNDEF(ResultVT);

    // Extract the relevant vectorWidth bits.  Generate an EXTRACT_SUBVECTOR
    unsigned ElemsPerChunk = vectorWidth / ElVT.getSizeInBits();

    // This is the index of the first element of the vectorWidth-bit chunk
    // we want.
    unsigned NormalizedIdxVal = (((IdxVal * ElVT.getSizeInBits()) / vectorWidth)
            * ElemsPerChunk);

    // If the input is a buildvector just emit a smaller one.
    if (Vec.getOpcode() == ISD::BUILD_VECTOR)
        return DAG.getNode(ISD::BUILD_VECTOR, dl, ResultVT,
                Vec->op_begin()+NormalizedIdxVal, ElemsPerChunk);

    SDValue VecIdx = DAG.getIntPtrConstant(NormalizedIdxVal);
    SDValue Result = DAG.getNode(ISD::EXTRACT_SUBVECTOR, dl, ResultVT, Vec,
            VecIdx);

    return Result;

}
/// Generate a DAG to grab 128-bits from a vector > 128 bits.  This
/// sets things up to match to an AVX VEXTRACTF128 / VEXTRACTI128
/// or AVX-512 VEXTRACTF32x4 / VEXTRACTI32x4
/// instructions or a simple subregister reference. Idx is an index in the
/// 128 bits we want.  It need not be aligned to a 128-bit bounday.  That makes
/// lowering EXTRACT_VECTOR_ELT operations easier.
static SDValue Extract128BitVector(SDValue Vec, unsigned IdxVal,
        SelectionDAG &DAG, SDLoc dl) {
    assert((Vec.getValueType().is256BitVector() ||
                Vec.getValueType().is512BitVector()) && "Unexpected vector size!");
    return ExtractSubVector(Vec, IdxVal, DAG, dl, 128);
}

/// Generate a DAG to grab 256-bits from a 512-bit vector.
static SDValue Extract256BitVector(SDValue Vec, unsigned IdxVal,
        SelectionDAG &DAG, SDLoc dl) {
    assert(Vec.getValueType().is512BitVector() && "Unexpected vector size!");
    return ExtractSubVector(Vec, IdxVal, DAG, dl, 256);
}

static SDValue InsertSubVector(SDValue Result, SDValue Vec,
        unsigned IdxVal, SelectionDAG &DAG,
        SDLoc dl, unsigned vectorWidth) {
    assert((vectorWidth == 128 || vectorWidth == 256) &&
            "Unsupported vector width");
    // Inserting UNDEF is Result
    if (Vec.getOpcode() == ISD::UNDEF)
        return Result;
    EVT VT = Vec.getValueType();
    EVT ElVT = VT.getVectorElementType();
    EVT ResultVT = Result.getValueType();

    // Insert the relevant vectorWidth bits.
    unsigned ElemsPerChunk = vectorWidth/ElVT.getSizeInBits();

    // This is the index of the first element of the vectorWidth-bit chunk
    // we want.
    unsigned NormalizedIdxVal = (((IdxVal * ElVT.getSizeInBits())/vectorWidth)
            * ElemsPerChunk);

    SDValue VecIdx = DAG.getIntPtrConstant(NormalizedIdxVal);
    return DAG.getNode(ISD::INSERT_SUBVECTOR, dl, ResultVT, Result, Vec,
            VecIdx);
}
/// Generate a DAG to put 128-bits into a vector > 128 bits.  This
/// sets things up to match to an AVX VINSERTF128/VINSERTI128 or
/// AVX-512 VINSERTF32x4/VINSERTI32x4 instructions or a
/// simple superregister reference.  Idx is an index in the 128 bits
/// we want.  It need not be aligned to a 128-bit bounday.  That makes
/// lowering INSERT_VECTOR_ELT operations easier.
static SDValue Insert128BitVector(SDValue Result, SDValue Vec,
        unsigned IdxVal, SelectionDAG &DAG,
        SDLoc dl) {
    assert(Vec.getValueType().is128BitVector() && "Unexpected vector size!");
    return InsertSubVector(Result, Vec, IdxVal, DAG, dl, 128);
}

static SDValue Insert256BitVector(SDValue Result, SDValue Vec,
        unsigned IdxVal, SelectionDAG &DAG,
        SDLoc dl) {
    assert(Vec.getValueType().is256BitVector() && "Unexpected vector size!");
    return InsertSubVector(Result, Vec, IdxVal, DAG, dl, 256);
}

/// Concat two 128-bit vectors into a 256 bit vector using VINSERTF128
/// instructions. This is used because creating CONCAT_VECTOR nodes of
/// BUILD_VECTORS returns a larger BUILD_VECTOR while we're trying to lower
/// large BUILD_VECTORS.
static SDValue Concat128BitVectors(SDValue V1, SDValue V2, EVT VT,
        unsigned NumElems, SelectionDAG &DAG,
        SDLoc dl) {
    SDValue V = Insert128BitVector(DAG.getUNDEF(VT), V1, 0, DAG, dl);
    return Insert128BitVector(V, V2, NumElems/2, DAG, dl);
}

static SDValue Concat256BitVectors(SDValue V1, SDValue V2, EVT VT,
        unsigned NumElems, SelectionDAG &DAG,
        SDLoc dl) {
    SDValue V = Insert256BitVector(DAG.getUNDEF(VT), V1, 0, DAG, dl);
    return Insert256BitVector(V, V2, NumElems/2, DAG, dl);
}

static TargetLoweringObjectFile *createTLOF(Cse523TargetMachine &TM) {
    const Cse523Subtarget *Subtarget = &TM.getSubtarget<Cse523Subtarget>();

    if (Subtarget->isTargetMacho())
        return new Cse523_64MachoTargetObjectFile();
    if (Subtarget->isTargetLinux())
        return new Cse523LinuxTargetObjectFile();
    if (Subtarget->isTargetELF())
        return new TargetLoweringObjectFileELF();
    if (Subtarget->isTargetWindows())
        return new Cse523WindowsTargetObjectFile();
    if (Subtarget->isTargetCOFF())
        return new TargetLoweringObjectFileCOFF();

    llvm_unreachable("unknown subtarget type");
}

Cse523TargetLowering::Cse523TargetLowering(Cse523TargetMachine &TM)
    : TargetLowering(TM, createTLOF(TM)) 
{
    Subtarget = &TM.getSubtarget<Cse523Subtarget>();
    Cse523ScalarSSEf64 = Subtarget->hasSSE2();
    Cse523ScalarSSEf32 = Subtarget->hasSSE1();
    TD = getDataLayout();

    resetOperationActions();
}

void Cse523TargetLowering::resetOperationActions() {
    const TargetMachine &TM = getTargetMachine();
    static bool FirstTimeThrough = true;

    // If none of the target options have changed, then we don't need to reset the
    // operation actions.
    if (!FirstTimeThrough && TO == TM.Options) return;

    if (!FirstTimeThrough) {
        // Reinitialize the actions.
        initActions();
        FirstTimeThrough = false;
    }

    TO = TM.Options;

    // Set up the TargetLowering object.
    static const MVT IntVTs[] = { MVT::i8, MVT::i16, MVT::i32, MVT::i64 };

    // Cse523 is weird, it always uses i8 for shift amounts and setcc results.
    setBooleanContents(ZeroOrOneBooleanContent);
    // Cse523-SSE is even stranger. It uses -1 or 0 for vector masks.
    setBooleanVectorContents(ZeroOrNegativeOneBooleanContent);

    // For 64-bit since we have so many registers use the ILP scheduler, for
    // 32-bit code use the register pressure specific scheduling.
    // For Atom, always use ILP scheduling.
    if (Subtarget->isAtom())
        setSchedulingPreference(Sched::ILP);
    else if (Subtarget->is64Bit())
        setSchedulingPreference(Sched::ILP);
    else
        setSchedulingPreference(Sched::RegPressure);
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(TM.getRegisterInfo());
    setStackPointerRegisterToSaveRestore(RegInfo->getStackRegister());

    // Bypass expensive divides on Atom when compiling with O2
    if (Subtarget->hasSlowDivide() && TM.getOptLevel() >= CodeGenOpt::Default) {
        addBypassSlowDiv(32, 8);
        if (Subtarget->is64Bit())
            addBypassSlowDiv(64, 16);
    }

    if (Subtarget->isTargetWindows() && !Subtarget->isTargetCygMing()) {
        // Setup Windows compiler runtime calls.
        setLibcallName(RTLIB::SDIV_I64, "_alldiv");
        setLibcallName(RTLIB::UDIV_I64, "_aulldiv");
        setLibcallName(RTLIB::SREM_I64, "_allrem");
        setLibcallName(RTLIB::UREM_I64, "_aullrem");
        setLibcallName(RTLIB::MUL_I64, "_allmul");
        setLibcallCallingConv(RTLIB::SDIV_I64, CallingConv::X86_StdCall);
        setLibcallCallingConv(RTLIB::UDIV_I64, CallingConv::X86_StdCall);
        setLibcallCallingConv(RTLIB::SREM_I64, CallingConv::X86_StdCall);
        setLibcallCallingConv(RTLIB::UREM_I64, CallingConv::X86_StdCall);
        setLibcallCallingConv(RTLIB::MUL_I64,  CallingConv::X86_StdCall);

        // The _ftol2 runtime function has an unusual calling conv, which
        // is modeled by a special pseudo-instruction.
        setLibcallName(RTLIB::FPTOUINT_F64_I64, 0);
        setLibcallName(RTLIB::FPTOUINT_F32_I64, 0);
        setLibcallName(RTLIB::FPTOUINT_F64_I32, 0);
        setLibcallName(RTLIB::FPTOUINT_F32_I32, 0);
    }
    #include "Cse523SelLibCallName.inc"


    if (Subtarget->isTargetDarwin()) {
        // Darwin should use _setjmp/_longjmp instead of setjmp/longjmp.
        setUseUnderscoreSetJmp(false);
        setUseUnderscoreLongJmp(false);
    } else if (Subtarget->isTargetMingw()) {
        // MS runtime is weird: it exports _setjmp, but longjmp!
        setUseUnderscoreSetJmp(true);
        setUseUnderscoreLongJmp(false);
    } else {
        setUseUnderscoreSetJmp(true);
        setUseUnderscoreLongJmp(true);
    }

   
    // Set up the register classes.
    addRegisterClass(MVT::i64, &Cse523::GR64RegClass);
//    addRegisterClass(MVT::i32, &Cse523::GR64RegClass);

    setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);

   
    // SETOEQ and SETUNE require checking two conditions.
    setCondCodeAction(ISD::SETOEQ, MVT::f32, Expand);
    setCondCodeAction(ISD::SETOEQ, MVT::f64, Expand);
    setCondCodeAction(ISD::SETOEQ, MVT::f80, Expand);
    setCondCodeAction(ISD::SETUNE, MVT::f32, Expand);
    setCondCodeAction(ISD::SETUNE, MVT::f64, Expand);
    setCondCodeAction(ISD::SETUNE, MVT::f80, Expand);

    // Promote all UINT_TO_FP to larger SINT_TO_FP's, as Cse523 doesn't have this
    // operation.
    setOperationAction(ISD::UINT_TO_FP       , MVT::i1   , Promote);
    setOperationAction(ISD::UINT_TO_FP       , MVT::i8   , Promote);
    setOperationAction(ISD::UINT_TO_FP       , MVT::i16  , Promote);

    setOperationAction(ISD::UINT_TO_FP     , MVT::i32  , Promote);
    setOperationAction(ISD::UINT_TO_FP     , MVT::i64  , Custom);

    // Promote i1/i8 SINT_TO_FP to larger SINT_TO_FP's, as Cse523 doesn't have
    // this operation.
    setOperationAction(ISD::SINT_TO_FP       , MVT::i1   , Promote);
    setOperationAction(ISD::SINT_TO_FP       , MVT::i8   , Promote);

    if (!TM.Options.UseSoftFloat) {
        setOperationAction(ISD::SINT_TO_FP     , MVT::i16  , Custom);
        setOperationAction(ISD::SINT_TO_FP     , MVT::i32  , Custom);
    } else {
        setOperationAction(ISD::SINT_TO_FP     , MVT::i16  , Promote);
        setOperationAction(ISD::SINT_TO_FP     , MVT::i32  , Promote);
    }

    // In 32-bit mode these are custom lowered.  In 64-bit mode F32 and F64
    // are Legal, f80 is custom lowered.
    setOperationAction(ISD::FP_TO_SINT     , MVT::i64  , Custom);
    setOperationAction(ISD::SINT_TO_FP     , MVT::i64  , Custom);

    // Promote i1/i8 FP_TO_SINT to larger FP_TO_SINTS's, as Cse523 doesn't have
    // this operation.
    setOperationAction(ISD::FP_TO_SINT       , MVT::i1   , Promote);
    setOperationAction(ISD::FP_TO_SINT       , MVT::i8   , Promote);

    setOperationAction(ISD::FP_TO_SINT     , MVT::i16  , Promote);
    setOperationAction(ISD::FP_TO_SINT     , MVT::i32  , Promote);

    // Handle FP_TO_UINT by promoting the destination to a larger signed
    // conversion.
    setOperationAction(ISD::FP_TO_UINT       , MVT::i1   , Promote);
    setOperationAction(ISD::FP_TO_UINT       , MVT::i8   , Promote);
    setOperationAction(ISD::FP_TO_UINT       , MVT::i16  , Promote);

    setOperationAction(ISD::FP_TO_UINT     , MVT::i64  , Expand);
    setOperationAction(ISD::FP_TO_UINT     , MVT::i32  , Promote);

    if (isTargetFTOL()) {
        // Use the _ftol2 runtime function, which has a pseudo-instruction
        // to handle its weird calling convention.
        setOperationAction(ISD::FP_TO_UINT     , MVT::i64  , Custom);
    }

    // TODO: when we have SSE, these could be more efficient, by using movd/movq.
    if (!Cse523ScalarSSEf64) {
        setOperationAction(ISD::BITCAST        , MVT::f32  , Expand);
        setOperationAction(ISD::BITCAST        , MVT::i32  , Expand);
        if (Subtarget->is64Bit()) {
            setOperationAction(ISD::BITCAST      , MVT::f64  , Expand);
            // Without SSE, i64->f64 goes through memory.
            setOperationAction(ISD::BITCAST      , MVT::i64  , Expand);
        }
    }

    // Scalar integer divide and remainder are lowered to use operations that
    // produce two results, to match the available instructions. This exposes
    // the two-result form to trivial CSE, which is able to combine x/y and x%y
    // into a single instruction.
    //
    // Scalar integer multiply-high is also lowered to use two-result
    // operations, to match the available instructions. However, plain multiply
    // (low) operations are left as Legal, as there are single-result
    // instructions for this in cse523. Using the two-result multiply instructions
    // when both high and low results are needed must be arranged by dagcombine.
    for (unsigned i = 0; i != array_lengthof(IntVTs); ++i) {
        MVT VT = IntVTs[i];
        setOperationAction(ISD::MULHS, VT, Expand);
        setOperationAction(ISD::MULHU, VT, Expand);
        setOperationAction(ISD::SDIV, VT, Expand);
        setOperationAction(ISD::UDIV, VT, Expand);
        setOperationAction(ISD::SREM, VT, Expand);
        setOperationAction(ISD::UREM, VT, Expand);

        // Add/Sub overflow ops with MVT::Glues are lowered to EFLAGS dependences.
        setOperationAction(ISD::ADDC, VT, Custom);
        setOperationAction(ISD::ADDE, VT, Custom);
        setOperationAction(ISD::SUBC, VT, Custom);
        setOperationAction(ISD::SUBE, VT, Custom);
    }

    setOperationAction(ISD::BR_JT            , MVT::Other, Expand);
    setOperationAction(ISD::BRCOND           , MVT::Other, Custom);
    setOperationAction(ISD::BR_CC            , MVT::f32,   Expand);
    setOperationAction(ISD::BR_CC            , MVT::f64,   Expand);
    setOperationAction(ISD::BR_CC            , MVT::f80,   Expand);
    setOperationAction(ISD::BR_CC            , MVT::i8,    Expand);
    setOperationAction(ISD::BR_CC            , MVT::i16,   Expand);
    setOperationAction(ISD::BR_CC            , MVT::i32,   Expand);
    setOperationAction(ISD::BR_CC            , MVT::i64,   Expand);
    setOperationAction(ISD::SELECT_CC        , MVT::Other, Expand);

    setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i32, Expand);
    setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16  , Expand);
    setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8   , Expand);
    setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1   , Expand);
    setOperationAction(ISD::FP_ROUND_INREG   , MVT::f32  , Expand);
    setOperationAction(ISD::FREM             , MVT::f32  , Expand);
    setOperationAction(ISD::FREM             , MVT::f64  , Expand);
    setOperationAction(ISD::FREM             , MVT::f80  , Expand);
    setOperationAction(ISD::FLT_ROUNDS_      , MVT::i32  , Custom);

    // Promote the i8 variants and force them on up to i32 which has a shorter
    // encoding.
    setOperationAction(ISD::CTTZ             , MVT::i8   , Promote);
    AddPromotedToType (ISD::CTTZ             , MVT::i8   , MVT::i32);
    setOperationAction(ISD::CTTZ_ZERO_UNDEF  , MVT::i8   , Promote);
    AddPromotedToType (ISD::CTTZ_ZERO_UNDEF  , MVT::i8   , MVT::i32);
    if (Subtarget->hasBMI()) {
        setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i16  , Expand);
        setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i32  , Expand);
        if (Subtarget->is64Bit())
            setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i64, Expand);
    } else {
        setOperationAction(ISD::CTTZ           , MVT::i16  , Custom);
        setOperationAction(ISD::CTTZ           , MVT::i32  , Custom);
        if (Subtarget->is64Bit())
            setOperationAction(ISD::CTTZ         , MVT::i64  , Custom);
    }

    if (Subtarget->hasLZCNT()) {
        // When promoting the i8 variants, force them to i32 for a shorter
        // encoding.
        setOperationAction(ISD::CTLZ           , MVT::i8   , Promote);
        AddPromotedToType (ISD::CTLZ           , MVT::i8   , MVT::i32);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i8   , Promote);
        AddPromotedToType (ISD::CTLZ_ZERO_UNDEF, MVT::i8   , MVT::i32);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i16  , Expand);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32  , Expand);
        if (Subtarget->is64Bit())
            setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i64, Expand);
    } else {
        setOperationAction(ISD::CTLZ           , MVT::i8   , Custom);
        setOperationAction(ISD::CTLZ           , MVT::i16  , Custom);
        setOperationAction(ISD::CTLZ           , MVT::i32  , Custom);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i8   , Custom);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i16  , Custom);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32  , Custom);
        if (Subtarget->is64Bit()) {
            setOperationAction(ISD::CTLZ         , MVT::i64  , Custom);
            setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i64, Custom);
        }
    }

    if (Subtarget->hasPOPCNT()) {
        setOperationAction(ISD::CTPOP          , MVT::i8   , Promote);
    } else {
        setOperationAction(ISD::CTPOP          , MVT::i8   , Expand);
        setOperationAction(ISD::CTPOP          , MVT::i16  , Expand);
        setOperationAction(ISD::CTPOP          , MVT::i32  , Expand);
        if (Subtarget->is64Bit())
            setOperationAction(ISD::CTPOP        , MVT::i64  , Expand);
    }

    setOperationAction(ISD::READCYCLECOUNTER , MVT::i64  , Custom);
    setOperationAction(ISD::BSWAP            , MVT::i16  , Expand);

    // These should be promoted to a larger select which is supported.
    setOperationAction(ISD::SELECT          , MVT::i1   , Promote);
    // Cse523 wants to expand cmov itself.
    setOperationAction(ISD::SELECT          , MVT::i8   , Promote);
    setOperationAction(ISD::SELECT          , MVT::i16  , Promote);
    setOperationAction(ISD::SELECT          , MVT::i32  , Promote);
    setOperationAction(ISD::SELECT          , MVT::f32  , Promote);
    setOperationAction(ISD::SELECT          , MVT::f64  , Custom);
    setOperationAction(ISD::SELECT          , MVT::f80  , Custom);
    setOperationAction(ISD::SETCC           , MVT::i8   , Promote);
    setOperationAction(ISD::SETCC           , MVT::i16  , Promote);
    setOperationAction(ISD::SETCC           , MVT::i32  , Promote);
    setOperationAction(ISD::SETCC           , MVT::f32  , Promote);
    setOperationAction(ISD::SETCC           , MVT::f64  , Custom);
    setOperationAction(ISD::SETCC           , MVT::f80  , Custom);
    if (Subtarget->is64Bit()) {
        setOperationAction(ISD::SELECT        , MVT::i64  , Custom);
        setOperationAction(ISD::SETCC         , MVT::i64  , Custom);
    }
    setOperationAction(ISD::EH_RETURN       , MVT::Other, Custom);
    	
    // NOTE: EH_SJLJ_SETJMP/_LONGJMP supported here is NOT intended to support
    // SjLj exception handling but a light-weight setjmp/longjmp replacement to
    // support continuation, user-level threading, and etc.. As a result, no
    // other SjLj exception interfaces are implemented and please don't build
    // your own exception handling based on them.
    // LLVM/Clang supports zero-cost DWARF exception handling.
    setOperationAction(ISD::EH_SJLJ_SETJMP, MVT::i32, Custom);
    setOperationAction(ISD::EH_SJLJ_LONGJMP, MVT::Other, Custom);

    // Darwin ABI issue.
    setOperationAction(ISD::ConstantPool    , MVT::i32  , Custom);
    setOperationAction(ISD::JumpTable       , MVT::i32  , Custom);
    setOperationAction(ISD::GlobalAddress   , MVT::i32  , Custom);
    setOperationAction(ISD::GlobalTLSAddress, MVT::i32  , Custom);
    if (Subtarget->is64Bit())
        setOperationAction(ISD::GlobalTLSAddress, MVT::i64, Custom);
    setOperationAction(ISD::ExternalSymbol  , MVT::i32  , Custom);
    setOperationAction(ISD::BlockAddress    , MVT::i32  , Custom);
    if (Subtarget->is64Bit()) {
        setOperationAction(ISD::ConstantPool  , MVT::i64  , Custom);
        setOperationAction(ISD::JumpTable     , MVT::i64  , Custom);
        setOperationAction(ISD::GlobalAddress , MVT::i64  , Custom);
        setOperationAction(ISD::ExternalSymbol, MVT::i64  , Custom);
        setOperationAction(ISD::BlockAddress  , MVT::i64  , Custom);
    }
    // 64-bit addm sub, shl, sra, srl (iff 32-bit cse523)
    setOperationAction(ISD::SHL_PARTS       , MVT::i32  , Custom);
    setOperationAction(ISD::SRA_PARTS       , MVT::i32  , Custom);
    setOperationAction(ISD::SRL_PARTS       , MVT::i32  , Custom);
    if (Subtarget->is64Bit()) {
        setOperationAction(ISD::SHL_PARTS     , MVT::i64  , Custom);
        setOperationAction(ISD::SRA_PARTS     , MVT::i64  , Custom);
        setOperationAction(ISD::SRL_PARTS     , MVT::i64  , Custom);
    }

    if (Subtarget->hasSSE1())
        setOperationAction(ISD::PREFETCH      , MVT::Other, Legal);

    setOperationAction(ISD::ATOMIC_FENCE  , MVT::Other, Custom);

    // Expand certain atomics
    for (unsigned i = 0; i != array_lengthof(IntVTs); ++i) {
        MVT VT = IntVTs[i];
        setOperationAction(ISD::ATOMIC_CMP_SWAP, VT, Custom);
        setOperationAction(ISD::ATOMIC_LOAD_SUB, VT, Custom);
        setOperationAction(ISD::ATOMIC_STORE, VT, Custom);
    }

    if (Subtarget->hasCmpxchg16b()) {
        setOperationAction(ISD::ATOMIC_CMP_SWAP, MVT::i128, Custom);
    }

    // FIXME - use subtarget debug flags
    if (!Subtarget->isTargetDarwin() &&
            !Subtarget->isTargetELF() &&
            !Subtarget->isTargetCygMing()) {
        setOperationAction(ISD::EH_LABEL, MVT::Other, Expand);
    }

    setExceptionPointerRegister(Cse523::RAX);
    setExceptionSelectorRegister(Cse523::RDX);

    setOperationAction(ISD::FRAME_TO_ARGS_OFFSET, MVT::i32, Custom);
    setOperationAction(ISD::FRAME_TO_ARGS_OFFSET, MVT::i64, Custom);

    setOperationAction(ISD::INIT_TRAMPOLINE, MVT::Other, Custom);
    setOperationAction(ISD::ADJUST_TRAMPOLINE, MVT::Other, Custom);

    setOperationAction(ISD::TRAP, MVT::Other, Legal);
    setOperationAction(ISD::DEBUGTRAP, MVT::Other, Legal);

    // VASTART needs to be custom lowered to use the VarArgsFrameIndex
    setOperationAction(ISD::VASTART           , MVT::Other, Custom);
    setOperationAction(ISD::VAEND             , MVT::Other, Expand);
    if (Subtarget->is64Bit() && !Subtarget->isTargetWin64()) {
        // TargetInfo::Cse523_64ABIBuiltinVaList
        setOperationAction(ISD::VAARG           , MVT::Other, Custom);
        setOperationAction(ISD::VACOPY          , MVT::Other, Custom);
    } else {
        // TargetInfo::CharPtrBuiltinVaList
        setOperationAction(ISD::VAARG           , MVT::Other, Expand);
        setOperationAction(ISD::VACOPY          , MVT::Other, Expand);
    }

    setOperationAction(ISD::STACKSAVE,          MVT::Other, Expand);
    setOperationAction(ISD::STACKRESTORE,       MVT::Other, Expand);

    if (Subtarget->isOSWindows() && !Subtarget->isTargetMacho())
        setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Custom);
    else if (TM.Options.EnableSegmentedStacks)
        setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Custom);
    else
        setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Expand);

    if (!TM.Options.UseSoftFloat) {
        // f32 and f64 in x87.
        // Set up the FP register classes.
        setOperationAction(ISD::UNDEF,     MVT::f64, Expand);
        setOperationAction(ISD::UNDEF,     MVT::f32, Expand);
        setOperationAction(ISD::FCOPYSIGN, MVT::f64, Expand);
        setOperationAction(ISD::FCOPYSIGN, MVT::f32, Expand);

        if (!TM.Options.UnsafeFPMath) {
            setOperationAction(ISD::FSIN   , MVT::f64, Expand);
            setOperationAction(ISD::FSIN   , MVT::f32, Expand);
            setOperationAction(ISD::FCOS   , MVT::f64, Expand);
            setOperationAction(ISD::FCOS   , MVT::f32, Expand);
            setOperationAction(ISD::FSINCOS, MVT::f64, Expand);
            setOperationAction(ISD::FSINCOS, MVT::f32, Expand);
        }
        addLegalFPImmediate(APFloat(+0.0)); // FLD0
        addLegalFPImmediate(APFloat(+1.0)); // FLD1
        addLegalFPImmediate(APFloat(-0.0)); // FLD0/FCHS
        addLegalFPImmediate(APFloat(-1.0)); // FLD1/FCHS
        addLegalFPImmediate(APFloat(+0.0f)); // FLD0
        addLegalFPImmediate(APFloat(+1.0f)); // FLD1
        addLegalFPImmediate(APFloat(-0.0f)); // FLD0/FCHS
        addLegalFPImmediate(APFloat(-1.0f)); // FLD1/FCHS
    }

    // We don't support FMA.
    setOperationAction(ISD::FMA, MVT::f64, Expand);
    setOperationAction(ISD::FMA, MVT::f32, Expand);

    // Long double always uses X87.
    if (!TM.Options.UseSoftFloat) {
        setOperationAction(ISD::UNDEF,     MVT::f80, Expand);
        setOperationAction(ISD::FCOPYSIGN, MVT::f80, Expand);
        {
            APFloat TmpFlt = APFloat::getZero(APFloat::x87DoubleExtended);
            addLegalFPImmediate(TmpFlt);  // FLD0
            TmpFlt.changeSign();
            addLegalFPImmediate(TmpFlt);  // FLD0/FCHS

            bool ignored;
            APFloat TmpFlt2(+1.0);
            TmpFlt2.convert(APFloat::x87DoubleExtended, APFloat::rmNearestTiesToEven,
                    &ignored);
            addLegalFPImmediate(TmpFlt2);  // FLD1
            TmpFlt2.changeSign();
            addLegalFPImmediate(TmpFlt2);  // FLD1/FCHS
        }

        if (!TM.Options.UnsafeFPMath) {
            setOperationAction(ISD::FSIN   , MVT::f80, Expand);
            setOperationAction(ISD::FCOS   , MVT::f80, Expand);
            setOperationAction(ISD::FSINCOS, MVT::f80, Expand);
        }

        setOperationAction(ISD::FFLOOR, MVT::f80, Expand);
        setOperationAction(ISD::FCEIL,  MVT::f80, Expand);
        setOperationAction(ISD::FTRUNC, MVT::f80, Expand);
        setOperationAction(ISD::FRINT,  MVT::f80, Expand);
        setOperationAction(ISD::FNEARBYINT, MVT::f80, Expand);
        setOperationAction(ISD::FMA, MVT::f80, Expand);
    }

    // Always use a library call for pow.
    setOperationAction(ISD::FPOW             , MVT::f32  , Expand);
    setOperationAction(ISD::FPOW             , MVT::f64  , Expand);
    setOperationAction(ISD::FPOW             , MVT::f80  , Expand);

    setOperationAction(ISD::FLOG, MVT::f80, Expand);
    setOperationAction(ISD::FLOG2, MVT::f80, Expand);
    setOperationAction(ISD::FLOG10, MVT::f80, Expand);
    setOperationAction(ISD::FEXP, MVT::f80, Expand);
    setOperationAction(ISD::FEXP2, MVT::f80, Expand);

    // First set operation action for all vector types to either promote
    // (for widening) or expand (for scalarization). Then we will selectively
    // turn on ones that can be effectively codegen'd.
    for (int i = MVT::FIRST_VECTOR_VALUETYPE;
            i <= MVT::LAST_VECTOR_VALUETYPE; ++i) {
        MVT VT = (MVT::SimpleValueType)i;
        setOperationAction(ISD::ADD , VT, Expand);
        setOperationAction(ISD::SUB , VT, Expand);
        setOperationAction(ISD::FADD, VT, Expand);
        setOperationAction(ISD::FNEG, VT, Expand);
        setOperationAction(ISD::FSUB, VT, Expand);
        setOperationAction(ISD::MUL , VT, Expand);
        setOperationAction(ISD::FMUL, VT, Expand);
        setOperationAction(ISD::SDIV, VT, Expand);
        setOperationAction(ISD::UDIV, VT, Expand);
        setOperationAction(ISD::FDIV, VT, Expand);
        setOperationAction(ISD::SREM, VT, Expand);
        setOperationAction(ISD::UREM, VT, Expand);
        setOperationAction(ISD::LOAD, VT, Expand);
        setOperationAction(ISD::VECTOR_SHUFFLE, VT, Expand);
        setOperationAction(ISD::EXTRACT_VECTOR_ELT, VT,Expand);
        setOperationAction(ISD::INSERT_VECTOR_ELT, VT, Expand);
        setOperationAction(ISD::EXTRACT_SUBVECTOR, VT,Expand);
        setOperationAction(ISD::INSERT_SUBVECTOR, VT,Expand);
        setOperationAction(ISD::FABS, VT, Expand);
        setOperationAction(ISD::FSIN, VT, Expand);
        setOperationAction(ISD::FSINCOS, VT, Expand);
        setOperationAction(ISD::FCOS, VT, Expand);
        setOperationAction(ISD::FSINCOS, VT, Expand);
        setOperationAction(ISD::FREM, VT, Expand);
        setOperationAction(ISD::FMA,  VT, Expand);
        setOperationAction(ISD::FPOWI, VT, Expand);
        setOperationAction(ISD::FSQRT, VT, Expand);
        setOperationAction(ISD::FCOPYSIGN, VT, Expand);
        setOperationAction(ISD::FFLOOR, VT, Expand);
        setOperationAction(ISD::FCEIL, VT, Expand);
        setOperationAction(ISD::FTRUNC, VT, Expand);
        setOperationAction(ISD::FRINT, VT, Expand);
        setOperationAction(ISD::FNEARBYINT, VT, Expand);
        setOperationAction(ISD::SMUL_LOHI, VT, Expand);
        setOperationAction(ISD::UMUL_LOHI, VT, Expand);
        setOperationAction(ISD::SDIVREM, VT, Expand);
        setOperationAction(ISD::UDIVREM, VT, Expand);
        setOperationAction(ISD::FPOW, VT, Expand);
        setOperationAction(ISD::CTPOP, VT, Expand);
        setOperationAction(ISD::CTTZ, VT, Expand);
        setOperationAction(ISD::CTTZ_ZERO_UNDEF, VT, Expand);
        setOperationAction(ISD::CTLZ, VT, Expand);
        setOperationAction(ISD::CTLZ_ZERO_UNDEF, VT, Expand);
        setOperationAction(ISD::SHL, VT, Expand);
        setOperationAction(ISD::SRA, VT, Expand);
        setOperationAction(ISD::SRL, VT, Expand);
        setOperationAction(ISD::ROTL, VT, Expand);
        setOperationAction(ISD::ROTR, VT, Expand);
        setOperationAction(ISD::BSWAP, VT, Expand);
        setOperationAction(ISD::SETCC, VT, Expand);
        setOperationAction(ISD::FLOG, VT, Expand);
        setOperationAction(ISD::FLOG2, VT, Expand);
        setOperationAction(ISD::FLOG10, VT, Expand);
        setOperationAction(ISD::FEXP, VT, Expand);
        setOperationAction(ISD::FEXP2, VT, Expand);
        setOperationAction(ISD::FP_TO_UINT, VT, Expand);
        setOperationAction(ISD::FP_TO_SINT, VT, Expand);
        setOperationAction(ISD::UINT_TO_FP, VT, Expand);
        setOperationAction(ISD::SINT_TO_FP, VT, Expand);
        setOperationAction(ISD::SIGN_EXTEND_INREG, VT,Expand);
        setOperationAction(ISD::TRUNCATE, VT, Expand);
        setOperationAction(ISD::SIGN_EXTEND, VT, Expand);
        setOperationAction(ISD::ZERO_EXTEND, VT, Expand);
        setOperationAction(ISD::ANY_EXTEND, VT, Expand);
        setOperationAction(ISD::VSELECT, VT, Expand);
        for (int InnerVT = MVT::FIRST_VECTOR_VALUETYPE;
                InnerVT <= MVT::LAST_VECTOR_VALUETYPE; ++InnerVT)
            setTruncStoreAction(VT,
                    (MVT::SimpleValueType)InnerVT, Expand);
        setLoadExtAction(ISD::SEXTLOAD, VT, Expand);
        setLoadExtAction(ISD::ZEXTLOAD, VT, Expand);
        setLoadExtAction(ISD::EXTLOAD, VT, Expand);
    }

    // SIGN_EXTEND_INREGs are evaluated by the extend type. Handle the expansion
    // of this type with custom code.
    for (int VT = MVT::FIRST_VECTOR_VALUETYPE;
            VT != MVT::LAST_VECTOR_VALUETYPE; VT++) {
        setOperationAction(ISD::SIGN_EXTEND_INREG, (MVT::SimpleValueType)VT,
                Custom);
    }

    // We want to custom lower some of our intrinsics.
    setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);
    setOperationAction(ISD::INTRINSIC_W_CHAIN, MVT::Other, Custom);
    setOperationAction(ISD::INTRINSIC_VOID, MVT::Other, Custom);

    // Only custom-lower 64-bit SADDO and friends on 64-bit because we don't
    // handle type legalization for these operations here.
    //
    // FIXME: We really should do custom legalization for addition and
    // subtraction on cse523-32 once PR3203 is fixed.  We really can't do much better
    // than generic legalization for 64-bit multiplication-with-overflow, though.
    for (unsigned i = 0, e = 3+Subtarget->is64Bit(); i != e; ++i) {
        // Add/Sub/Mul with overflow operations are custom lowered.
        MVT VT = IntVTs[i];
        setOperationAction(ISD::SADDO, VT, Custom);
        setOperationAction(ISD::UADDO, VT, Custom);
        setOperationAction(ISD::SSUBO, VT, Custom);
        setOperationAction(ISD::USUBO, VT, Custom);
        setOperationAction(ISD::SMULO, VT, Custom);
        setOperationAction(ISD::UMULO, VT, Custom);
    }

    // There are no 8-bit 3-address imul/mul instructions
    setOperationAction(ISD::SMULO, MVT::i8, Expand);
    setOperationAction(ISD::UMULO, MVT::i8, Expand);

    // Combine sin / cos into one node or libcall if possible.
    if (Subtarget->hasSinCos()) {
        setLibcallName(RTLIB::SINCOS_F32, "sincosf");
        setLibcallName(RTLIB::SINCOS_F64, "sincos");
        if (Subtarget->isTargetDarwin()) {
            // For MacOSX, we don't want to the normal expansion of a libcall to
            // sincos. We want to issue a libcall to __sincos_stret to avoid memory
            // traffic.
            setOperationAction(ISD::FSINCOS, MVT::f64, Custom);
            setOperationAction(ISD::FSINCOS, MVT::f32, Custom);
        }
    }

    // We have target-specific dag combine patterns for the following nodes:
    setTargetDAGCombine(ISD::VECTOR_SHUFFLE);
    setTargetDAGCombine(ISD::EXTRACT_VECTOR_ELT);
    setTargetDAGCombine(ISD::VSELECT);
    setTargetDAGCombine(ISD::SELECT);
    setTargetDAGCombine(ISD::SHL);
    setTargetDAGCombine(ISD::SRA);
    setTargetDAGCombine(ISD::SRL);
    setTargetDAGCombine(ISD::OR);
    setTargetDAGCombine(ISD::AND);
    setTargetDAGCombine(ISD::ADD);
    setTargetDAGCombine(ISD::FADD);
    setTargetDAGCombine(ISD::FSUB);
    setTargetDAGCombine(ISD::FMA);
    setTargetDAGCombine(ISD::SUB);
    setTargetDAGCombine(ISD::LOAD);
    setTargetDAGCombine(ISD::STORE);
    setTargetDAGCombine(ISD::ZERO_EXTEND);
    setTargetDAGCombine(ISD::ANY_EXTEND);
    setTargetDAGCombine(ISD::SIGN_EXTEND);
    setTargetDAGCombine(ISD::SIGN_EXTEND_INREG);
    setTargetDAGCombine(ISD::TRUNCATE);
    setTargetDAGCombine(ISD::SINT_TO_FP);
    setTargetDAGCombine(ISD::SETCC);
    if (Subtarget->is64Bit())
        setTargetDAGCombine(ISD::MUL);
    setTargetDAGCombine(ISD::XOR);

    computeRegisterProperties();

    // On Darwin, -Os means optimize for size without hurting performance,
    // do not reduce the limit.
    MaxStoresPerMemset = 16; // For @llvm.memset -> sequence of stores
    MaxStoresPerMemsetOptSize = Subtarget->isTargetDarwin() ? 16 : 8;
    MaxStoresPerMemcpy = 8; // For @llvm.memcpy -> sequence of stores
    MaxStoresPerMemcpyOptSize = Subtarget->isTargetDarwin() ? 8 : 4;
    MaxStoresPerMemmove = 8; // For @llvm.memmove -> sequence of stores
    MaxStoresPerMemmoveOptSize = Subtarget->isTargetDarwin() ? 8 : 4;
    setPrefLoopAlignment(4); // 2^4 bytes.

    // Predictable cmov don't hurt on atom because it's in-order.
    PredictableSelectIsExpensive = !Subtarget->isAtom();

    setPrefFunctionAlignment(4); // 2^4 bytes.
}

EVT Cse523TargetLowering::getSetCCResultType(LLVMContext &, EVT VT) const {
//    assert(!VT.isVector());
 if (!VT.isVector())
    return MVT::i64;

    switch(VT.getVectorNumElements()) {
    case  2: return MVT::v2i1;
    case  4: return MVT::v4i1;
    case  8: return MVT::v8i1;
    case 16: return MVT::v16i1;
  }

  return VT.changeVectorElementTypeToInteger();
}

/// getMaxByValAlign - Helper for getByValTypeAlignment to determine
/// the desired ByVal argument alignment.
static void getMaxByValAlign(Type *Ty, unsigned &MaxAlign) {
    if (MaxAlign == 16)
        return;
    if (VectorType *VTy = dyn_cast<VectorType>(Ty)) {
        if (VTy->getBitWidth() == 128)
            MaxAlign = 16;
    } else if (ArrayType *ATy = dyn_cast<ArrayType>(Ty)) {
        unsigned EltAlign = 0;
        getMaxByValAlign(ATy->getElementType(), EltAlign);
        if (EltAlign > MaxAlign)
            MaxAlign = EltAlign;
    } else if (StructType *STy = dyn_cast<StructType>(Ty)) {
        for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
            unsigned EltAlign = 0;
            getMaxByValAlign(STy->getElementType(i), EltAlign);
            if (EltAlign > MaxAlign)
                MaxAlign = EltAlign;
            if (MaxAlign == 16)
                break;
        }
    }
}

/// getByValTypeAlignment - Return the desired alignment for ByVal aggregate
/// function arguments in the caller parameter area. For Cse523, aggregates
/// that contain SSE vectors are placed at 16-byte boundaries while the rest
/// are at 4-byte boundaries.
unsigned Cse523TargetLowering::getByValTypeAlignment(Type *Ty) const {
    if (Subtarget->is64Bit()) {
        // Max of 8 and alignment of type.
        unsigned TyAlign = TD->getABITypeAlignment(Ty);
        if (TyAlign > 8)
            return TyAlign;
        return 8;
    }

    unsigned Align = 4;
    if (Subtarget->hasSSE1())
        getMaxByValAlign(Ty, Align);
    return Align;
}

/// getOptimalMemOpType - Returns the target specific optimal type for load
/// and store operations as a result of memset, memcpy, and memmove
/// lowering. If DstAlign is zero that means it's safe to destination
/// alignment can satisfy any constraint. Similarly if SrcAlign is zero it
/// means there isn't a need to check it against alignment requirement,
/// probably because the source does not need to be loaded. If 'IsMemset' is
/// true, that means it's expanding a memset. If 'ZeroMemset' is true, that
/// means it's a memset of zero. 'MemcpyStrSrc' indicates whether the memcpy
/// source is constant so it does not need to be loaded.
/// It returns EVT::Other if the type should be determined using generic
/// target-independent logic.
EVT
Cse523TargetLowering::getOptimalMemOpType(uint64_t Size,
        unsigned DstAlign, unsigned SrcAlign,
        bool IsMemset, bool ZeroMemset,
        bool MemcpyStrSrc,
        MachineFunction &MF) const {
    const Function *F = MF.getFunction();
    if ((!IsMemset || ZeroMemset) &&
            !F->getAttributes().hasAttribute(AttributeSet::FunctionIndex,
                Attribute::NoImplicitFloat)) {
        if (Size >= 16 &&
                (Subtarget->isUnalignedMemAccessFast() ||
                 ((DstAlign == 0 || DstAlign >= 16) &&
                  (SrcAlign == 0 || SrcAlign >= 16)))) {
            if (Size >= 32) {
                if (Subtarget->hasInt256())
                    return MVT::v8i32;
                if (Subtarget->hasFp256())
                    return MVT::v8f32;
            }
            if (Subtarget->hasSSE2())
                return MVT::v4i32;
            if (Subtarget->hasSSE1())
                return MVT::v4f32;
        } else if (!MemcpyStrSrc && Size >= 8 &&
                !Subtarget->is64Bit() &&
                Subtarget->hasSSE2()) {
            // Do not use f64 to lower memcpy if source is string constant. It's
            // better to use i32 to avoid the loads.
            return MVT::f64;
        }
    }
    if (Subtarget->is64Bit() && Size >= 8)
        return MVT::i64;
    return MVT::i32;
}

bool Cse523TargetLowering::isSafeMemOpType(MVT VT) const {
    if (VT == MVT::f32)
        return Cse523ScalarSSEf32;
    else if (VT == MVT::f64)
        return Cse523ScalarSSEf64;
    return true;
}

bool
Cse523TargetLowering::allowsUnalignedMemoryAccesses(EVT VT,
        unsigned,
        bool *Fast) const {
    if (Fast)
        *Fast = Subtarget->isUnalignedMemAccessFast();
    return true;
}

/// getJumpTableEncoding - Return the entry encoding for a jump table in the
/// current function.  The returned value is a member of the
/// MachineJumpTableInfo::JTEntryKind enum.
unsigned Cse523TargetLowering::getJumpTableEncoding() const {
    // In GOT pic mode, each entry in the jump table is emitted as a @GOTOFF
    // symbol.
    if (getTargetMachine().getRelocationModel() == Reloc::PIC_ &&
            Subtarget->isPICStyleGOT())
        return MachineJumpTableInfo::EK_Custom32;

    // Otherwise, use the normal jump table encoding heuristics.
    return TargetLowering::getJumpTableEncoding();
}

const MCExpr *
Cse523TargetLowering::LowerCustomJumpTableEntry(const MachineJumpTableInfo *MJTI,
        const MachineBasicBlock *MBB,
        unsigned uid,MCContext &Ctx) const{
    assert(getTargetMachine().getRelocationModel() == Reloc::PIC_ &&
            Subtarget->isPICStyleGOT());
    // In 32-bit ELF systems, our jump table entries are formed with @GOTOFF
    // entries.
    return MCSymbolRefExpr::Create(MBB->getSymbol(),
            MCSymbolRefExpr::VK_GOTOFF, Ctx);
}

/// getPICJumpTableRelocaBase - Returns relocation base for the given PIC
/// jumptable.
SDValue Cse523TargetLowering::getPICJumpTableRelocBase(SDValue Table,
        SelectionDAG &DAG) const {
    if (!Subtarget->is64Bit())
        // This doesn't have SDLoc associated with it, but is not really the
        // same as a Register.
        return DAG.getNode(Cse523ISD::GlobalBaseReg, SDLoc(), getPointerTy());
    return Table;
}

/// getPICJumpTableRelocBaseExpr - This returns the relocation base for the
/// given PIC jumptable, the same as getPICJumpTableRelocBase, but as an
/// MCExpr.
const MCExpr *Cse523TargetLowering::
getPICJumpTableRelocBaseExpr(const MachineFunction *MF, unsigned JTI,
        MCContext &Ctx) const {
    // Cse523-64 uses RIP relative addressing based on the jump table label.
    if (Subtarget->isPICStyleRIPRel())
        return TargetLowering::getPICJumpTableRelocBaseExpr(MF, JTI, Ctx);

    // Otherwise, the reference is relative to the PIC base.
    return MCSymbolRefExpr::Create(MF->getPICBaseSymbol(), Ctx);
}

// FIXME: Why this routine is here? Move to RegInfo!
std::pair<const TargetRegisterClass*, uint8_t>
Cse523TargetLowering::findRepresentativeClass(MVT VT) const{
    const TargetRegisterClass *RRC = 0;
    uint8_t Cost = 1;
    switch (VT.SimpleTy) {
        default:
            return TargetLowering::findRepresentativeClass(VT);
        case MVT::i8: case MVT::i16: case MVT::i32: case MVT::i64:
            RRC = (const TargetRegisterClass*)&Cse523::GR64RegClass;
            break;
        case MVT::f32: case MVT::f64:
        case MVT::v16i8: case MVT::v8i16: case MVT::v4i32: case MVT::v2i64:
        case MVT::v4f32: case MVT::v2f64:
        case MVT::v32i8: case MVT::v8i32: case MVT::v4i64: case MVT::v8f32:
        case MVT::v4f64:
            break;
    }
    return std::make_pair(RRC, Cost);
}

bool Cse523TargetLowering::getStackCookieLocation(unsigned &AddressSpace,
        unsigned &Offset) const {
    if (!Subtarget->isTargetLinux())
        return false;

    if (Subtarget->is64Bit()) {
        // %fs:0x28, unless we're using a Kernel code model, in which case it's %gs:
        Offset = 0x28;
        if (getTargetMachine().getCodeModel() == CodeModel::Kernel)
            AddressSpace = 256;
        else
            AddressSpace = 257;
    } else {
        // %gs:0x14 on i386
        Offset = 0x14;
        AddressSpace = 256;
    }
    return true;
}

bool Cse523TargetLowering::isNoopAddrSpaceCast(unsigned SrcAS,
        unsigned DestAS) const {
    assert(SrcAS != DestAS && "Expected different address spaces!");

    return SrcAS < 256 && DestAS < 256;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "Cse523GenCallingConv.inc"

bool
Cse523TargetLowering::CanLowerReturn(CallingConv::ID CallConv,
        MachineFunction &MF, bool isVarArg,
        const SmallVectorImpl<ISD::OutputArg> &Outs,
        LLVMContext &Context) const {
    SmallVector<CCValAssign, 16> RVLocs;
    CCState CCInfo(CallConv, isVarArg, MF, getTargetMachine(),
            RVLocs, Context);
    return CCInfo.CheckReturn(Outs, RetCC_Cse523);
}

const uint16_t *Cse523TargetLowering::getScratchRegisters(CallingConv::ID) const {
    static const uint16_t ScratchRegs[] = { Cse523::R11, 0 };
    return ScratchRegs;
}

SDValue
Cse523TargetLowering::LowerReturn(SDValue Chain,
        CallingConv::ID CallConv, bool isVarArg,
        const SmallVectorImpl<ISD::OutputArg> &Outs,
        const SmallVectorImpl<SDValue> &OutVals,
        SDLoc dl, SelectionDAG &DAG) const {
    MachineFunction &MF = DAG.getMachineFunction();
    Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();

    SmallVector<CCValAssign, 16> RVLocs;
    CCState CCInfo(CallConv, isVarArg, MF, getTargetMachine(),
            RVLocs, *DAG.getContext());
    CCInfo.AnalyzeReturn(Outs, RetCC_Cse523);

    SDValue Flag;
    SmallVector<SDValue, 6> RetOps;
    RetOps.push_back(Chain); // Operand #0 = Chain (updated below)
    // Operand #1 = Bytes To Pop
    RetOps.push_back(DAG.getTargetConstant(FuncInfo->getBytesToPopOnReturn(),
                MVT::i64));

    // Copy the result values into the output registers.
    for (unsigned i = 0; i != RVLocs.size(); ++i) {
        CCValAssign &VA = RVLocs[i];
        assert(VA.isRegLoc() && "Can only return in registers!");
        SDValue ValToCopy = OutVals[i];
        EVT ValVT = ValToCopy.getValueType();

        // Promote values to the appropriate types
        if (VA.getLocInfo() == CCValAssign::SExt)
            ValToCopy = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), ValToCopy);
        else if (VA.getLocInfo() == CCValAssign::ZExt)
            ValToCopy = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), ValToCopy);
        else if (VA.getLocInfo() == CCValAssign::AExt)
            ValToCopy = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), ValToCopy);
        else if (VA.getLocInfo() == CCValAssign::BCvt)
            ValToCopy = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), ValToCopy);

        assert(VA.getLocInfo() != CCValAssign::FPExt &&
                "Unexpected FP-extend for return value.");  

        // If this is cse523-64, and we disabled SSE, we can't return FP values,
        // or SSE or MMX vectors.
        //if ((ValVT == MVT::f32 || ValVT == MVT::f64 ||
        //            VA.getLocReg() == Cse523::XMM0 || VA.getLocReg() == Cse523::XMM1) &&
        //        (Subtarget->is64Bit() && !Subtarget->hasSSE1())) {
        //    report_fatal_error("SSE register return with SSE disabled");
        //}
        // Likewise we can't return F64 values with SSE1 only.  gcc does so, but
        // llvm-gcc has never done it right and no one has noticed, so this
        // should be OK for now.
        if (ValVT == MVT::f64 &&
                (Subtarget->is64Bit() && !Subtarget->hasSSE2()))
            report_fatal_error("SSE2 register return with SSE2 disabled");

        // Returns in ST0/ST1 are handled specially: these are pushed as operands to
        // the RET instruction and handled by the FP Stackifier.
        //if (VA.getLocReg() == Cse523::ST0 ||
        //        VA.getLocReg() == Cse523::ST1) {
        //    // If this is a copy from an xmm register to ST(0), use an FPExtend to
        //    // change the value to the FP stack register class.
        //    if (isScalarFPTypeInSSEReg(VA.getValVT()))
        //        ValToCopy = DAG.getNode(ISD::FP_EXTEND, dl, MVT::f80, ValToCopy);
        //    RetOps.push_back(ValToCopy);
        //    // Don't emit a copytoreg.
        //    continue;
        //}

        // 64-bit vector (MMX) values are returned in XMM0 / XMM1 except for v1i64
        // which is returned in RAX / RDX.
        //if (Subtarget->is64Bit()) {
        //    if (ValVT == MVT::cse523mmx) {
        //        if (VA.getLocReg() == Cse523::XMM0 || VA.getLocReg() == Cse523::XMM1) {
        //            ValToCopy = DAG.getNode(ISD::BITCAST, dl, MVT::i64, ValToCopy);
        //            ValToCopy = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, MVT::v2i64,
        //                    ValToCopy);
        //            // If we don't have SSE2 available, convert to v4f32 so the generated
        //            // register is legal.
        //            if (!Subtarget->hasSSE2())
        //                ValToCopy = DAG.getNode(ISD::BITCAST, dl, MVT::v4f32,ValToCopy);
        //        }
        //    }
        //}

        Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(), ValToCopy, Flag);
        Flag = Chain.getValue(1);
        RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
    }

    // The cse523-64 ABIs require that for returning structs by value we copy
    // the sret argument into %rax/%eax (depending on ABI) for the return.
    // Win32 requires us to put the sret argument to %eax as well.
    // We saved the argument into a virtual register in the entry block,
    // so now we copy the value out and into %rax/%eax.
    if (DAG.getMachineFunction().getFunction()->hasStructRetAttr() &&
            (Subtarget->is64Bit() || Subtarget->isTargetWindows())) {
        MachineFunction &MF = DAG.getMachineFunction();
        Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();
        unsigned Reg = FuncInfo->getSRetReturnReg();
        assert(Reg &&
                "SRetReturnReg should have been set in LowerFormalArguments().");
        SDValue Val = DAG.getCopyFromReg(Chain, dl, Reg, getPointerTy());

        unsigned RetValReg = Cse523::RAX;
        Chain = DAG.getCopyToReg(Chain, dl, RetValReg, Val, Flag);
        Flag = Chain.getValue(1);

        // RAX/EAX now acts like a return value.
        RetOps.push_back(DAG.getRegister(RetValReg, getPointerTy()));
    }

    RetOps[0] = Chain;  // Update chain.

    // Add the flag if we have it.
    if (Flag.getNode())
        RetOps.push_back(Flag);

    return DAG.getNode(Cse523ISD::RET_FLAG, dl,
            MVT::Other, &RetOps[0], RetOps.size());
}

bool Cse523TargetLowering::isUsedByReturnOnly(SDNode *N, SDValue &Chain) const {
    if (N->getNumValues() != 1)
        return false;
    if (!N->hasNUsesOfValue(1, 0))
        return false;

    SDValue TCChain = Chain;
    SDNode *Copy = *N->use_begin();
    if (Copy->getOpcode() == ISD::CopyToReg) {
        // If the copy has a glue operand, we conservatively assume it isn't safe to
        // perform a tail call.
        if (Copy->getOperand(Copy->getNumOperands()-1).getValueType() == MVT::Glue)
            return false;
        TCChain = Copy->getOperand(0);
    } else if (Copy->getOpcode() != ISD::FP_EXTEND)
        return false;

    bool HasRet = false;
    for (SDNode::use_iterator UI = Copy->use_begin(), UE = Copy->use_end();
            UI != UE; ++UI) {
        if (UI->getOpcode() != Cse523ISD::RET_FLAG)
            return false;
        HasRet = true;
    }

    if (!HasRet)
        return false;

    Chain = TCChain;
    return true;
}

MVT
Cse523TargetLowering::getTypeForExtArgOrReturn(MVT VT,
        ISD::NodeType ExtendKind) const {
    MVT ReturnMVT;
    // TODO: Is this also valid on 32-bit?
    if (Subtarget->is64Bit() && VT == MVT::i1 && ExtendKind == ISD::ZERO_EXTEND)
        ReturnMVT = MVT::i8;
    else
        ReturnMVT = MVT::i64;

    MVT MinVT = getRegisterType(ReturnMVT);
    return VT.bitsLT(MinVT) ? MinVT : VT;
}

/// LowerCallResult - Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
///
SDValue
Cse523TargetLowering::LowerCallResult(SDValue Chain, SDValue InFlag,
        CallingConv::ID CallConv, bool isVarArg,
        const SmallVectorImpl<ISD::InputArg> &Ins,
        SDLoc dl, SelectionDAG &DAG,
        SmallVectorImpl<SDValue> &InVals) const {

    // Assign locations to each value returned by this call.
    SmallVector<CCValAssign, 16> RVLocs;
    bool Is64Bit = Subtarget->is64Bit();
    CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
            getTargetMachine(), RVLocs, *DAG.getContext());
    CCInfo.AnalyzeCallResult(Ins, RetCC_Cse523);

    // Copy all of the result registers out of their specified physreg.
    for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
        CCValAssign &VA = RVLocs[i];
        EVT CopyVT = VA.getValVT();

        // If this is cse523-64, and we disabled SSE, we can't return FP values
        if ((CopyVT == MVT::f32 || CopyVT == MVT::f64) &&
                ((Is64Bit || Ins[i].Flags.isInReg()) && !Subtarget->hasSSE1())) {
            report_fatal_error("SSE register return with SSE disabled");
        }

        SDValue Val;

        // If this is a call to a function that returns an fp value on the floating
        // point stack, we must guarantee the value is popped from the stack, so
        // a CopyFromReg is not good enough - the copy instruction may be eliminated
        // if the return value is not used. We use the FpPOP_RETVAL instruction
        // instead.
//        if (VA.getLocReg() == Cse523::ST0 || VA.getLocReg() == Cse523::ST1) {
//            // If we prefer to use the value in xmm registers, copy it out as f80 and
//            // use a truncate to move it from fp stack reg to xmm reg.
//            if (isScalarFPTypeInSSEReg(VA.getValVT())) CopyVT = MVT::f80;
//            SDValue Ops[] = { Chain, InFlag };
//            Chain = SDValue(DAG.getMachineNode(Cse523::FpPOP_RETVAL, dl, CopyVT,
//                        MVT::Other, MVT::Glue, Ops), 1);
//            Val = Chain.getValue(0);
//
//            // Round the f80 to the right size, which also moves it to the appropriate
//            // xmm register.
//            if (CopyVT != VA.getValVT())
//                Val = DAG.getNode(ISD::FP_ROUND, dl, VA.getValVT(), Val,
//                        // This truncation won't change the value.
//                        DAG.getIntPtrConstant(1));
//        } else {
            Chain = DAG.getCopyFromReg(Chain, dl, VA.getLocReg(),
                    CopyVT, InFlag).getValue(1);
            Val = Chain.getValue(0);
//        }
        InFlag = Chain.getValue(2);
        InVals.push_back(Val);
    }

    return Chain;
}

//===----------------------------------------------------------------------===//
//                C & StdCall & Fast Calling Convention implementation
//===----------------------------------------------------------------------===//
//  StdCall calling convention seems to be standard for many Windows' API
//  routines and around. It differs from C calling convention just a little:
//  callee should clean up the stack, not caller. Symbols should be also
//  decorated in some fancy way :) It doesn't support any vector arguments.
//  For info on fast calling convention see Fast Calling Convention (tail call)
//  implementation LowerCse523_32FastCCCallTo.

/// CallIsStructReturn - Determines whether a call uses struct return
/// semantics.
enum StructReturnType {
    NotStructReturn,
    RegStructReturn,
    StackStructReturn
};
static StructReturnType
callIsStructReturn(const SmallVectorImpl<ISD::OutputArg> &Outs) {
    if (Outs.empty())
        return NotStructReturn;

    const ISD::ArgFlagsTy &Flags = Outs[0].Flags;
    if (!Flags.isSRet())
        return NotStructReturn;
    if (Flags.isInReg())
        return RegStructReturn;
    return StackStructReturn;
}

/// ArgsAreStructReturn - Determines whether a function uses struct
/// return semantics.
static StructReturnType
argsAreStructReturn(const SmallVectorImpl<ISD::InputArg> &Ins) {
    if (Ins.empty())
        return NotStructReturn;

    const ISD::ArgFlagsTy &Flags = Ins[0].Flags;
    if (!Flags.isSRet())
        return NotStructReturn;
    if (Flags.isInReg())
        return RegStructReturn;
    return StackStructReturn;
}

/// CreateCopyOfByValArgument - Make a copy of an aggregate at address specified
/// by "Src" to address "Dst" with size and alignment information specified by
/// the specific parameter attribute. The copy will be passed as a byval
/// function parameter.
static SDValue
CreateCopyOfByValArgument(SDValue Src, SDValue Dst, SDValue Chain,
        ISD::ArgFlagsTy Flags, SelectionDAG &DAG,
        SDLoc dl) {
    SDValue SizeNode = DAG.getConstant(Flags.getByValSize(), MVT::i64);

    return DAG.getMemcpy(Chain, dl, Dst, Src, SizeNode, Flags.getByValAlign(),
            /*isVolatile*/false, /*AlwaysInline=*/true,
            MachinePointerInfo(), MachinePointerInfo());
}

/// IsTailCallConvention - Return true if the calling convention is one that
/// supports tail call optimization.
static bool IsTailCallConvention(CallingConv::ID CC) {
    return (CC == CallingConv::Fast || CC == CallingConv::GHC ||
            CC == CallingConv::HiPE);
}

/// \brief Return true if the calling convention is a C calling convention.
static bool IsCCallConvention(CallingConv::ID CC) {
    return (CC == CallingConv::C || CC == CallingConv::X86_64_Win64 ||
            CC == CallingConv::X86_64_SysV);
}

bool Cse523TargetLowering::mayBeEmittedAsTailCall(CallInst *CI) const {
    if (!CI->isTailCall() || getTargetMachine().Options.DisableTailCalls)
        return false;

    CallSite CS(CI);
    CallingConv::ID CalleeCC = CS.getCallingConv();
    if (!IsTailCallConvention(CalleeCC) && !IsCCallConvention(CalleeCC))
        return false;

    return true;
}

/// FuncIsMadeTailCallSafe - Return true if the function is being made into
/// a tailcall target by changing its ABI.
static bool FuncIsMadeTailCallSafe(CallingConv::ID CC,
        bool GuaranteedTailCallOpt) {
    return GuaranteedTailCallOpt && IsTailCallConvention(CC);
}

SDValue
Cse523TargetLowering::LowerMemArgument(SDValue Chain,
        CallingConv::ID CallConv,
        const SmallVectorImpl<ISD::InputArg> &Ins,
        SDLoc dl, SelectionDAG &DAG,
        const CCValAssign &VA,
        MachineFrameInfo *MFI,
        unsigned i) const {
    // Create the nodes corresponding to a load from this parameter slot.
    ISD::ArgFlagsTy Flags = Ins[i].Flags;
    bool AlwaysUseMutable = FuncIsMadeTailCallSafe(CallConv,
            getTargetMachine().Options.GuaranteedTailCallOpt);
    bool isImmutable = !AlwaysUseMutable && !Flags.isByVal();
    EVT ValVT;

    // If value is passed by pointer we have address passed instead of the value
    // itself.
    if (VA.getLocInfo() == CCValAssign::Indirect)
        ValVT = VA.getLocVT();
    else
        ValVT = VA.getValVT();

    // FIXME: For now, all byval parameter objects are marked mutable. This can be
    // changed with more analysis.
    // In case of tail call optimization mark all arguments mutable. Since they
    // could be overwritten by lowering of arguments in case of a tail call.
    if (Flags.isByVal()) {
        unsigned Bytes = Flags.getByValSize();
        if (Bytes == 0) Bytes = 1; // Don't create zero-sized stack objects.
        int FI = MFI->CreateFixedObject(Bytes, VA.getLocMemOffset(), isImmutable);
        return DAG.getFrameIndex(FI, getPointerTy());
    } else {
        int FI = MFI->CreateFixedObject(ValVT.getSizeInBits()/8,
                VA.getLocMemOffset(), isImmutable);
        SDValue FIN = DAG.getFrameIndex(FI, getPointerTy());
        return DAG.getLoad(ValVT, dl, Chain, FIN,
                MachinePointerInfo::getFixedStack(FI),
                false, false, false, 0);
    }
}

    SDValue
Cse523TargetLowering::LowerFormalArguments(SDValue Chain,
        CallingConv::ID CallConv,
        bool isVarArg,
        const SmallVectorImpl<ISD::InputArg> &Ins,
        SDLoc dl,
        SelectionDAG &DAG,
        SmallVectorImpl<SDValue> &InVals)
    const {
        MachineFunction &MF = DAG.getMachineFunction();
        Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();

        const Function* Fn = MF.getFunction();
        if (Fn->hasExternalLinkage() &&
                Subtarget->isTargetCygMing() &&
                Fn->getName() == "main")
            FuncInfo->setForceFramePointer(true);

        MachineFrameInfo *MFI = MF.getFrameInfo();
        bool Is64Bit = Subtarget->is64Bit();
        bool IsWin64 = Subtarget->isCallingConvWin64(CallConv);

        assert(!(isVarArg && IsTailCallConvention(CallConv)) &&
                "Var args not supported with calling convention fastcc, ghc or hipe");

        // Assign locations to all of the incoming arguments.
        SmallVector<CCValAssign, 16> ArgLocs;
        CCState CCInfo(CallConv, isVarArg, MF, getTargetMachine(),
                ArgLocs, *DAG.getContext());

        // Allocate shadow area for Win64
        if (IsWin64)
            CCInfo.AllocateStack(32, 8);

        CCInfo.AnalyzeFormalArguments(Ins, CC_Cse523);

        unsigned LastVal = ~0U;
        SDValue ArgValue;
        for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
            CCValAssign &VA = ArgLocs[i];
            // TODO: If an arg is passed in two places (e.g. reg and stack), skip later
            // places.
            assert(VA.getValNo() != LastVal &&
                    "Don't support value assigned to multiple locs yet");
            (void)LastVal;
            LastVal = VA.getValNo();

            if (VA.isRegLoc()) {
                EVT RegVT = VA.getLocVT();
                const TargetRegisterClass *RC;
                if (Is64Bit && RegVT == MVT::i64)
                    RC = &Cse523::GR64RegClass;
                else
                    llvm_unreachable("Unknown argument type!");

                unsigned Reg = MF.addLiveIn(VA.getLocReg(), RC);
                ArgValue = DAG.getCopyFromReg(Chain, dl, Reg, RegVT);

                // If this is an 8 or 16-bit value, it is really passed promoted to 32
                // bits.  Insert an assert[sz]ext to capture this, then truncate to the
                // right size.
                if (VA.getLocInfo() == CCValAssign::SExt)
                    ArgValue = DAG.getNode(ISD::AssertSext, dl, RegVT, ArgValue,
                            DAG.getValueType(VA.getValVT()));
                else if (VA.getLocInfo() == CCValAssign::ZExt)
                    ArgValue = DAG.getNode(ISD::AssertZext, dl, RegVT, ArgValue,
                            DAG.getValueType(VA.getValVT()));
                else if (VA.getLocInfo() == CCValAssign::BCvt)
                    ArgValue = DAG.getNode(ISD::BITCAST, dl, VA.getValVT(), ArgValue);

                if (VA.isExtInLoc()) {
                    // Handle MMX values passed in XMM regs.
                    if (RegVT.isVector())
                        ArgValue = DAG.getNode(Cse523ISD::MOVDQ2Q, dl, VA.getValVT(), ArgValue);
                    else
                        ArgValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), ArgValue);
                }
            } else {
                assert(VA.isMemLoc());
                ArgValue = LowerMemArgument(Chain, CallConv, Ins, dl, DAG, VA, MFI, i);
            }

            // If value is passed via pointer - do a load.
            if (VA.getLocInfo() == CCValAssign::Indirect)
                ArgValue = DAG.getLoad(VA.getValVT(), dl, Chain, ArgValue,
                        MachinePointerInfo(), false, false, false, 0);

            InVals.push_back(ArgValue);
        }

        // The cse523-64 ABIs require that for returning structs by value we copy
        // the sret argument into %rax/%eax (depending on ABI) for the return.
        // Win32 requires us to put the sret argument to %eax as well.
        // Save the argument into a virtual register so that we can access it
        // from the return points.
        if (MF.getFunction()->hasStructRetAttr() &&
                (Subtarget->is64Bit() || Subtarget->isTargetWindows())) {
            Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();
            unsigned Reg = FuncInfo->getSRetReturnReg();
            if (!Reg) {
                MVT PtrTy = getPointerTy();
                Reg = MF.getRegInfo().createVirtualRegister(getRegClassFor(PtrTy));
                FuncInfo->setSRetReturnReg(Reg);
            }
            SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
            Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);
        }

        unsigned StackSize = CCInfo.getNextStackOffset();
        // Align stack specially for tail calls.
        if (FuncIsMadeTailCallSafe(CallConv,
                    MF.getTarget().Options.GuaranteedTailCallOpt))
            StackSize = GetAlignedArgumentStackSize(StackSize, DAG);

        // If the function takes variable number of arguments, make a frame index for
        // the start of the first vararg value... for expansion of llvm.va_start.
        if (isVarArg) {
            if (Is64Bit || (CallConv != CallingConv::X86_FastCall &&
                        CallConv != CallingConv::X86_ThisCall)) {
                FuncInfo->setVarArgsFrameIndex(MFI->CreateFixedObject(1, StackSize,true));
            }
            if (Is64Bit) {
                unsigned TotalNumIntRegs = 0, TotalNumXMMRegs = 0;

                // FIXME: We should really autogenerate these arrays
                static const uint16_t GPR64ArgRegsWin64[] = {
                    Cse523::RCX, Cse523::RDX, Cse523::R8,  Cse523::R9
                };
                static const uint16_t GPR64ArgRegs64Bit[] = {
                    Cse523::RDI, Cse523::RSI, Cse523::RDX, Cse523::RCX, Cse523::R8, Cse523::R9
                };
                const uint16_t *GPR64ArgRegs;
                unsigned NumXMMRegs = 0;

                if (IsWin64) {
                    // The XMM registers which might contain var arg parameters are shadowed
                    // in their paired GPR.  So we only need to save the GPR to their home
                    // slots.
                    TotalNumIntRegs = 4;
                    GPR64ArgRegs = GPR64ArgRegsWin64;
                } else {
                    TotalNumIntRegs = 6; TotalNumXMMRegs = 0;
                    GPR64ArgRegs = GPR64ArgRegs64Bit;
                }
                unsigned NumIntRegs = CCInfo.getFirstUnallocated(GPR64ArgRegs,
                        TotalNumIntRegs);

                bool NoImplicitFloatOps = Fn->getAttributes().
                    hasAttribute(AttributeSet::FunctionIndex, Attribute::NoImplicitFloat);
                assert(!(NumXMMRegs && !Subtarget->hasSSE1()) &&
                        "SSE register cannot be used when SSE is disabled!");
                assert(!(NumXMMRegs && MF.getTarget().Options.UseSoftFloat &&
                            NoImplicitFloatOps) &&
                        "SSE register cannot be used when SSE is disabled!");
                if (MF.getTarget().Options.UseSoftFloat || NoImplicitFloatOps ||
                        !Subtarget->hasSSE1())
                    // Kernel mode asks for SSE to be disabled, so don't push them
                    // on the stack.
                    TotalNumXMMRegs = 0;

                if (IsWin64) {
                    const TargetFrameLowering &TFI = *getTargetMachine().getFrameLowering();
                    // Get to the caller-allocated home save location.  Add 8 to account
                    // for the return address.
                    int HomeOffset = TFI.getOffsetOfLocalArea() + 8;
                    FuncInfo->setRegSaveFrameIndex(
                            MFI->CreateFixedObject(1, NumIntRegs * 8 + HomeOffset, false));
                    // Fixup to set vararg frame on shadow area (4 x i64).
                    if (NumIntRegs < 4)
                        FuncInfo->setVarArgsFrameIndex(FuncInfo->getRegSaveFrameIndex());
                } else {
                    // For Cse523-64, if there are vararg parameters that are passed via
                    // registers, then we must store them to their spots on the stack so
                    // they may be loaded by deferencing the result of va_next.
                    FuncInfo->setVarArgsGPOffset(NumIntRegs * 8);
                    FuncInfo->setVarArgsFPOffset(TotalNumIntRegs * 8 + NumXMMRegs * 16);
                    FuncInfo->setRegSaveFrameIndex(
                            MFI->CreateStackObject(TotalNumIntRegs * 8 + TotalNumXMMRegs * 16, 16,
                                false));
                }

                // Store the integer parameter registers.
                SmallVector<SDValue, 8> MemOps;
                SDValue RSFIN = DAG.getFrameIndex(FuncInfo->getRegSaveFrameIndex(),
                        getPointerTy());
                unsigned Offset = FuncInfo->getVarArgsGPOffset();
                for (; NumIntRegs != TotalNumIntRegs; ++NumIntRegs) {
                    SDValue FIN = DAG.getNode(ISD::ADD, dl, getPointerTy(), RSFIN,
                            DAG.getIntPtrConstant(Offset));
                    unsigned VReg = MF.addLiveIn(GPR64ArgRegs[NumIntRegs],
                            &Cse523::GR64RegClass);
                    SDValue Val = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i64);
                    SDValue Store =
                        DAG.getStore(Val.getValue(1), dl, Val, FIN,
                                MachinePointerInfo::getFixedStack(
                                    FuncInfo->getRegSaveFrameIndex(), Offset),
                                false, false, 0);
                    MemOps.push_back(Store);
                    Offset += 8;
                }

                if (!MemOps.empty())
                    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                            &MemOps[0], MemOps.size());
            }
        }

        // Some CCs need callee pop.
        if (Cse523::isCalleePop(CallConv, Is64Bit, isVarArg,
                    MF.getTarget().Options.GuaranteedTailCallOpt)) {
            FuncInfo->setBytesToPopOnReturn(StackSize); // Callee pops everything.
        } else {
            FuncInfo->setBytesToPopOnReturn(0); // Callee pops nothing.
            // If this is an sret function, the return should pop the hidden pointer.
            if (!Is64Bit && !IsTailCallConvention(CallConv) &&
                    !Subtarget->getTargetTriple().isOSMSVCRT() &&
                    argsAreStructReturn(Ins) == StackStructReturn)
                FuncInfo->setBytesToPopOnReturn(4);
        }

        if (!Is64Bit) {
            // RegSaveFrameIndex is Cse523-64 only.
            FuncInfo->setRegSaveFrameIndex(0xAAAAAAA);
            if (CallConv == CallingConv::X86_FastCall ||
                    CallConv == CallingConv::X86_ThisCall)
                // fastcc functions can't have varargs.
                FuncInfo->setVarArgsFrameIndex(0xAAAAAAA);
        }

        FuncInfo->setArgumentStackSize(StackSize);

        return Chain;
    }

SDValue
Cse523TargetLowering::LowerMemOpCallTo(SDValue Chain,
        SDValue StackPtr, SDValue Arg,
        SDLoc dl, SelectionDAG &DAG,
        const CCValAssign &VA,
        ISD::ArgFlagsTy Flags) const {
    unsigned LocMemOffset = VA.getLocMemOffset();
    SDValue PtrOff = DAG.getIntPtrConstant(LocMemOffset);
    PtrOff = DAG.getNode(ISD::ADD, dl, getPointerTy(), StackPtr, PtrOff);
    if (Flags.isByVal())
        return CreateCopyOfByValArgument(Arg, PtrOff, Chain, Flags, DAG, dl);

    return DAG.getStore(Chain, dl, Arg, PtrOff,
            MachinePointerInfo::getStack(LocMemOffset),
            false, false, 0);
}

/// EmitTailCallLoadRetAddr - Emit a load of return address if tail call
/// optimization is performed and it is required.
SDValue
Cse523TargetLowering::EmitTailCallLoadRetAddr(SelectionDAG &DAG,
        SDValue &OutRetAddr, SDValue Chain,
        bool IsTailCall, bool Is64Bit,
        int FPDiff, SDLoc dl) const {
    // Adjust the Return address stack slot.
    EVT VT = getPointerTy();
    OutRetAddr = getReturnAddressFrameIndex(DAG);

    // Load the "old" Return address.
    OutRetAddr = DAG.getLoad(VT, dl, Chain, OutRetAddr, MachinePointerInfo(),
            false, false, false, 0);
    return SDValue(OutRetAddr.getNode(), 1);
}

/// EmitTailCallStoreRetAddr - Emit a store of the return address if tail call
/// optimization is performed and it is required (FPDiff!=0).
static SDValue
EmitTailCallStoreRetAddr(SelectionDAG & DAG, MachineFunction &MF,
        SDValue Chain, SDValue RetAddrFrIdx, EVT PtrVT,
        unsigned SlotSize, int FPDiff, SDLoc dl) {
    // Store the return address to the appropriate stack slot.
    if (!FPDiff) return Chain;
    // Calculate the new stack slot for the return address.
    int NewReturnAddrFI =
        MF.getFrameInfo()->CreateFixedObject(SlotSize, (int64_t)FPDiff - SlotSize,
                false);
    SDValue NewRetAddrFrIdx = DAG.getFrameIndex(NewReturnAddrFI, PtrVT);
    Chain = DAG.getStore(Chain, dl, RetAddrFrIdx, NewRetAddrFrIdx,
            MachinePointerInfo::getFixedStack(NewReturnAddrFI),
            false, false, 0);
    return Chain;
}

SDValue
Cse523TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
        SmallVectorImpl<SDValue> &InVals) const {
    SelectionDAG &DAG                     = CLI.DAG;
    SDLoc &dl                             = CLI.DL;
    SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
    SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
    SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
    SDValue Chain                         = CLI.Chain;
    SDValue Callee                        = CLI.Callee;
    CallingConv::ID CallConv              = CLI.CallConv;
    bool &isTailCall                      = CLI.IsTailCall;
    bool isVarArg                         = CLI.IsVarArg;

    MachineFunction &MF = DAG.getMachineFunction();
    bool Is64Bit        = Subtarget->is64Bit();
    bool IsWin64        = Subtarget->isCallingConvWin64(CallConv);
    StructReturnType SR = callIsStructReturn(Outs);
    bool IsSibcall      = false;

    if (MF.getTarget().Options.DisableTailCalls)
        isTailCall = false;

    if (isTailCall) {
        // Check if it's really possible to do a tail call.
        isTailCall = IsEligibleForTailCallOptimization(Callee, CallConv,
                isVarArg, SR != NotStructReturn,
                MF.getFunction()->hasStructRetAttr(), CLI.RetTy,
                Outs, OutVals, Ins, DAG);

        // Sibcalls are automatically detected tailcalls which do not require
        // ABI changes.
        if (!MF.getTarget().Options.GuaranteedTailCallOpt && isTailCall)
            IsSibcall = true;

        if (isTailCall)
            ++NumTailCalls;
    }

    assert(!(isVarArg && IsTailCallConvention(CallConv)) &&
            "Var args not supported with calling convention fastcc, ghc or hipe");

    // Analyze operands of the call, assigning locations to each operand.
    SmallVector<CCValAssign, 16> ArgLocs;
    CCState CCInfo(CallConv, isVarArg, MF, getTargetMachine(),
            ArgLocs, *DAG.getContext());

    // Allocate shadow area for Win64
    if (IsWin64)
        CCInfo.AllocateStack(32, 8);

    CCInfo.AnalyzeCallOperands(Outs, CC_Cse523);

    // Get a count of how many bytes are to be pushed on the stack.
    unsigned NumBytes = CCInfo.getNextStackOffset();
    if (IsSibcall)
        // This is a sibcall. The memory operands are available in caller's
        // own caller's stack.
        NumBytes = 0;
    else if (getTargetMachine().Options.GuaranteedTailCallOpt &&
            IsTailCallConvention(CallConv))
        NumBytes = GetAlignedArgumentStackSize(NumBytes, DAG);

    int FPDiff = 0;
    if (isTailCall && !IsSibcall) {
        // Lower arguments at fp - stackoffset + fpdiff.
        Cse523MachineFunctionInfo *Cse523Info = MF.getInfo<Cse523MachineFunctionInfo>();
        unsigned NumBytesCallerPushed = Cse523Info->getBytesToPopOnReturn();

        FPDiff = NumBytesCallerPushed - NumBytes;

        // Set the delta of movement of the returnaddr stackslot.
        // But only set if delta is greater than previous delta.
        if (FPDiff < Cse523Info->getTCReturnAddrDelta())
            Cse523Info->setTCReturnAddrDelta(FPDiff);
    }

    unsigned NumBytesToPush = NumBytes;
    unsigned NumBytesToPop = NumBytes;

    // If we have an inalloca argument, all stack space has already been allocated
    // for us and be right at the top of the stack.  We don't support multiple
    // arguments passed in memory when using inalloca.
    if (!Outs.empty() && Outs.back().Flags.isInAlloca()) {
        NumBytesToPush = 0;
        assert(ArgLocs.back().getLocMemOffset() == 0 &&
                "an inalloca argument must be the only memory argument");
    }

    if (!IsSibcall)
        Chain = DAG.getCALLSEQ_START(
                Chain, DAG.getIntPtrConstant(NumBytesToPush, true), dl);

    SDValue RetAddrFrIdx;
    // Load return address for tail calls.
    if (isTailCall && FPDiff)
        Chain = EmitTailCallLoadRetAddr(DAG, RetAddrFrIdx, Chain, isTailCall,
                Is64Bit, FPDiff, dl);

    SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
    SmallVector<SDValue, 8> MemOpChains;
    SDValue StackPtr;

    // Walk the register/memloc assignments, inserting copies/loads.  In the case
    // of tail call optimization arguments are handle later.
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
        // Skip inalloca arguments, they have already been written.
        ISD::ArgFlagsTy Flags = Outs[i].Flags;
        if (Flags.isInAlloca())
            continue;

        CCValAssign &VA = ArgLocs[i];
        EVT RegVT = VA.getLocVT();
        SDValue Arg = OutVals[i];
        bool isByVal = Flags.isByVal();

        // Promote the value if needed.
        switch (VA.getLocInfo()) {
            default: llvm_unreachable("Unknown loc info!");
            case CCValAssign::Full: break;
            case CCValAssign::SExt:
                                    Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, RegVT, Arg);
                                    break;
            case CCValAssign::ZExt:
                                    Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, RegVT, Arg);
                                    break;
            case CCValAssign::AExt:
                                    if (RegVT.is128BitVector()) {
                                        // Special case: passing MMX values in XMM registers.
                                        Arg = DAG.getNode(ISD::BITCAST, dl, MVT::i64, Arg);
                                        Arg = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, MVT::v2i64, Arg);
                                        Arg = getMOVL(DAG, dl, MVT::v2i64, DAG.getUNDEF(MVT::v2i64), Arg);
                                    } else
                                        Arg = DAG.getNode(ISD::ANY_EXTEND, dl, RegVT, Arg);
                                    break;
            case CCValAssign::BCvt:
                                    Arg = DAG.getNode(ISD::BITCAST, dl, RegVT, Arg);
                                    break;
            case CCValAssign::Indirect: {
                                            // Store the argument.
                                            SDValue SpillSlot = DAG.CreateStackTemporary(VA.getValVT());
                                            int FI = cast<FrameIndexSDNode>(SpillSlot)->getIndex();
                                            Chain = DAG.getStore(Chain, dl, Arg, SpillSlot,
                                                    MachinePointerInfo::getFixedStack(FI),
                                                    false, false, 0);
                                            Arg = SpillSlot;
                                            break;
                                        }
        }

        if (VA.isRegLoc()) {
            RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
            if (isVarArg && IsWin64) {
                // Win64 ABI requires argument XMM reg to be copied to the corresponding
                // shadow reg if callee is a varargs function.
                unsigned ShadowReg = 0;
                if (ShadowReg)
                    RegsToPass.push_back(std::make_pair(ShadowReg, Arg));
            }
        } else if (!IsSibcall && (!isTailCall || isByVal)) {
            assert(VA.isMemLoc());
            if (StackPtr.getNode() == 0)
                StackPtr = DAG.getCopyFromReg(Chain, dl, RegInfo->getStackRegister(),
                        getPointerTy());
            MemOpChains.push_back(LowerMemOpCallTo(Chain, StackPtr, Arg,
                        dl, DAG, VA, Flags));
        }
    }

    if (!MemOpChains.empty())
        Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                &MemOpChains[0], MemOpChains.size());

    if (Subtarget->isPICStyleGOT()) {
        // ELF / PIC requires GOT in the EBX register before function calls via PLT
        // GOT pointer.
        if (!isTailCall) {
            assert(0);
            //RegsToPass.push_back(std::make_pair(unsigned(Cse523::EBX),
            //            DAG.getNode(Cse523ISD::GlobalBaseReg, SDLoc(), getPointerTy())));
        } else {
            // If we are tail calling and generating PIC/GOT style code load the
            // address of the callee into ECX. The value in ecx is used as target of
            // the tail jump. This is done to circumvent the ebx/callee-saved problem
            // for tail calls on PIC/GOT architectures. Normally we would just put the
            // address of GOT into ebx and then call target@PLT. But for tail calls
            // ebx would be restored (since ebx is callee saved) before jumping to the
            // target@PLT.

            // Note: The actual moving to ECX is done further down.
            GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee);
            if (G && !G->getGlobal()->hasHiddenVisibility() &&
                    !G->getGlobal()->hasProtectedVisibility())
                Callee = LowerGlobalAddress(Callee, DAG);
            else if (isa<ExternalSymbolSDNode>(Callee))
                Callee = LowerExternalSymbol(Callee, DAG);
        }
    }

    if (Is64Bit && isVarArg && !IsWin64) {
        // From AMD64 ABI document:
        // For calls that may call functions that use varargs or stdargs
        // (prototype-less calls or calls to functions containing ellipsis (...) in
        // the declaration) %al is used as hidden argument to specify the number
        // of SSE registers used. The contents of %al do not need to match exactly
        // the number of registers, but must be an ubound on the number of SSE
        // registers used and is in the range 0 - 8 inclusive.

        // Count the number of XMM registers allocated.
        //static const uint16_t XMMArgRegs[] = {
        //    Cse523::XMM0, Cse523::XMM1, Cse523::XMM2, Cse523::XMM3,
        //    Cse523::XMM4, Cse523::XMM5, Cse523::XMM6, Cse523::XMM7
        //};
        unsigned NumXMMRegs = 0;//CCInfo.getFirstUnallocated(XMMArgRegs, 8);
        assert((Subtarget->hasSSE1() || !NumXMMRegs)
                && "SSE registers cannot be used when SSE is disabled");

        RegsToPass.push_back(std::make_pair(unsigned(Cse523::RAX),
                    DAG.getConstant(NumXMMRegs, MVT::i64)));
    }

    // For tail calls lower the arguments to the 'real' stack slot.
    if (isTailCall) {
        // Force all the incoming stack arguments to be loaded from the stack
        // before any new outgoing arguments are stored to the stack, because the
        // outgoing stack slots may alias the incoming argument stack slots, and
        // the alias isn't otherwise explicit. This is slightly more conservative
        // than necessary, because it means that each store effectively depends
        // on every argument instead of just those arguments it would clobber.
        SDValue ArgChain = DAG.getStackArgumentTokenFactor(Chain);

        SmallVector<SDValue, 8> MemOpChains2;
        SDValue FIN;
        int FI = 0;
        if (getTargetMachine().Options.GuaranteedTailCallOpt) {
            for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
                CCValAssign &VA = ArgLocs[i];
                if (VA.isRegLoc())
                    continue;
                assert(VA.isMemLoc());
                SDValue Arg = OutVals[i];
                ISD::ArgFlagsTy Flags = Outs[i].Flags;
                // Create frame index.
                int32_t Offset = VA.getLocMemOffset()+FPDiff;
                uint32_t OpSize = (VA.getLocVT().getSizeInBits()+7)/8;
                FI = MF.getFrameInfo()->CreateFixedObject(OpSize, Offset, true);
                FIN = DAG.getFrameIndex(FI, getPointerTy());

                if (Flags.isByVal()) {
                    // Copy relative to framepointer.
                    SDValue Source = DAG.getIntPtrConstant(VA.getLocMemOffset());
                    if (StackPtr.getNode() == 0)
                        StackPtr = DAG.getCopyFromReg(Chain, dl,
                                RegInfo->getStackRegister(),
                                getPointerTy());
                    Source = DAG.getNode(ISD::ADD, dl, getPointerTy(), StackPtr, Source);

                    MemOpChains2.push_back(CreateCopyOfByValArgument(Source, FIN,
                                ArgChain,
                                Flags, DAG, dl));
                } else {
                    // Store relative to framepointer.
                    MemOpChains2.push_back(
                            DAG.getStore(ArgChain, dl, Arg, FIN,
                                MachinePointerInfo::getFixedStack(FI),
                                false, false, 0));
                }
            }
        }

        if (!MemOpChains2.empty())
            Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                    &MemOpChains2[0], MemOpChains2.size());

        // Store the return address to the appropriate stack slot.
        Chain = EmitTailCallStoreRetAddr(DAG, MF, Chain, RetAddrFrIdx,
                getPointerTy(), RegInfo->getSlotSize(),
                FPDiff, dl);
    }

    // Build a sequence of copy-to-reg nodes chained together with token chain
    // and flag operands which copy the outgoing args into registers.
    SDValue InFlag;
    for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
        Chain = DAG.getCopyToReg(Chain, dl, RegsToPass[i].first,
                RegsToPass[i].second, InFlag);
        InFlag = Chain.getValue(1);
    }

    if (getTargetMachine().getCodeModel() == CodeModel::Large) {
        assert(Is64Bit && "Large code model is only legal in 64-bit mode.");
        // In the 64-bit large code model, we have to make all calls
        // through a register, since the call instruction's 32-bit
        // pc-relative offset may not be large enough to hold the whole
        // address.
    } else if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
        // If the callee is a GlobalAddress node (quite common, every direct call
        // is) turn it into a TargetGlobalAddress node so that legalize doesn't hack
        // it.

        // We should use extra load for direct calls to dllimported functions in
        // non-JIT mode.
        const GlobalValue *GV = G->getGlobal();
        if (!GV->hasDLLImportStorageClass()) {
            unsigned char OpFlags = 0;
            bool ExtraLoad = false;
            unsigned WrapperKind = ISD::DELETED_NODE;

            // On ELF targets, in both Cse523-64 and Cse523-32 mode, direct calls to
            // external symbols most go through the PLT in PIC mode.  If the symbol
            // has hidden or protected visibility, or if it is static or local, then
            // we don't need to use the PLT - we can directly call it.
            if (Subtarget->isTargetELF() &&
                    getTargetMachine().getRelocationModel() == Reloc::PIC_ &&
                    GV->hasDefaultVisibility() && !GV->hasLocalLinkage()) {
                OpFlags = Cse523II::MO_PLT;
            } else if (Subtarget->isPICStyleStubAny() &&
                    (GV->isDeclaration() || GV->isWeakForLinker()) &&
                    (!Subtarget->getTargetTriple().isMacOSX() ||
                     Subtarget->getTargetTriple().isMacOSXVersionLT(10, 5))) {
                // PC-relative references to external symbols should go through $stub,
                // unless we're building with the leopard linker or later, which
                // automatically synthesizes these stubs.
                OpFlags = Cse523II::MO_DARWIN_STUB;
            } else if (Subtarget->isPICStyleRIPRel() &&
                    isa<Function>(GV) &&
                    cast<Function>(GV)->getAttributes().
                    hasAttribute(AttributeSet::FunctionIndex,
                        Attribute::NonLazyBind)) {
                // If the function is marked as non-lazy, generate an indirect call
                // which loads from the GOT directly. This avoids runtime overhead
                // at the cost of eager binding (and one extra byte of encoding).
                OpFlags = Cse523II::MO_GOTPCREL;
                WrapperKind = Cse523ISD::WrapperRIP;
                ExtraLoad = true;
            }

            Callee = DAG.getTargetGlobalAddress(GV, dl, getPointerTy(),
                    G->getOffset(), OpFlags);

            // Add a wrapper if needed.
            if (WrapperKind != ISD::DELETED_NODE)
                Callee = DAG.getNode(Cse523ISD::WrapperRIP, dl, getPointerTy(), Callee);
            // Add extra indirection if needed.
            if (ExtraLoad)
                Callee = DAG.getLoad(getPointerTy(), dl, DAG.getEntryNode(), Callee,
                        MachinePointerInfo::getGOT(),
                        false, false, false, 0);
        }
    } else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee)) {
        unsigned char OpFlags = 0;

        // On ELF targets, in either Cse523-64 or Cse523-32 mode, direct calls to
        // external symbols should go through the PLT.
        if (Subtarget->isTargetELF() &&
                getTargetMachine().getRelocationModel() == Reloc::PIC_) {
            OpFlags = Cse523II::MO_PLT;
        } else if (Subtarget->isPICStyleStubAny() &&
                (!Subtarget->getTargetTriple().isMacOSX() ||
                 Subtarget->getTargetTriple().isMacOSXVersionLT(10, 5))) {
            // PC-relative references to external symbols should go through $stub,
            // unless we're building with the leopard linker or later, which
            // automatically synthesizes these stubs.
            OpFlags = Cse523II::MO_DARWIN_STUB;
        }

        Callee = DAG.getTargetExternalSymbol(S->getSymbol(), getPointerTy(),
                OpFlags);
    }

    // Returns a chain & a flag for retval copy to use.
    SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
    SmallVector<SDValue, 8> Ops;

    if (!IsSibcall && isTailCall) {
        Chain = DAG.getCALLSEQ_END(Chain,
                DAG.getIntPtrConstant(NumBytesToPop, true),
                DAG.getIntPtrConstant(0, true), InFlag, dl);
        InFlag = Chain.getValue(1);
    }

    Ops.push_back(Chain);
    Ops.push_back(Callee);

    if (isTailCall)
        Ops.push_back(DAG.getConstant(FPDiff, MVT::i64));

    // Add argument registers to the end of the list so that they are known live
    // into the call.
    for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
        Ops.push_back(DAG.getRegister(RegsToPass[i].first,
                    RegsToPass[i].second.getValueType()));

    // Add a register mask operand representing the call-preserved registers.
    const TargetRegisterInfo *TRI = getTargetMachine().getRegisterInfo();
    const uint32_t *Mask = TRI->getCallPreservedMask(CallConv);
    assert(Mask && "Missing call preserved mask for calling convention");
    Ops.push_back(DAG.getRegisterMask(Mask));

    if (InFlag.getNode())
        Ops.push_back(InFlag);

    if (isTailCall) {
        // We used to do:
        //// If this is the first return lowered for this function, add the regs
        //// to the liveout set for the function.
        // This isn't right, although it's probably harmless on cse523; liveouts
        // should be computed from returns not tail calls.  Consider a void
        // function making a tail call to a function returning int.
        return DAG.getNode(Cse523ISD::TC_RETURN, dl, NodeTys, &Ops[0], Ops.size());
    }

    Chain = DAG.getNode(Cse523ISD::CALL, dl, NodeTys, &Ops[0], Ops.size());
    InFlag = Chain.getValue(1);

    // Create the CALLSEQ_END node.
    unsigned NumBytesForCalleeToPop;
    if (Cse523::isCalleePop(CallConv, Is64Bit, isVarArg,
                getTargetMachine().Options.GuaranteedTailCallOpt))
        NumBytesForCalleeToPop = NumBytes;    // Callee pops everything
    else if (!Is64Bit && !IsTailCallConvention(CallConv) &&
            !Subtarget->getTargetTriple().isOSMSVCRT() &&
            SR == StackStructReturn)
        // If this is a call to a struct-return function, the callee
        // pops the hidden struct pointer, so we have to push it back.
        // This is common for Darwin/Cse523, Linux & Mingw32 targets.
        // For MSVC Win32 targets, the caller pops the hidden struct pointer.
        NumBytesForCalleeToPop = 4;
    else
        NumBytesForCalleeToPop = 0;  // Callee pops nothing.

    // Returns a flag for retval copy to use.
    if (!IsSibcall) {
        Chain = DAG.getCALLSEQ_END(Chain,
                DAG.getIntPtrConstant(NumBytesToPop, true),
                DAG.getIntPtrConstant(NumBytesForCalleeToPop,
                    true),
                InFlag, dl);
        InFlag = Chain.getValue(1);
    }

    // Handle result values, copying them out of physregs into vregs that we
    // return.
    return LowerCallResult(Chain, InFlag, CallConv, isVarArg,
            Ins, dl, DAG, InVals);
}

//===----------------------------------------------------------------------===//
//                Fast Calling Convention (tail call) implementation
//===----------------------------------------------------------------------===//

//  Like std call, callee cleans arguments, convention except that ECX is
//  reserved for storing the tail called function address. Only 2 registers are
//  free for argument passing (inreg). Tail call optimization is performed
//  provided:
//                * tailcallopt is enabled
//                * caller/callee are fastcc
//  On Cse523_64 architecture with GOT-style position independent code only local
//  (within module) calls are supported at the moment.
//  To keep the stack aligned according to platform abi the function
//  GetAlignedArgumentStackSize ensures that argument delta is always multiples
//  of stack alignment. (Dynamic linkers need this - darwin's dyld for example)
//  If a tail called function callee has more arguments than the caller the
//  caller needs to make sure that there is room to move the RETADDR to. This is
//  achieved by reserving an area the size of the argument delta right after the
//  original REtADDR, but before the saved framepointer or the spilled registers
//  e.g. caller(arg1, arg2) calls callee(arg1, arg2,arg3,arg4)
//  stack layout:
//    arg1
//    arg2
//    RETADDR
//    [ new RETADDR
//      move area ]
//    (possible EBP)
//    ESI
//    EDI
//    local1 ..

/// GetAlignedArgumentStackSize - Make the stack size align e.g 16n + 12 aligned
/// for a 16 byte align requirement.
unsigned
Cse523TargetLowering::GetAlignedArgumentStackSize(unsigned StackSize,
        SelectionDAG& DAG) const {
    MachineFunction &MF = DAG.getMachineFunction();
    const TargetMachine &TM = MF.getTarget();
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(TM.getRegisterInfo());
    const TargetFrameLowering &TFI = *TM.getFrameLowering();
    unsigned StackAlignment = TFI.getStackAlignment();
    uint64_t AlignMask = StackAlignment - 1;
    int64_t Offset = StackSize;
    unsigned SlotSize = RegInfo->getSlotSize();
    if ( (Offset & AlignMask) <= (StackAlignment - SlotSize) ) {
        // Number smaller than 12 so just add the difference.
        Offset += ((StackAlignment - SlotSize) - (Offset & AlignMask));
    } else {
        // Mask out lower bits, add stackalignment once plus the 12 bytes.
        Offset = ((~AlignMask) & Offset) + StackAlignment +
            (StackAlignment-SlotSize);
    }
    return Offset;
}

/// MatchingStackOffset - Return true if the given stack call argument is
/// already available in the same position (relatively) of the caller's
/// incoming argument stack.
static
bool MatchingStackOffset(SDValue Arg, unsigned Offset, ISD::ArgFlagsTy Flags,
        MachineFrameInfo *MFI, const MachineRegisterInfo *MRI,
        const Cse523InstrInfo *TII) {
    unsigned Bytes = Arg.getValueType().getSizeInBits() / 8;
    int FI = INT_MAX;
    if (Arg.getOpcode() == ISD::CopyFromReg) {
        unsigned VR = cast<RegisterSDNode>(Arg.getOperand(1))->getReg();
        if (!TargetRegisterInfo::isVirtualRegister(VR))
            return false;
        MachineInstr *Def = MRI->getVRegDef(VR);
        if (!Def)
            return false;
        if (!Flags.isByVal()) {
            if (!TII->isLoadFromStackSlot(Def, FI))
                return false;
        } else {
            unsigned Opcode = Def->getOpcode();
            if ((Opcode == Cse523::LEA64r) &&
                    Def->getOperand(1).isFI()) {
                FI = Def->getOperand(1).getIndex();
                Bytes = Flags.getByValSize();
            } else
                return false;
        }
    } else if (LoadSDNode *Ld = dyn_cast<LoadSDNode>(Arg)) {
        if (Flags.isByVal())
            // ByVal argument is passed in as a pointer but it's now being
            // dereferenced. e.g.
            // define @foo(%struct.X* %A) {
            //   tail call @bar(%struct.X* byval %A)
            // }
            return false;
        SDValue Ptr = Ld->getBasePtr();
        FrameIndexSDNode *FINode = dyn_cast<FrameIndexSDNode>(Ptr);
        if (!FINode)
            return false;
        FI = FINode->getIndex();
    } else if (Arg.getOpcode() == ISD::FrameIndex && Flags.isByVal()) {
        FrameIndexSDNode *FINode = cast<FrameIndexSDNode>(Arg);
        FI = FINode->getIndex();
        Bytes = Flags.getByValSize();
    } else
        return false;

    assert(FI != INT_MAX);
    if (!MFI->isFixedObjectIndex(FI))
        return false;
    return Offset == MFI->getObjectOffset(FI) && Bytes == MFI->getObjectSize(FI);
}

/// IsEligibleForTailCallOptimization - Check whether the call is eligible
/// for tail call optimization. Targets which want to do tail call
/// optimization should implement this function.
bool
Cse523TargetLowering::IsEligibleForTailCallOptimization(SDValue Callee,
        CallingConv::ID CalleeCC,
        bool isVarArg,
        bool isCalleeStructRet,
        bool isCallerStructRet,
        Type *RetTy,
        const SmallVectorImpl<ISD::OutputArg> &Outs,
        const SmallVectorImpl<SDValue> &OutVals,
        const SmallVectorImpl<ISD::InputArg> &Ins,
        SelectionDAG &DAG) const {
    if (!IsTailCallConvention(CalleeCC) && !IsCCallConvention(CalleeCC))
        return false;

    // If -tailcallopt is specified, make fastcc functions tail-callable.
    const MachineFunction &MF = DAG.getMachineFunction();
    const Function *CallerF = MF.getFunction();

    CallingConv::ID CallerCC = CallerF->getCallingConv();
    bool CCMatch = CallerCC == CalleeCC;
    bool IsCalleeWin64 = Subtarget->isCallingConvWin64(CalleeCC);
    bool IsCallerWin64 = Subtarget->isCallingConvWin64(CallerCC);

    if (getTargetMachine().Options.GuaranteedTailCallOpt) {
        if (IsTailCallConvention(CalleeCC) && CCMatch)
            return true;
        return false;
    }

    // Look for obvious safe cases to perform tail call optimization that do not
    // require ABI changes. This is what gcc calls sibcall.

    // Can't do sibcall if stack needs to be dynamically re-aligned. PEI needs to
    // emit a special epilogue.
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    if (RegInfo->needsStackRealignment(MF))
        return false;

    // Also avoid sibcall optimization if either caller or callee uses struct
    // return semantics.
    if (isCalleeStructRet || isCallerStructRet)
        return false;

    // An stdcall/thiscall caller is expected to clean up its arguments; the
    // callee isn't going to do that.
    // FIXME: this is more restrictive than needed. We could produce a tailcall
    // when the stack adjustment matches. For example, with a thiscall that takes
    // only one argument.
    if (!CCMatch && (CallerCC == CallingConv::X86_StdCall ||
                CallerCC == CallingConv::X86_ThisCall))
        return false;

    // Do not sibcall optimize vararg calls unless all arguments are passed via
    // registers.
    if (isVarArg && !Outs.empty()) {

        // Optimizing for varargs on Win64 is unlikely to be safe without
        // additional testing.
        if (IsCalleeWin64 || IsCallerWin64)
            return false;

        SmallVector<CCValAssign, 16> ArgLocs;
        CCState CCInfo(CalleeCC, isVarArg, DAG.getMachineFunction(),
                getTargetMachine(), ArgLocs, *DAG.getContext());

        CCInfo.AnalyzeCallOperands(Outs, CC_Cse523);
        for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i)
            if (!ArgLocs[i].isRegLoc())
                return false;
    }

    // If the call result is in ST0 / ST1, it needs to be popped off the x87
    // stack.  Therefore, if it's not used by the call it is not safe to optimize
    // this into a sibcall.
    bool Unused = false;
    for (unsigned i = 0, e = Ins.size(); i != e; ++i) {
        if (!Ins[i].Used) {
            Unused = true;
            break;
        }
    }
    if (Unused) {
        SmallVector<CCValAssign, 16> RVLocs;
        CCState CCInfo(CalleeCC, false, DAG.getMachineFunction(),
                getTargetMachine(), RVLocs, *DAG.getContext());
        CCInfo.AnalyzeCallResult(Ins, RetCC_Cse523);
        for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
            CCValAssign &VA = RVLocs[i];
        }
    }

    // If the calling conventions do not match, then we'd better make sure the
    // results are returned in the same way as what the caller expects.
    if (!CCMatch) {
        SmallVector<CCValAssign, 16> RVLocs1;
        CCState CCInfo1(CalleeCC, false, DAG.getMachineFunction(),
                getTargetMachine(), RVLocs1, *DAG.getContext());
        CCInfo1.AnalyzeCallResult(Ins, RetCC_Cse523);

        SmallVector<CCValAssign, 16> RVLocs2;
        CCState CCInfo2(CallerCC, false, DAG.getMachineFunction(),
                getTargetMachine(), RVLocs2, *DAG.getContext());
        CCInfo2.AnalyzeCallResult(Ins, RetCC_Cse523);

        if (RVLocs1.size() != RVLocs2.size())
            return false;
        for (unsigned i = 0, e = RVLocs1.size(); i != e; ++i) {
            if (RVLocs1[i].isRegLoc() != RVLocs2[i].isRegLoc())
                return false;
            if (RVLocs1[i].getLocInfo() != RVLocs2[i].getLocInfo())
                return false;
            if (RVLocs1[i].isRegLoc()) {
                if (RVLocs1[i].getLocReg() != RVLocs2[i].getLocReg())
                    return false;
            } else {
                if (RVLocs1[i].getLocMemOffset() != RVLocs2[i].getLocMemOffset())
                    return false;
            }
        }
    }

    // If the callee takes no arguments then go on to check the results of the
    // call.
    if (!Outs.empty()) {
        // Check if stack adjustment is needed. For now, do not do this if any
        // argument is passed on the stack.
        SmallVector<CCValAssign, 16> ArgLocs;
        CCState CCInfo(CalleeCC, isVarArg, DAG.getMachineFunction(),
                getTargetMachine(), ArgLocs, *DAG.getContext());

        // Allocate shadow area for Win64
        if (IsCalleeWin64)
            CCInfo.AllocateStack(32, 8);

        CCInfo.AnalyzeCallOperands(Outs, CC_Cse523);
        if (CCInfo.getNextStackOffset()) {
            MachineFunction &MF = DAG.getMachineFunction();
            if (MF.getInfo<Cse523MachineFunctionInfo>()->getBytesToPopOnReturn())
                return false;

            // Check if the arguments are already laid out in the right way as
            // the caller's fixed stack objects.
            MachineFrameInfo *MFI = MF.getFrameInfo();
            const MachineRegisterInfo *MRI = &MF.getRegInfo();
            const Cse523InstrInfo *TII =
                ((const Cse523TargetMachine&)getTargetMachine()).getInstrInfo();
            for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
                CCValAssign &VA = ArgLocs[i];
                SDValue Arg = OutVals[i];
                ISD::ArgFlagsTy Flags = Outs[i].Flags;
                if (VA.getLocInfo() == CCValAssign::Indirect)
                    return false;
                if (!VA.isRegLoc()) {
                    if (!MatchingStackOffset(Arg, VA.getLocMemOffset(), Flags,
                                MFI, MRI, TII))
                        return false;
                }
            }
        }

        // If the tailcall address may be in a register, then make sure it's
        // possible to register allocate for it. In 32-bit, the call address can
        // only target EAX, EDX, or ECX since the tail call must be scheduled after
        // callee-saved registers are restored. These happen to be the same
        // registers used to pass 'inreg' arguments so watch out for those.
        if (!Subtarget->is64Bit() &&
                ((!isa<GlobalAddressSDNode>(Callee) &&
                  !isa<ExternalSymbolSDNode>(Callee)) ||
                 getTargetMachine().getRelocationModel() == Reloc::PIC_)) {
            unsigned NumInRegs = 0;
            // In PIC we need an extra register to formulate the address computation
            // for the callee.
            unsigned MaxInRegs =
                (getTargetMachine().getRelocationModel() == Reloc::PIC_) ? 2 : 3;

            for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
                CCValAssign &VA = ArgLocs[i];
                if (!VA.isRegLoc())
                    continue;
                unsigned Reg = VA.getLocReg();
                switch (Reg) {
                    default: break;
                }
            }
        }
    }

    return true;
}

FastISel *
Cse523TargetLowering::createFastISel(FunctionLoweringInfo &funcInfo,
        const TargetLibraryInfo *libInfo) const {
    return Cse523::createFastISel(funcInfo, libInfo);
}

//===----------------------------------------------------------------------===//
//                           Other Lowering Hooks
//===----------------------------------------------------------------------===//

static bool MayFoldLoad(SDValue Op) {
    return Op.hasOneUse() && ISD::isNormalLoad(Op.getNode());
}

static bool MayFoldIntoStore(SDValue Op) {
    return Op.hasOneUse() && ISD::isNormalStore(*Op.getNode()->use_begin());
}

static bool isTargetShuffle(unsigned Opcode) {
    switch(Opcode) {
        default: return false;
        case Cse523ISD::PSHUFD:
        case Cse523ISD::PSHUFHW:
        case Cse523ISD::PSHUFLW:
        case Cse523ISD::SHUFP:
        case Cse523ISD::PALIGNR:
        case Cse523ISD::MOVLHPS:
        case Cse523ISD::MOVLHPD:
        case Cse523ISD::MOVHLPS:
        case Cse523ISD::MOVLPS:
        case Cse523ISD::MOVLPD:
        case Cse523ISD::MOVSHDUP:
        case Cse523ISD::MOVSLDUP:
        case Cse523ISD::MOVDDUP:
        case Cse523ISD::MOVSS:
        case Cse523ISD::MOVSD:
        case Cse523ISD::UNPCKL:
        case Cse523ISD::UNPCKH:
        case Cse523ISD::VPERMILP:
        case Cse523ISD::VPERM2X128:
        case Cse523ISD::VPERMI:
                 return true;
    }
}

static SDValue getTargetShuffleNode(unsigned Opc, SDLoc dl, EVT VT,
        SDValue V1, SelectionDAG &DAG) {
    switch(Opc) {
        default: llvm_unreachable("Unknown cse523 shuffle node");
        case Cse523ISD::MOVSHDUP:
        case Cse523ISD::MOVSLDUP:
        case Cse523ISD::MOVDDUP:
                 return DAG.getNode(Opc, dl, VT, V1);
    }
}

static SDValue getTargetShuffleNode(unsigned Opc, SDLoc dl, EVT VT,
        SDValue V1, unsigned TargetMask,
        SelectionDAG &DAG) {
    switch(Opc) {
        default: llvm_unreachable("Unknown cse523 shuffle node");
//        case Cse523ISD::PSHUFD:
//        case Cse523ISD::PSHUFHW:
//        case Cse523ISD::PSHUFLW:
//        case Cse523ISD::VPERMILP:
//        case Cse523ISD::VPERMI:
//                 return DAG.getNode(Opc, dl, VT, V1, DAG.getConstant(TargetMask, MVT::i8));
    }
}

static SDValue getTargetShuffleNode(unsigned Opc, SDLoc dl, EVT VT,
        SDValue V1, SDValue V2, unsigned TargetMask,
        SelectionDAG &DAG) {
    switch(Opc) {
        default: llvm_unreachable("Unknown cse523 shuffle node");
//        case Cse523ISD::PALIGNR:
//        case Cse523ISD::SHUFP:
//        case Cse523ISD::VPERM2X128:
//                 return DAG.getNode(Opc, dl, VT, V1, V2,
//                         DAG.getConstant(TargetMask, MVT::i8));
    }
}

static SDValue getTargetShuffleNode(unsigned Opc, SDLoc dl, EVT VT,
        SDValue V1, SDValue V2, SelectionDAG &DAG) {
    switch(Opc) {
        default: llvm_unreachable("Unknown cse523 shuffle node");
        case Cse523ISD::MOVLHPS:
        case Cse523ISD::MOVLHPD:
        case Cse523ISD::MOVHLPS:
        case Cse523ISD::MOVLPS:
        case Cse523ISD::MOVLPD:
        case Cse523ISD::MOVSS:
        case Cse523ISD::MOVSD:
        case Cse523ISD::UNPCKL:
        case Cse523ISD::UNPCKH:
                 return DAG.getNode(Opc, dl, VT, V1, V2);
    }
}

SDValue Cse523TargetLowering::getReturnAddressFrameIndex(SelectionDAG &DAG) const {
    MachineFunction &MF = DAG.getMachineFunction();
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();
    int ReturnAddrIndex = FuncInfo->getRAIndex();

    if (ReturnAddrIndex == 0) {
        // Set up a frame object for the return address.
        unsigned SlotSize = RegInfo->getSlotSize();
        ReturnAddrIndex = MF.getFrameInfo()->CreateFixedObject(SlotSize,
                -(int64_t)SlotSize,
                false);
        FuncInfo->setRAIndex(ReturnAddrIndex);
    }

    return DAG.getFrameIndex(ReturnAddrIndex, getPointerTy());
}

bool Cse523::isOffsetSuitableForCodeModel(int64_t Offset, CodeModel::Model M,
        bool hasSymbolicDisplacement) {
    // Offset should fit into 32 bit immediate field.
    if (!isInt<32>(Offset))
        return false;

    // If we don't have a symbolic displacement - we don't have any extra
    // restrictions.
    if (!hasSymbolicDisplacement)
        return true;

    // FIXME: Some tweaks might be needed for medium code model.
    if (M != CodeModel::Small && M != CodeModel::Kernel)
        return false;

    // For small code model we assume that latest object is 16MB before end of 31
    // bits boundary. We may also accept pretty large negative constants knowing
    // that all objects are in the positive half of address space.
    if (M == CodeModel::Small && Offset < 16*1024*1024)
        return true;

    // For kernel code model we know that all object resist in the negative half
    // of 32bits address space. We may not accept negative offsets, since they may
    // be just off and we may accept pretty large positive ones.
    if (M == CodeModel::Kernel && Offset > 0)
        return true;

    return false;
}

/// isCalleePop - Determines whether the callee is required to pop its
/// own arguments. Callee pop is necessary to support tail calls.
bool Cse523::isCalleePop(CallingConv::ID CallingConv,
        bool is64Bit, bool IsVarArg, bool TailCallOpt) {
    if (IsVarArg)
        return false;

    switch (CallingConv) {
        default:
            return false;
        case CallingConv::X86_StdCall:
            return !is64Bit;
        case CallingConv::X86_FastCall:
            return !is64Bit;
        case CallingConv::X86_ThisCall:
            return !is64Bit;
        case CallingConv::Fast:
            return TailCallOpt;
        case CallingConv::GHC:
            return TailCallOpt;
        case CallingConv::HiPE:
            return TailCallOpt;
    }
}

/// \brief Return true if the condition is an unsigned comparison operation.
static bool isCse523CCUnsigned(unsigned Cse523CC) {
    switch (Cse523CC) {
        default: llvm_unreachable("Invalid integer condition!");
        case Cse523::COND_E:     return true;
        case Cse523::COND_G:     return false;
        case Cse523::COND_GE:    return false;
        case Cse523::COND_L:     return false;
        case Cse523::COND_LE:    return false;
        case Cse523::COND_NE:    return true;
        case Cse523::COND_B:     return true;
        case Cse523::COND_A:     return true;
        case Cse523::COND_BE:    return true;
        case Cse523::COND_AE:    return true;
    }
    llvm_unreachable("covered switch fell through?!");
}

/// TranslateCse523CC - do a one to one translation of a ISD::CondCode to the Cse523
/// specific condition code, returning the condition code and the LHS/RHS of the
/// comparison to make.
static unsigned TranslateCse523CC(ISD::CondCode SetCCOpcode, bool isFP,
        SDValue &LHS, SDValue &RHS, SelectionDAG &DAG) {
    assert(!isFP && "Unsupported Floating Point Conditions");
    if (ConstantSDNode *RHSC = dyn_cast<ConstantSDNode>(RHS)) {
        if (SetCCOpcode == ISD::SETGT && RHSC->isAllOnesValue()) {
            // X > -1   -> X == 0, jump !sign.
            RHS = DAG.getConstant(0, RHS.getValueType());
            return Cse523::COND_NS;
        }
        if (SetCCOpcode == ISD::SETLT && RHSC->isNullValue()) {
            // X < 0   -> X == 0, jump on sign.
            return Cse523::COND_S;
        }
        if (SetCCOpcode == ISD::SETLT && RHSC->getZExtValue() == 1) {
            // X < 1   -> X <= 0
            RHS = DAG.getConstant(0, RHS.getValueType());
            return Cse523::COND_LE;
        }
    }

    switch (SetCCOpcode) {
        default: llvm_unreachable("Invalid integer condition!");
        case ISD::SETEQ:  return Cse523::COND_E;
        case ISD::SETGT:  return Cse523::COND_G;
        case ISD::SETGE:  return Cse523::COND_GE;
        case ISD::SETLT:  return Cse523::COND_L;
        case ISD::SETLE:  return Cse523::COND_LE;
        case ISD::SETNE:  return Cse523::COND_NE;
        case ISD::SETULT: return Cse523::COND_B;
        case ISD::SETUGT: return Cse523::COND_A;
        case ISD::SETULE: return Cse523::COND_BE;
        case ISD::SETUGE: return Cse523::COND_AE;
    }
}

/// hasFPCMov - is there a floating point cmov for the specific Cse523 condition
/// code. Current cse523 isa includes the following FP cmov instructions:
/// fcmovb, fcomvbe, fcomve, fcmovu, fcmovae, fcmova, fcmovne, fcmovnu.
static bool hasFPCMov(unsigned Cse523CC) {
    switch (Cse523CC) {
        default:
            return false;
        case Cse523::COND_B:
        case Cse523::COND_BE:
        case Cse523::COND_E:
        case Cse523::COND_P:
        case Cse523::COND_A:
        case Cse523::COND_AE:
        case Cse523::COND_NE:
        case Cse523::COND_NP:
            return true;
    }
}

/// isFPImmLegal - Returns true if the target can instruction select the
/// specified FP immediate natively. If false, the legalizer will
/// materialize the FP immediate as a load from a constant pool.
bool Cse523TargetLowering::isFPImmLegal(const APFloat &Imm, EVT VT) const {
    for (unsigned i = 0, e = LegalFPImmediates.size(); i != e; ++i) {
        if (Imm.bitwiseIsEqual(LegalFPImmediates[i]))
            return true;
    }
    return false;
}

/// \brief Returns true if it is beneficial to convert a load of a constant
/// to just the constant itself.
bool Cse523TargetLowering::shouldConvertConstantLoadToIntImm(const APInt &Imm,
        Type *Ty) const {
    assert(Ty->isIntegerTy());

    unsigned BitSize = Ty->getPrimitiveSizeInBits();
    if (BitSize == 0 || BitSize > 64)
        return false;
    return true;
}

/// isUndefOrInRange - Return true if Val is undef or if its value falls within
/// the specified range (L, H].
static bool isUndefOrInRange(int Val, int Low, int Hi) {
    return (Val < 0) || (Val >= Low && Val < Hi);
}

/// isUndefOrEqual - Val is either less than zero (undef) or equal to the
/// specified value.
static bool isUndefOrEqual(int Val, int CmpVal) {
    return (Val < 0 || Val == CmpVal);
}

/// isSequentialOrUndefInRange - Return true if every element in Mask, beginning
/// from position Pos and ending in Pos+Size, falls within the specified
/// sequential range (L, L+Pos]. or is undef.
static bool isSequentialOrUndefInRange(ArrayRef<int> Mask,
        unsigned Pos, unsigned Size, int Low) {
    for (unsigned i = Pos, e = Pos+Size; i != e; ++i, ++Low)
        if (!isUndefOrEqual(Mask[i], Low))
            return false;
    return true;
}

/// isPSHUFDMask - Return true if the node specifies a shuffle of elements that
/// is suitable for input to PSHUFD or PSHUFW.  That is, it doesn't reference
/// the second operand.
static bool isPSHUFDMask(ArrayRef<int> Mask, MVT VT) {
    if (VT == MVT::v4f32 || VT == MVT::v4i32 )
        return (Mask[0] < 4 && Mask[1] < 4 && Mask[2] < 4 && Mask[3] < 4);
    if (VT == MVT::v2f64 || VT == MVT::v2i64)
        return (Mask[0] < 2 && Mask[1] < 2);
    return false;
}

/// isPSHUFHWMask - Return true if the node specifies a shuffle of elements that
/// is suitable for input to PSHUFHW.
static bool isPSHUFHWMask(ArrayRef<int> Mask, MVT VT, bool HasInt256) {
    if (VT != MVT::v8i16 && (!HasInt256 || VT != MVT::v16i16))
        return false;

    // Lower quadword copied in order or undef.
    if (!isSequentialOrUndefInRange(Mask, 0, 4, 0))
        return false;

    // Upper quadword shuffled.
    for (unsigned i = 4; i != 8; ++i)
        if (!isUndefOrInRange(Mask[i], 4, 8))
            return false;

    if (VT == MVT::v16i16) {
        // Lower quadword copied in order or undef.
        if (!isSequentialOrUndefInRange(Mask, 8, 4, 8))
            return false;

        // Upper quadword shuffled.
        for (unsigned i = 12; i != 16; ++i)
            if (!isUndefOrInRange(Mask[i], 12, 16))
                return false;
    }

    return true;
}

/// isPSHUFLWMask - Return true if the node specifies a shuffle of elements that
/// is suitable for input to PSHUFLW.
static bool isPSHUFLWMask(ArrayRef<int> Mask, MVT VT, bool HasInt256) {
    if (VT != MVT::v8i16 && (!HasInt256 || VT != MVT::v16i16))
        return false;

    // Upper quadword copied in order.
    if (!isSequentialOrUndefInRange(Mask, 4, 4, 4))
        return false;

    // Lower quadword shuffled.
    for (unsigned i = 0; i != 4; ++i)
        if (!isUndefOrInRange(Mask[i], 0, 4))
            return false;

    if (VT == MVT::v16i16) {
        // Upper quadword copied in order.
        if (!isSequentialOrUndefInRange(Mask, 12, 4, 12))
            return false;

        // Lower quadword shuffled.
        for (unsigned i = 8; i != 12; ++i)
            if (!isUndefOrInRange(Mask[i], 8, 12))
                return false;
    }

    return true;
}

/// isPALIGNRMask - Return true if the node specifies a shuffle of elements that
/// is suitable for input to PALIGNR.
static bool isPALIGNRMask(ArrayRef<int> Mask, MVT VT,
        const Cse523Subtarget *Subtarget) {
    if ((VT.is128BitVector() && !Subtarget->hasSSSE3()) ||
            (VT.is256BitVector() && !Subtarget->hasInt256()))
        return false;

    unsigned NumElts = VT.getVectorNumElements();
    unsigned NumLanes = VT.is512BitVector() ? 1: VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts/NumLanes;

    // Do not handle 64-bit element shuffles with palignr.
    if (NumLaneElts == 2)
        return false;

    for (unsigned l = 0; l != NumElts; l+=NumLaneElts) {
        unsigned i;
        for (i = 0; i != NumLaneElts; ++i) {
            if (Mask[i+l] >= 0)
                break;
        }

        // Lane is all undef, go to next lane
        if (i == NumLaneElts)
            continue;

        int Start = Mask[i+l];

        // Make sure its in this lane in one of the sources
        if (!isUndefOrInRange(Start, l, l+NumLaneElts) &&
                !isUndefOrInRange(Start, l+NumElts, l+NumElts+NumLaneElts))
            return false;

        // If not lane 0, then we must match lane 0
        if (l != 0 && Mask[i] >= 0 && !isUndefOrEqual(Start, Mask[i]+l))
            return false;

        // Correct second source to be contiguous with first source
        if (Start >= (int)NumElts)
            Start -= NumElts - NumLaneElts;

        // Make sure we're shifting in the right direction.
        if (Start <= (int)(i+l))
            return false;

        Start -= i;

        // Check the rest of the elements to see if they are consecutive.
        for (++i; i != NumLaneElts; ++i) {
            int Idx = Mask[i+l];

            // Make sure its in this lane
            if (!isUndefOrInRange(Idx, l, l+NumLaneElts) &&
                    !isUndefOrInRange(Idx, l+NumElts, l+NumElts+NumLaneElts))
                return false;

            // If not lane 0, then we must match lane 0
            if (l != 0 && Mask[i] >= 0 && !isUndefOrEqual(Idx, Mask[i]+l))
                return false;

            if (Idx >= (int)NumElts)
                Idx -= NumElts - NumLaneElts;

            if (!isUndefOrEqual(Idx, Start+i))
                return false;

        }
    }

    return true;
}

/// CommuteVectorShuffleMask - Change values in a shuffle permute mask assuming
/// the two vector operands have swapped position.
static void CommuteVectorShuffleMask(SmallVectorImpl<int> &Mask,
        unsigned NumElems) {
    for (unsigned i = 0; i != NumElems; ++i) {
        int idx = Mask[i];
        if (idx < 0)
            continue;
        else if (idx < (int)NumElems)
            Mask[i] = idx + NumElems;
        else
            Mask[i] = idx - NumElems;
    }
}

/// isSHUFPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to 128/256-bit
/// SHUFPS and SHUFPD. If Commuted is true, then it checks for sources to be
/// reverse of what cse523 shuffles want.
static bool isSHUFPMask(ArrayRef<int> Mask, MVT VT, bool Commuted = false) {

    unsigned NumElems = VT.getVectorNumElements();
    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned NumLaneElems = NumElems/NumLanes;

    if (NumLaneElems != 2 && NumLaneElems != 4)
        return false;

    unsigned EltSize = VT.getVectorElementType().getSizeInBits();
    bool symetricMaskRequired =
        (VT.getSizeInBits() >= 256) && (EltSize == 32);

    // VSHUFPSY divides the resulting vector into 4 chunks.
    // The sources are also splitted into 4 chunks, and each destination
    // chunk must come from a different source chunk.
    //
    //  SRC1 =>   X7    X6    X5    X4    X3    X2    X1    X0
    //  SRC2 =>   Y7    Y6    Y5    Y4    Y3    Y2    Y1    Y9
    //
    //  DST  =>  Y7..Y4,   Y7..Y4,   X7..X4,   X7..X4,
    //           Y3..Y0,   Y3..Y0,   X3..X0,   X3..X0
    //
    // VSHUFPDY divides the resulting vector into 4 chunks.
    // The sources are also splitted into 4 chunks, and each destination
    // chunk must come from a different source chunk.
    //
    //  SRC1 =>      X3       X2       X1       X0
    //  SRC2 =>      Y3       Y2       Y1       Y0
    //
    //  DST  =>  Y3..Y2,  X3..X2,  Y1..Y0,  X1..X0
    //
    SmallVector<int, 4> MaskVal(NumLaneElems, -1);
    unsigned HalfLaneElems = NumLaneElems/2;
    for (unsigned l = 0; l != NumElems; l += NumLaneElems) {
        for (unsigned i = 0; i != NumLaneElems; ++i) {
            int Idx = Mask[i+l];
            unsigned RngStart = l + ((Commuted == (i<HalfLaneElems)) ? NumElems : 0);
            if (!isUndefOrInRange(Idx, RngStart, RngStart+NumLaneElems))
                return false;
            // For VSHUFPSY, the mask of the second half must be the same as the
            // first but with the appropriate offsets. This works in the same way as
            // VPERMILPS works with masks.
            if (!symetricMaskRequired || Idx < 0)
                continue;
            if (MaskVal[i] < 0) {
                MaskVal[i] = Idx - l;
                continue;
            }
            if ((signed)(Idx - l) != MaskVal[i])
                return false;
        }
    }

    return true;
}

/// isMOVHLPSMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVHLPS.
static bool isMOVHLPSMask(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if (NumElems != 4)
        return false;

    // Expect bit0 == 6, bit1 == 7, bit2 == 2, bit3 == 3
    return isUndefOrEqual(Mask[0], 6) &&
        isUndefOrEqual(Mask[1], 7) &&
        isUndefOrEqual(Mask[2], 2) &&
        isUndefOrEqual(Mask[3], 3);
}

/// isMOVHLPS_v_undef_Mask - Special case of isMOVHLPSMask for canonical form
/// of vector_shuffle v, v, <2, 3, 2, 3>, i.e. vector_shuffle v, undef,
/// <2, 3, 2, 3>
static bool isMOVHLPS_v_undef_Mask(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if (NumElems != 4)
        return false;

    return isUndefOrEqual(Mask[0], 2) &&
        isUndefOrEqual(Mask[1], 3) &&
        isUndefOrEqual(Mask[2], 2) &&
        isUndefOrEqual(Mask[3], 3);
}

/// isMOVLPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVLP{S|D}.
static bool isMOVLPMask(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if (NumElems != 2 && NumElems != 4)
        return false;

    for (unsigned i = 0, e = NumElems/2; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i + NumElems))
            return false;

    for (unsigned i = NumElems/2, e = NumElems; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i))
            return false;

    return true;
}

/// isMOVLHPSMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVLHPS.
static bool isMOVLHPSMask(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if (NumElems != 2 && NumElems != 4)
        return false;

    for (unsigned i = 0, e = NumElems/2; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i))
            return false;

    for (unsigned i = 0, e = NumElems/2; i != e; ++i)
        if (!isUndefOrEqual(Mask[i + e], i + NumElems))
            return false;

    return true;
}

//
// Some special combinations that can be optimized.
//
static
SDValue Compact8x32ShuffleNode(ShuffleVectorSDNode *SVOp,
        SelectionDAG &DAG) {
    MVT VT = SVOp->getSimpleValueType(0);
    SDLoc dl(SVOp);

    if (VT != MVT::v8i32 && VT != MVT::v8f32)
        return SDValue();

    ArrayRef<int> Mask = SVOp->getMask();

    // These are the special masks that may be optimized.
    static const int MaskToOptimizeEven[] = {0, 8, 2, 10, 4, 12, 6, 14};
    static const int MaskToOptimizeOdd[]  = {1, 9, 3, 11, 5, 13, 7, 15};
    bool MatchEvenMask = true;
    bool MatchOddMask  = true;
    for (int i=0; i<8; ++i) {
        if (!isUndefOrEqual(Mask[i], MaskToOptimizeEven[i]))
            MatchEvenMask = false;
        if (!isUndefOrEqual(Mask[i], MaskToOptimizeOdd[i]))
            MatchOddMask = false;
    }

    if (!MatchEvenMask && !MatchOddMask)
        return SDValue();

    SDValue UndefNode = DAG.getNode(ISD::UNDEF, dl, VT);

    SDValue Op0 = SVOp->getOperand(0);
    SDValue Op1 = SVOp->getOperand(1);

    if (MatchEvenMask) {
        // Shift the second operand right to 32 bits.
        static const int ShiftRightMask[] = {-1, 0, -1, 2, -1, 4, -1, 6 };
        Op1 = DAG.getVectorShuffle(VT, dl, Op1, UndefNode, ShiftRightMask);
    } else {
        // Shift the first operand left to 32 bits.
        static const int ShiftLeftMask[] = {1, -1, 3, -1, 5, -1, 7, -1 };
        Op0 = DAG.getVectorShuffle(VT, dl, Op0, UndefNode, ShiftLeftMask);
    }
    static const int BlendMask[] = {0, 9, 2, 11, 4, 13, 6, 15};
    return DAG.getVectorShuffle(VT, dl, Op0, Op1, BlendMask);
}

/// isUNPCKLMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to UNPCKL.
static bool isUNPCKLMask(ArrayRef<int> Mask, MVT VT,
        bool HasInt256, bool V2IsSplat = false) {

    assert(VT.getSizeInBits() >= 128 &&
            "Unsupported vector type for unpckl");

    // AVX defines UNPCK* to operate independently on 128-bit lanes.
    unsigned NumLanes;
    unsigned NumOf256BitLanes;
    unsigned NumElts = VT.getVectorNumElements();
    if (VT.is256BitVector()) {
        if (NumElts != 4 && NumElts != 8 &&
                (!HasInt256 || (NumElts != 16 && NumElts != 32)))
            return false;
        NumLanes = 2;
        NumOf256BitLanes = 1;
    } else if (VT.is512BitVector()) {
        assert(VT.getScalarType().getSizeInBits() >= 32 &&
                "Unsupported vector type for unpckh");
        NumLanes = 2;
        NumOf256BitLanes = 2;
    } else {
        NumLanes = 1;
        NumOf256BitLanes = 1;
    }

    unsigned NumEltsInStride = NumElts/NumOf256BitLanes;
    unsigned NumLaneElts = NumEltsInStride/NumLanes;

    for (unsigned l256 = 0; l256 < NumOf256BitLanes; l256 += 1) {
        for (unsigned l = 0; l != NumEltsInStride; l += NumLaneElts) {
            for (unsigned i = 0, j = l; i != NumLaneElts; i += 2, ++j) {
                int BitI  = Mask[l256*NumEltsInStride+l+i];
                int BitI1 = Mask[l256*NumEltsInStride+l+i+1];
                if (!isUndefOrEqual(BitI, j+l256*NumElts))
                    return false;
                if (V2IsSplat && !isUndefOrEqual(BitI1, NumElts))
                    return false;
                if (!isUndefOrEqual(BitI1, j+l256*NumElts+NumEltsInStride))
                    return false;
            }
        }
    }
    return true;
}

/// isUNPCKHMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to UNPCKH.
static bool isUNPCKHMask(ArrayRef<int> Mask, MVT VT,
        bool HasInt256, bool V2IsSplat = false) {
    assert(VT.getSizeInBits() >= 128 &&
            "Unsupported vector type for unpckh");

    // AVX defines UNPCK* to operate independently on 128-bit lanes.
    unsigned NumLanes;
    unsigned NumOf256BitLanes;
    unsigned NumElts = VT.getVectorNumElements();
    if (VT.is256BitVector()) {
        if (NumElts != 4 && NumElts != 8 &&
                (!HasInt256 || (NumElts != 16 && NumElts != 32)))
            return false;
        NumLanes = 2;
        NumOf256BitLanes = 1;
    } else if (VT.is512BitVector()) {
        assert(VT.getScalarType().getSizeInBits() >= 32 &&
                "Unsupported vector type for unpckh");
        NumLanes = 2;
        NumOf256BitLanes = 2;
    } else {
        NumLanes = 1;
        NumOf256BitLanes = 1;
    }

    unsigned NumEltsInStride = NumElts/NumOf256BitLanes;
    unsigned NumLaneElts = NumEltsInStride/NumLanes;

    for (unsigned l256 = 0; l256 < NumOf256BitLanes; l256 += 1) {
        for (unsigned l = 0; l != NumEltsInStride; l += NumLaneElts) {
            for (unsigned i = 0, j = l+NumLaneElts/2; i != NumLaneElts; i += 2, ++j) {
                int BitI  = Mask[l256*NumEltsInStride+l+i];
                int BitI1 = Mask[l256*NumEltsInStride+l+i+1];
                if (!isUndefOrEqual(BitI, j+l256*NumElts))
                    return false;
                if (V2IsSplat && !isUndefOrEqual(BitI1, NumElts))
                    return false;
                if (!isUndefOrEqual(BitI1, j+l256*NumElts+NumEltsInStride))
                    return false;
            }
        }
    }
    return true;
}

/// isUNPCKL_v_undef_Mask - Special case of isUNPCKLMask for canonical form
/// of vector_shuffle v, v, <0, 4, 1, 5>, i.e. vector_shuffle v, undef,
/// <0, 0, 1, 1>
static bool isUNPCKL_v_undef_Mask(ArrayRef<int> Mask, MVT VT, bool HasInt256) {
    unsigned NumElts = VT.getVectorNumElements();
    bool Is256BitVec = VT.is256BitVector();

    if (VT.is512BitVector())
        return false;
    assert((VT.is128BitVector() || VT.is256BitVector()) &&
            "Unsupported vector type for unpckh");

    if (Is256BitVec && NumElts != 4 && NumElts != 8 &&
            (!HasInt256 || (NumElts != 16 && NumElts != 32)))
        return false;

    // For 256-bit i64/f64, use MOVDDUPY instead, so reject the matching pattern
    // FIXME: Need a better way to get rid of this, there's no latency difference
    // between UNPCKLPD and MOVDDUP, the later should always be checked first and
    // the former later. We should also remove the "_undef" special mask.
    if (NumElts == 4 && Is256BitVec)
        return false;

    // Handle 128 and 256-bit vector lengths. AVX defines UNPCK* to operate
    // independently on 128-bit lanes.
    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts/NumLanes;

    for (unsigned l = 0; l != NumElts; l += NumLaneElts) {
        for (unsigned i = 0, j = l; i != NumLaneElts; i += 2, ++j) {
            int BitI  = Mask[l+i];
            int BitI1 = Mask[l+i+1];

            if (!isUndefOrEqual(BitI, j))
                return false;
            if (!isUndefOrEqual(BitI1, j))
                return false;
        }
    }

    return true;
}

/// isUNPCKH_v_undef_Mask - Special case of isUNPCKHMask for canonical form
/// of vector_shuffle v, v, <2, 6, 3, 7>, i.e. vector_shuffle v, undef,
/// <2, 2, 3, 3>
static bool isUNPCKH_v_undef_Mask(ArrayRef<int> Mask, MVT VT, bool HasInt256) {
    unsigned NumElts = VT.getVectorNumElements();

    if (VT.is512BitVector())
        return false;

    assert((VT.is128BitVector() || VT.is256BitVector()) &&
            "Unsupported vector type for unpckh");

    if (VT.is256BitVector() && NumElts != 4 && NumElts != 8 &&
            (!HasInt256 || (NumElts != 16 && NumElts != 32)))
        return false;

    // Handle 128 and 256-bit vector lengths. AVX defines UNPCK* to operate
    // independently on 128-bit lanes.
    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts/NumLanes;

    for (unsigned l = 0; l != NumElts; l += NumLaneElts) {
        for (unsigned i = 0, j = l+NumLaneElts/2; i != NumLaneElts; i += 2, ++j) {
            int BitI  = Mask[l+i];
            int BitI1 = Mask[l+i+1];
            if (!isUndefOrEqual(BitI, j))
                return false;
            if (!isUndefOrEqual(BitI1, j))
                return false;
        }
    }
    return true;
}

/// isMOVLMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVSS,
/// MOVSD, and MOVD, i.e. setting the lowest element.
static bool isMOVLMask(ArrayRef<int> Mask, EVT VT) {
    if (VT.getVectorElementType().getSizeInBits() < 32)
        return false;
    if (!VT.is128BitVector())
        return false;

    unsigned NumElts = VT.getVectorNumElements();

    if (!isUndefOrEqual(Mask[0], NumElts))
        return false;

    for (unsigned i = 1; i != NumElts; ++i)
        if (!isUndefOrEqual(Mask[i], i))
            return false;

    return true;
}

/// isVPERM2X128Mask - Match 256-bit shuffles where the elements are considered
/// as permutations between 128-bit chunks or halves. As an example: this
/// shuffle bellow:
///   vector_shuffle <4, 5, 6, 7, 12, 13, 14, 15>
/// The first half comes from the second half of V1 and the second half from the
/// the second half of V2.
static bool isVPERM2X128Mask(ArrayRef<int> Mask, MVT VT, bool HasFp256) {
    if (!HasFp256 || !VT.is256BitVector())
        return false;

    // The shuffle result is divided into half A and half B. In total the two
    // sources have 4 halves, namely: C, D, E, F. The final values of A and
    // B must come from C, D, E or F.
    unsigned HalfSize = VT.getVectorNumElements()/2;
    bool MatchA = false, MatchB = false;

    // Check if A comes from one of C, D, E, F.
    for (unsigned Half = 0; Half != 4; ++Half) {
        if (isSequentialOrUndefInRange(Mask, 0, HalfSize, Half*HalfSize)) {
            MatchA = true;
            break;
        }
    }

    // Check if B comes from one of C, D, E, F.
    for (unsigned Half = 0; Half != 4; ++Half) {
        if (isSequentialOrUndefInRange(Mask, HalfSize, HalfSize, Half*HalfSize)) {
            MatchB = true;
            break;
        }
    }

    return MatchA && MatchB;
}

/// getShuffleVPERM2X128Immediate - Return the appropriate immediate to shuffle
/// the specified VECTOR_MASK mask with VPERM2F128/VPERM2I128 instructions.
static unsigned getShuffleVPERM2X128Immediate(ShuffleVectorSDNode *SVOp) {
    MVT VT = SVOp->getSimpleValueType(0);

    unsigned HalfSize = VT.getVectorNumElements()/2;

    unsigned FstHalf = 0, SndHalf = 0;
    for (unsigned i = 0; i < HalfSize; ++i) {
        if (SVOp->getMaskElt(i) > 0) {
            FstHalf = SVOp->getMaskElt(i)/HalfSize;
            break;
        }
    }
    for (unsigned i = HalfSize; i < HalfSize*2; ++i) {
        if (SVOp->getMaskElt(i) > 0) {
            SndHalf = SVOp->getMaskElt(i)/HalfSize;
            break;
        }
    }

    return (FstHalf | (SndHalf << 4));
}

// Symetric in-lane mask. Each lane has 4 elements (for imm8)
static bool isPermImmMask(ArrayRef<int> Mask, MVT VT, unsigned& Imm8) {
    unsigned EltSize = VT.getVectorElementType().getSizeInBits();
    if (EltSize < 32)
        return false;

    unsigned NumElts = VT.getVectorNumElements();
    Imm8 = 0;
    if (VT.is128BitVector() || (VT.is256BitVector() && EltSize == 64)) {
        for (unsigned i = 0; i != NumElts; ++i) {
            if (Mask[i] < 0)
                continue;
            Imm8 |= Mask[i] << (i*2);
        }
        return true;
    }

    unsigned LaneSize = 4;
    SmallVector<int, 4> MaskVal(LaneSize, -1);

    for (unsigned l = 0; l != NumElts; l += LaneSize) {
        for (unsigned i = 0; i != LaneSize; ++i) {
            if (!isUndefOrInRange(Mask[i+l], l, l+LaneSize))
                return false;
            if (Mask[i+l] < 0)
                continue;
            if (MaskVal[i] < 0) {
                MaskVal[i] = Mask[i+l] - l;
                Imm8 |= MaskVal[i] << (i*2);
                continue;
            }
            if (Mask[i+l] != (signed)(MaskVal[i]+l))
                return false;
        }
    }
    return true;
}

/// isVPERMILPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to VPERMILPD*.
/// Note that VPERMIL mask matching is different depending whether theunderlying
/// type is 32 or 64. In the VPERMILPS the high half of the mask should point
/// to the same elements of the low, but to the higher half of the source.
/// In VPERMILPD the two lanes could be shuffled independently of each other
/// with the same restriction that lanes can't be crossed. Also handles PSHUFDY.
static bool isVPERMILPMask(ArrayRef<int> Mask, MVT VT) {
    unsigned EltSize = VT.getVectorElementType().getSizeInBits();
    if (VT.getSizeInBits() < 256 || EltSize < 32)
        return false;
    bool symetricMaskRequired = (EltSize == 32);
    unsigned NumElts = VT.getVectorNumElements();

    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned LaneSize = NumElts/NumLanes;
    // 2 or 4 elements in one lane

    SmallVector<int, 4> ExpectedMaskVal(LaneSize, -1);
    for (unsigned l = 0; l != NumElts; l += LaneSize) {
        for (unsigned i = 0; i != LaneSize; ++i) {
            if (!isUndefOrInRange(Mask[i+l], l, l+LaneSize))
                return false;
            if (symetricMaskRequired) {
                if (ExpectedMaskVal[i] < 0 && Mask[i+l] >= 0) {
                    ExpectedMaskVal[i] = Mask[i+l] - l;
                    continue;
                }
                if (!isUndefOrEqual(Mask[i+l], ExpectedMaskVal[i]+l))
                    return false;
            }
        }
    }
    return true;
}

/// isCommutedMOVLMask - Returns true if the shuffle mask is except the reverse
/// of what cse523 movss want. Cse523 movs requires the lowest  element to be lowest
/// element of vector 2 and the other elements to come from vector 1 in order.
static bool isCommutedMOVLMask(ArrayRef<int> Mask, MVT VT,
        bool V2IsSplat = false, bool V2IsUndef = false) {
    if (!VT.is128BitVector())
        return false;

    unsigned NumOps = VT.getVectorNumElements();
    if (NumOps != 2 && NumOps != 4 && NumOps != 8 && NumOps != 16)
        return false;

    if (!isUndefOrEqual(Mask[0], 0))
        return false;

    for (unsigned i = 1; i != NumOps; ++i)
        if (!(isUndefOrEqual(Mask[i], i+NumOps) ||
                    (V2IsUndef && isUndefOrInRange(Mask[i], NumOps, NumOps*2)) ||
                    (V2IsSplat && isUndefOrEqual(Mask[i], NumOps))))
            return false;

    return true;
}

/// isMOVSHDUPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVSHDUP.
/// Masks to match: <1, 1, 3, 3> or <1, 1, 3, 3, 5, 5, 7, 7>
static bool isMOVSHDUPMask(ArrayRef<int> Mask, MVT VT,
        const Cse523Subtarget *Subtarget) {
    if (!Subtarget->hasSSE3())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if ((VT.is128BitVector() && NumElems != 4) ||
            (VT.is256BitVector() && NumElems != 8) ||
            (VT.is512BitVector() && NumElems != 16))
        return false;

    // "i+1" is the value the indexed mask element must have
    for (unsigned i = 0; i != NumElems; i += 2)
        if (!isUndefOrEqual(Mask[i], i+1) ||
                !isUndefOrEqual(Mask[i+1], i+1))
            return false;

    return true;
}

/// isMOVSLDUPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to MOVSLDUP.
/// Masks to match: <0, 0, 2, 2> or <0, 0, 2, 2, 4, 4, 6, 6>
static bool isMOVSLDUPMask(ArrayRef<int> Mask, MVT VT,
        const Cse523Subtarget *Subtarget) {
    if (!Subtarget->hasSSE3())
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if ((VT.is128BitVector() && NumElems != 4) ||
            (VT.is256BitVector() && NumElems != 8) ||
            (VT.is512BitVector() && NumElems != 16))
        return false;

    // "i" is the value the indexed mask element must have
    for (unsigned i = 0; i != NumElems; i += 2)
        if (!isUndefOrEqual(Mask[i], i) ||
                !isUndefOrEqual(Mask[i+1], i))
            return false;

    return true;
}

/// isMOVDDUPYMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to 256-bit
/// version of MOVDDUP.
static bool isMOVDDUPYMask(ArrayRef<int> Mask, MVT VT, bool HasFp256) {
    if (!HasFp256 || !VT.is256BitVector())
        return false;

    unsigned NumElts = VT.getVectorNumElements();
    if (NumElts != 4)
        return false;

    for (unsigned i = 0; i != NumElts/2; ++i)
        if (!isUndefOrEqual(Mask[i], 0))
            return false;
    for (unsigned i = NumElts/2; i != NumElts; ++i)
        if (!isUndefOrEqual(Mask[i], NumElts/2))
            return false;
    return true;
}

/// isMOVDDUPMask - Return true if the specified VECTOR_SHUFFLE operand
/// specifies a shuffle of elements that is suitable for input to 128-bit
/// version of MOVDDUP.
static bool isMOVDDUPMask(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    unsigned e = VT.getVectorNumElements() / 2;
    for (unsigned i = 0; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i))
            return false;
    for (unsigned i = 0; i != e; ++i)
        if (!isUndefOrEqual(Mask[e+i], i))
            return false;
    return true;
}

/// isVEXTRACTIndex - Return true if the specified
/// EXTRACT_SUBVECTOR operand specifies a vector extract that is
/// suitable for instruction that extract 128 or 256 bit vectors
static bool isVEXTRACTIndex(SDNode *N, unsigned vecWidth) {
    assert((vecWidth == 128 || vecWidth == 256) && "Unexpected vector width");
    if (!isa<ConstantSDNode>(N->getOperand(1).getNode()))
        return false;

    // The index should be aligned on a vecWidth-bit boundary.
    uint64_t Index =
        cast<ConstantSDNode>(N->getOperand(1).getNode())->getZExtValue();

    MVT VT = N->getSimpleValueType(0);
    unsigned ElSize = VT.getVectorElementType().getSizeInBits();
    bool Result = (Index * ElSize) % vecWidth == 0;

    return Result;
}

/// isVINSERTIndex - Return true if the specified INSERT_SUBVECTOR
/// operand specifies a subvector insert that is suitable for input to
/// insertion of 128 or 256-bit subvectors
static bool isVINSERTIndex(SDNode *N, unsigned vecWidth) {
    assert((vecWidth == 128 || vecWidth == 256) && "Unexpected vector width");
    if (!isa<ConstantSDNode>(N->getOperand(2).getNode()))
        return false;
    // The index should be aligned on a vecWidth-bit boundary.
    uint64_t Index =
        cast<ConstantSDNode>(N->getOperand(2).getNode())->getZExtValue();

    MVT VT = N->getSimpleValueType(0);
    unsigned ElSize = VT.getVectorElementType().getSizeInBits();
    bool Result = (Index * ElSize) % vecWidth == 0;

    return Result;
}

bool Cse523::isVINSERT128Index(SDNode *N) {
    return isVINSERTIndex(N, 128);
}

bool Cse523::isVINSERT256Index(SDNode *N) {
    return isVINSERTIndex(N, 256);
}

bool Cse523::isVEXTRACT128Index(SDNode *N) {
    return isVEXTRACTIndex(N, 128);
}

bool Cse523::isVEXTRACT256Index(SDNode *N) {
    return isVEXTRACTIndex(N, 256);
}

/// getShuffleSHUFImmediate - Return the appropriate immediate to shuffle
/// the specified VECTOR_SHUFFLE mask with PSHUF* and SHUFP* instructions.
/// Handles 128-bit and 256-bit.
static unsigned getShuffleSHUFImmediate(ShuffleVectorSDNode *N) {
    MVT VT = N->getSimpleValueType(0);

    assert((VT.getSizeInBits() >= 128) &&
            "Unsupported vector type for PSHUF/SHUFP");

    // Handle 128 and 256-bit vector lengths. AVX defines PSHUF/SHUFP to operate
    // independently on 128-bit lanes.
    unsigned NumElts = VT.getVectorNumElements();
    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts/NumLanes;

    assert((NumLaneElts == 2 || NumLaneElts == 4 || NumLaneElts == 8) &&
            "Only supports 2, 4 or 8 elements per lane");

    unsigned Shift = (NumLaneElts >= 4) ? 1 : 0;
    unsigned Mask = 0;
    for (unsigned i = 0; i != NumElts; ++i) {
        int Elt = N->getMaskElt(i);
        if (Elt < 0) continue;
        Elt &= NumLaneElts - 1;
        unsigned ShAmt = (i << Shift) % 8;
        Mask |= Elt << ShAmt;
    }

    return Mask;
}

/// getShufflePSHUFHWImmediate - Return the appropriate immediate to shuffle
/// the specified VECTOR_SHUFFLE mask with the PSHUFHW instruction.
static unsigned getShufflePSHUFHWImmediate(ShuffleVectorSDNode *N) {
    MVT VT = N->getSimpleValueType(0);

    assert((VT == MVT::v8i16 || VT == MVT::v16i16) &&
            "Unsupported vector type for PSHUFHW");

    unsigned NumElts = VT.getVectorNumElements();

    unsigned Mask = 0;
    for (unsigned l = 0; l != NumElts; l += 8) {
        // 8 nodes per lane, but we only care about the last 4.
        for (unsigned i = 0; i < 4; ++i) {
            int Elt = N->getMaskElt(l+i+4);
            if (Elt < 0) continue;
            Elt &= 0x3; // only 2-bits.
            Mask |= Elt << (i * 2);
        }
    }

    return Mask;
}

/// getShufflePSHUFLWImmediate - Return the appropriate immediate to shuffle
/// the specified VECTOR_SHUFFLE mask with the PSHUFLW instruction.
static unsigned getShufflePSHUFLWImmediate(ShuffleVectorSDNode *N) {
    MVT VT = N->getSimpleValueType(0);

    assert((VT == MVT::v8i16 || VT == MVT::v16i16) &&
            "Unsupported vector type for PSHUFHW");

    unsigned NumElts = VT.getVectorNumElements();

    unsigned Mask = 0;
    for (unsigned l = 0; l != NumElts; l += 8) {
        // 8 nodes per lane, but we only care about the first 4.
        for (unsigned i = 0; i < 4; ++i) {
            int Elt = N->getMaskElt(l+i);
            if (Elt < 0) continue;
            Elt &= 0x3; // only 2-bits
            Mask |= Elt << (i * 2);
        }
    }

    return Mask;
}

/// getShufflePALIGNRImmediate - Return the appropriate immediate to shuffle
/// the specified VECTOR_SHUFFLE mask with the PALIGNR instruction.
static unsigned getShufflePALIGNRImmediate(ShuffleVectorSDNode *SVOp) {
    MVT VT = SVOp->getSimpleValueType(0);
    unsigned EltSize = VT.is512BitVector() ? 1 :
        VT.getVectorElementType().getSizeInBits() >> 3;

    unsigned NumElts = VT.getVectorNumElements();
    unsigned NumLanes = VT.is512BitVector() ? 1 : VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts/NumLanes;

    int Val = 0;
    unsigned i;
    for (i = 0; i != NumElts; ++i) {
        Val = SVOp->getMaskElt(i);
        if (Val >= 0)
            break;
    }
    if (Val >= (int)NumElts)
        Val -= NumElts - NumLaneElts;

    assert(Val - i > 0 && "PALIGNR imm should be positive");
    return (Val - i) * EltSize;
}

static unsigned getExtractVEXTRACTImmediate(SDNode *N, unsigned vecWidth) {
    assert((vecWidth == 128 || vecWidth == 256) && "Unsupported vector width");
    if (!isa<ConstantSDNode>(N->getOperand(1).getNode()))
        llvm_unreachable("Illegal extract subvector for VEXTRACT");

    uint64_t Index =
        cast<ConstantSDNode>(N->getOperand(1).getNode())->getZExtValue();

    MVT VecVT = N->getOperand(0).getSimpleValueType();
    MVT ElVT = VecVT.getVectorElementType();

    unsigned NumElemsPerChunk = vecWidth / ElVT.getSizeInBits();
    return Index / NumElemsPerChunk;
}

static unsigned getInsertVINSERTImmediate(SDNode *N, unsigned vecWidth) {
    assert((vecWidth == 128 || vecWidth == 256) && "Unsupported vector width");
    if (!isa<ConstantSDNode>(N->getOperand(2).getNode()))
        llvm_unreachable("Illegal insert subvector for VINSERT");

    uint64_t Index =
        cast<ConstantSDNode>(N->getOperand(2).getNode())->getZExtValue();

    MVT VecVT = N->getSimpleValueType(0);
    MVT ElVT = VecVT.getVectorElementType();

    unsigned NumElemsPerChunk = vecWidth / ElVT.getSizeInBits();
    return Index / NumElemsPerChunk;
}

/// getExtractVEXTRACT128Immediate - Return the appropriate immediate
/// to extract the specified EXTRACT_SUBVECTOR index with VEXTRACTF128
/// and VINSERTI128 instructions.
unsigned Cse523::getExtractVEXTRACT128Immediate(SDNode *N) {
    return getExtractVEXTRACTImmediate(N, 128);
}

/// getExtractVEXTRACT256Immediate - Return the appropriate immediate
/// to extract the specified EXTRACT_SUBVECTOR index with VEXTRACTF64x4
/// and VINSERTI64x4 instructions.
unsigned Cse523::getExtractVEXTRACT256Immediate(SDNode *N) {
    return getExtractVEXTRACTImmediate(N, 256);
}

/// getInsertVINSERT128Immediate - Return the appropriate immediate
/// to insert at the specified INSERT_SUBVECTOR index with VINSERTF128
/// and VINSERTI128 instructions.
unsigned Cse523::getInsertVINSERT128Immediate(SDNode *N) {
    return getInsertVINSERTImmediate(N, 128);
}

/// getInsertVINSERT256Immediate - Return the appropriate immediate
/// to insert at the specified INSERT_SUBVECTOR index with VINSERTF46x4
/// and VINSERTI64x4 instructions.
unsigned Cse523::getInsertVINSERT256Immediate(SDNode *N) {
    return getInsertVINSERTImmediate(N, 256);
}

/// isZeroNode - Returns true if Elt is a constant zero or a floating point
/// constant +0.0.
bool Cse523::isZeroNode(SDValue Elt) {
    if (ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Elt))
        return CN->isNullValue();
    if (ConstantFPSDNode *CFP = dyn_cast<ConstantFPSDNode>(Elt))
        return CFP->getValueAPF().isPosZero();
    return false;
}

/// CommuteVectorShuffle - Swap vector_shuffle operands as well as values in
/// their permute mask.
static SDValue CommuteVectorShuffle(ShuffleVectorSDNode *SVOp,
        SelectionDAG &DAG) {
    MVT VT = SVOp->getSimpleValueType(0);
    unsigned NumElems = VT.getVectorNumElements();
    SmallVector<int, 8> MaskVec;

    for (unsigned i = 0; i != NumElems; ++i) {
        int Idx = SVOp->getMaskElt(i);
        if (Idx >= 0) {
            if (Idx < (int)NumElems)
                Idx += NumElems;
            else
                Idx -= NumElems;
        }
        MaskVec.push_back(Idx);
    }
    return DAG.getVectorShuffle(VT, SDLoc(SVOp), SVOp->getOperand(1),
            SVOp->getOperand(0), &MaskVec[0]);
}

/// ShouldXformToMOVHLPS - Return true if the node should be transformed to
/// match movhlps. The lower half elements should come from upper half of
/// V1 (and in order), and the upper half elements should come from the upper
/// half of V2 (and in order).
static bool ShouldXformToMOVHLPS(ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;
    if (VT.getVectorNumElements() != 4)
        return false;
    for (unsigned i = 0, e = 2; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i+2))
            return false;
    for (unsigned i = 2; i != 4; ++i)
        if (!isUndefOrEqual(Mask[i], i+4))
            return false;
    return true;
}

/// isScalarLoadToVector - Returns true if the node is a scalar load that
/// is promoted to a vector. It also returns the LoadSDNode by reference if
/// required.
static bool isScalarLoadToVector(SDNode *N, LoadSDNode **LD = NULL) {
    if (N->getOpcode() != ISD::SCALAR_TO_VECTOR)
        return false;
    N = N->getOperand(0).getNode();
    if (!ISD::isNON_EXTLoad(N))
        return false;
    if (LD)
        *LD = cast<LoadSDNode>(N);
    return true;
}

// Test whether the given value is a vector value which will be legalized
// into a load.
static bool WillBeConstantPoolLoad(SDNode *N) {
    if (N->getOpcode() != ISD::BUILD_VECTOR)
        return false;

    // Check for any non-constant elements.
    for (unsigned i = 0, e = N->getNumOperands(); i != e; ++i)
        switch (N->getOperand(i).getNode()->getOpcode()) {
            case ISD::UNDEF:
            case ISD::ConstantFP:
            case ISD::Constant:
                break;
            default:
                return false;
        }

    // Vectors of all-zeros and all-ones are materialized with special
    // instructions rather than being loaded.
    return !ISD::isBuildVectorAllZeros(N) &&
        !ISD::isBuildVectorAllOnes(N);
}

/// ShouldXformToMOVLP{S|D} - Return true if the node should be transformed to
/// match movlp{s|d}. The lower half elements should come from lower half of
/// V1 (and in order), and the upper half elements should come from the upper
/// half of V2 (and in order). And since V1 will become the source of the
/// MOVLP, it must be either a vector load or a scalar load to vector.
static bool ShouldXformToMOVLP(SDNode *V1, SDNode *V2,
        ArrayRef<int> Mask, MVT VT) {
    if (!VT.is128BitVector())
        return false;

    if (!ISD::isNON_EXTLoad(V1) && !isScalarLoadToVector(V1))
        return false;
    // Is V2 is a vector load, don't do this transformation. We will try to use
    // load folding shufps op.
    if (ISD::isNON_EXTLoad(V2) || WillBeConstantPoolLoad(V2))
        return false;

    unsigned NumElems = VT.getVectorNumElements();

    if (NumElems != 2 && NumElems != 4)
        return false;
    for (unsigned i = 0, e = NumElems/2; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i))
            return false;
    for (unsigned i = NumElems/2, e = NumElems; i != e; ++i)
        if (!isUndefOrEqual(Mask[i], i+NumElems))
            return false;
    return true;
}

/// isSplatVector - Returns true if N is a BUILD_VECTOR node whose elements are
/// all the same.
static bool isSplatVector(SDNode *N) {
    if (N->getOpcode() != ISD::BUILD_VECTOR)
        return false;

    SDValue SplatValue = N->getOperand(0);
    for (unsigned i = 1, e = N->getNumOperands(); i != e; ++i)
        if (N->getOperand(i) != SplatValue)
            return false;
    return true;
}

/// isZeroShuffle - Returns true if N is a VECTOR_SHUFFLE that can be resolved
/// to an zero vector.
/// FIXME: move to dag combiner / method on ShuffleVectorSDNode
static bool isZeroShuffle(ShuffleVectorSDNode *N) {
    SDValue V1 = N->getOperand(0);
    SDValue V2 = N->getOperand(1);
    unsigned NumElems = N->getValueType(0).getVectorNumElements();
    for (unsigned i = 0; i != NumElems; ++i) {
        int Idx = N->getMaskElt(i);
        if (Idx >= (int)NumElems) {
            unsigned Opc = V2.getOpcode();
            if (Opc == ISD::UNDEF || ISD::isBuildVectorAllZeros(V2.getNode()))
                continue;
            if (Opc != ISD::BUILD_VECTOR ||
                    !Cse523::isZeroNode(V2.getOperand(Idx-NumElems)))
                return false;
        } else if (Idx >= 0) {
            unsigned Opc = V1.getOpcode();
            if (Opc == ISD::UNDEF || ISD::isBuildVectorAllZeros(V1.getNode()))
                continue;
            if (Opc != ISD::BUILD_VECTOR ||
                    !Cse523::isZeroNode(V1.getOperand(Idx)))
                return false;
        }
    }
    return true;
}

/// getZeroVector - Returns a vector of specified type with all zero elements.
///
static SDValue getZeroVector(EVT VT, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG, SDLoc dl) {
    llvm_unreachable("Unexpected vector type");
    return SDValue();
}

/// getOnesVector - Returns a vector of specified type with all bits set.
/// Always build ones vectors as <4 x i32> or <8 x i32>. For 256-bit types with
/// no AVX2 supprt, use two <4 x i32> inserted in a <8 x i32> appropriately.
/// Then bitcast to their original type, ensuring they get CSE'd.
static SDValue getOnesVector(MVT VT, bool HasInt256, SelectionDAG &DAG,
        SDLoc dl) {
    llvm_unreachable("Unexpected vector type");
    return SDValue();
}

/// getMOVLMask - Returns a vector_shuffle mask for an movs{s|d}, movd
/// operation of specified width.
static SDValue getMOVL(SelectionDAG &DAG, SDLoc dl, EVT VT, SDValue V1,
        SDValue V2) {
    unsigned NumElems = VT.getVectorNumElements();
    SmallVector<int, 8> Mask;
    Mask.push_back(NumElems);
    for (unsigned i = 1; i != NumElems; ++i)
        Mask.push_back(i);
    return DAG.getVectorShuffle(VT, dl, V1, V2, &Mask[0]);
}

/// getUnpackl - Returns a vector_shuffle node for an unpackl operation.
static SDValue getUnpackl(SelectionDAG &DAG, SDLoc dl, MVT VT, SDValue V1,
        SDValue V2) {
    unsigned NumElems = VT.getVectorNumElements();
    SmallVector<int, 8> Mask;
    for (unsigned i = 0, e = NumElems/2; i != e; ++i) {
        Mask.push_back(i);
        Mask.push_back(i + NumElems);
    }
    return DAG.getVectorShuffle(VT, dl, V1, V2, &Mask[0]);
}

/// getUnpackh - Returns a vector_shuffle node for an unpackh operation.
static SDValue getUnpackh(SelectionDAG &DAG, SDLoc dl, MVT VT, SDValue V1,
        SDValue V2) {
    unsigned NumElems = VT.getVectorNumElements();
    SmallVector<int, 8> Mask;
    for (unsigned i = 0, Half = NumElems/2; i != Half; ++i) {
        Mask.push_back(i + Half);
        Mask.push_back(i + NumElems + Half);
    }
    return DAG.getVectorShuffle(VT, dl, V1, V2, &Mask[0]);
}

// PromoteSplati8i16 - All i16 and i8 vector types can't be used directly by
// a generic shuffle instruction because the target has no such instructions.
// Generate shuffles which repeat i16 and i8 several times until they can be
// represented by v4f32 and then be manipulated by target suported shuffles.
static SDValue PromoteSplati8i16(SDValue V, SelectionDAG &DAG, int &EltNo) {
    MVT VT = V.getSimpleValueType();
    int NumElems = VT.getVectorNumElements();
    SDLoc dl(V);

    while (NumElems > 4) {
        if (EltNo < NumElems/2) {
            V = getUnpackl(DAG, dl, VT, V, V);
        } else {
            V = getUnpackh(DAG, dl, VT, V, V);
            EltNo -= NumElems/2;
        }
        NumElems >>= 1;
    }
    return V;
}

/// getLegalSplat - Generate a legal splat with supported cse523 shuffles
static SDValue getLegalSplat(SelectionDAG &DAG, SDValue V, int EltNo) {
    MVT VT = V.getSimpleValueType();
    SDLoc dl(V);

    if (VT.is128BitVector()) {
        V = DAG.getNode(ISD::BITCAST, dl, MVT::v4f32, V);
        int SplatMask[4] = { EltNo, EltNo, EltNo, EltNo };
        V = DAG.getVectorShuffle(MVT::v4f32, dl, V, DAG.getUNDEF(MVT::v4f32),
                &SplatMask[0]);
    } else if (VT.is256BitVector()) {
        // To use VPERMILPS to splat scalars, the second half of indicies must
        // refer to the higher part, which is a duplication of the lower one,
        // because VPERMILPS can only handle in-lane permutations.
        int SplatMask[8] = { EltNo, EltNo, EltNo, EltNo,
            EltNo+4, EltNo+4, EltNo+4, EltNo+4 };

        V = DAG.getNode(ISD::BITCAST, dl, MVT::v8f32, V);
        V = DAG.getVectorShuffle(MVT::v8f32, dl, V, DAG.getUNDEF(MVT::v8f32),
                &SplatMask[0]);
    } else
        llvm_unreachable("Vector size not supported");

    return DAG.getNode(ISD::BITCAST, dl, VT, V);
}

/// PromoteSplat - Splat is promoted to target supported vector shuffles.
static SDValue PromoteSplat(ShuffleVectorSDNode *SV, SelectionDAG &DAG) {
    MVT SrcVT = SV->getSimpleValueType(0);
    SDValue V1 = SV->getOperand(0);
    SDLoc dl(SV);

    int EltNo = SV->getSplatIndex();
    int NumElems = SrcVT.getVectorNumElements();
    bool Is256BitVec = SrcVT.is256BitVector();

    assert(((SrcVT.is128BitVector() && NumElems > 4) || Is256BitVec) &&
            "Unknown how to promote splat for type");

    // Extract the 128-bit part containing the splat element and update
    // the splat element index when it refers to the higher register.
    if (Is256BitVec) {
        V1 = Extract128BitVector(V1, EltNo, DAG, dl);
        if (EltNo >= NumElems/2)
            EltNo -= NumElems/2;
    }

    // All i16 and i8 vector types can't be used directly by a generic shuffle
    // instruction because the target has no such instruction. Generate shuffles
    // which repeat i16 and i8 several times until they fit in i32, and then can
    // be manipulated by target suported shuffles.
    MVT EltVT = SrcVT.getVectorElementType();
    if (EltVT == MVT::i8 || EltVT == MVT::i16)
        V1 = PromoteSplati8i16(V1, DAG, EltNo);

    // Recreate the 256-bit vector and place the same 128-bit vector
    // into the low and high part. This is necessary because we want
    // to use VPERM* to shuffle the vectors
    if (Is256BitVec) {
        V1 = DAG.getNode(ISD::CONCAT_VECTORS, dl, SrcVT, V1, V1);
    }

    return getLegalSplat(DAG, V1, EltNo);
}

/// getShuffleVectorZeroOrUndef - Return a vector_shuffle of the specified
/// vector of zero or undef vector.  This produces a shuffle where the low
/// element of V2 is swizzled into the zero/undef vector, landing at element
/// Idx.  This produces a shuffle mask like 4,1,2,3 (idx=0) or  0,1,2,4 (idx=3).
static SDValue getShuffleVectorZeroOrUndef(SDValue V2, unsigned Idx,
        bool IsZero,
        const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    MVT VT = V2.getSimpleValueType();
    SDValue V1 = IsZero
        ? getZeroVector(VT, Subtarget, DAG, SDLoc(V2)) : DAG.getUNDEF(VT);
    unsigned NumElems = VT.getVectorNumElements();
    SmallVector<int, 16> MaskVec;
    for (unsigned i = 0; i != NumElems; ++i)
        // If this is the insertion idx, put the low elt of V2 here.
        MaskVec.push_back(i == Idx ? NumElems : i);
    return DAG.getVectorShuffle(VT, SDLoc(V2), V1, V2, &MaskVec[0]);
}

/// getTargetShuffleMask - Calculates the shuffle mask corresponding to the
/// target specific opcode. Returns true if the Mask could be calculated.
/// Sets IsUnary to true if only uses one source.
static bool getTargetShuffleMask(SDNode *N, MVT VT,
        SmallVectorImpl<int> &Mask, bool &IsUnary) {
    unsigned NumElems = VT.getVectorNumElements();
    SDValue ImmN;

    IsUnary = false;
    switch(N->getOpcode()) {
        case Cse523ISD::SHUFP:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodeSHUFPMask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            break;
        case Cse523ISD::UNPCKH:
            DecodeUNPCKHMask(VT, Mask);
            break;
        case Cse523ISD::UNPCKL:
            DecodeUNPCKLMask(VT, Mask);
            break;
        case Cse523ISD::MOVHLPS:
            DecodeMOVHLPSMask(NumElems, Mask);
            break;
        case Cse523ISD::MOVLHPS:
            DecodeMOVLHPSMask(NumElems, Mask);
            break;
        case Cse523ISD::PALIGNR:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodePALIGNRMask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            break;
        case Cse523ISD::PSHUFD:
        case Cse523ISD::VPERMILP:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodePSHUFMask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            IsUnary = true;
            break;
        case Cse523ISD::PSHUFHW:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodePSHUFHWMask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            IsUnary = true;
            break;
        case Cse523ISD::PSHUFLW:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodePSHUFLWMask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            IsUnary = true;
            break;
        case Cse523ISD::VPERMI:
            ImmN = N->getOperand(N->getNumOperands()-1);
            DecodeVPERMMask(cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
            IsUnary = true;
            break;
        case Cse523ISD::MOVSS:
        case Cse523ISD::MOVSD: {
                                   // The index 0 always comes from the first element of the second source,
                                   // this is why MOVSS and MOVSD are used in the first place. The other
                                   // elements come from the other positions of the first source vector
                                   Mask.push_back(NumElems);
                                   for (unsigned i = 1; i != NumElems; ++i) {
                                       Mask.push_back(i);
                                   }
                                   break;
                               }
        case Cse523ISD::VPERM2X128:
                               ImmN = N->getOperand(N->getNumOperands()-1);
                               DecodeVPERM2X128Mask(VT, cast<ConstantSDNode>(ImmN)->getZExtValue(), Mask);
                               if (Mask.empty()) return false;
                               break;
        case Cse523ISD::MOVDDUP:
        case Cse523ISD::MOVLHPD:
        case Cse523ISD::MOVLPD:
        case Cse523ISD::MOVLPS:
        case Cse523ISD::MOVSHDUP:
        case Cse523ISD::MOVSLDUP:
                               // Not yet implemented
                               return false;
        default: llvm_unreachable("unknown target shuffle node");
    }

    return true;
}

/// getShuffleScalarElt - Returns the scalar element that will make up the ith
/// element of the result of the vector shuffle.
static SDValue getShuffleScalarElt(SDNode *N, unsigned Index, SelectionDAG &DAG,
        unsigned Depth) {
    if (Depth == 6)
        return SDValue();  // Limit search depth.

    SDValue V = SDValue(N, 0);
    EVT VT = V.getValueType();
    unsigned Opcode = V.getOpcode();

    // Recurse into ISD::VECTOR_SHUFFLE node to find scalars.
    if (const ShuffleVectorSDNode *SV = dyn_cast<ShuffleVectorSDNode>(N)) {
        int Elt = SV->getMaskElt(Index);

        if (Elt < 0)
            return DAG.getUNDEF(VT.getVectorElementType());

        unsigned NumElems = VT.getVectorNumElements();
        SDValue NewV = (Elt < (int)NumElems) ? SV->getOperand(0)
            : SV->getOperand(1);
        return getShuffleScalarElt(NewV.getNode(), Elt % NumElems, DAG, Depth+1);
    }

    // Recurse into target specific vector shuffles to find scalars.
    if (isTargetShuffle(Opcode)) {
        MVT ShufVT = V.getSimpleValueType();
        unsigned NumElems = ShufVT.getVectorNumElements();
        SmallVector<int, 16> ShuffleMask;
        bool IsUnary;

        if (!getTargetShuffleMask(N, ShufVT, ShuffleMask, IsUnary))
            return SDValue();

        int Elt = ShuffleMask[Index];
        if (Elt < 0)
            return DAG.getUNDEF(ShufVT.getVectorElementType());

        SDValue NewV = (Elt < (int)NumElems) ? N->getOperand(0)
            : N->getOperand(1);
        return getShuffleScalarElt(NewV.getNode(), Elt % NumElems, DAG,
                Depth+1);
    }

    // Actual nodes that may contain scalar elements
    if (Opcode == ISD::BITCAST) {
        V = V.getOperand(0);
        EVT SrcVT = V.getValueType();
        unsigned NumElems = VT.getVectorNumElements();

        if (!SrcVT.isVector() || SrcVT.getVectorNumElements() != NumElems)
            return SDValue();
    }

    if (V.getOpcode() == ISD::SCALAR_TO_VECTOR)
        return (Index == 0) ? V.getOperand(0)
            : DAG.getUNDEF(VT.getVectorElementType());

    if (V.getOpcode() == ISD::BUILD_VECTOR)
        return V.getOperand(Index);

    return SDValue();
}

/// EltsFromConsecutiveLoads - Given the initializing elements 'Elts' of a
/// vector of type 'VT', see if the elements can be replaced by a single large
/// load which has the same value as a build_vector whose operands are 'elts'.
///
/// Example: <load i32 *a, load i32 *a+4, undef, undef> -> zextload a
///
/// FIXME: we'd also like to handle the case where the last elements are zero
/// rather than undef via VZEXT_LOAD, but we do not detect that case today.
/// There's even a handy isZeroNode for that purpose.
static SDValue EltsFromConsecutiveLoads(EVT VT, SmallVectorImpl<SDValue> &Elts,
        SDLoc &DL, SelectionDAG &DAG,
        bool isAfterLegalize) {
    EVT EltVT = VT.getVectorElementType();
    unsigned NumElems = Elts.size();

    LoadSDNode *LDBase = NULL;
    unsigned LastLoadedElt = -1U;

    // For each element in the initializer, see if we've found a load or an undef.
    // If we don't find an initial load element, or later load elements are
    // non-consecutive, bail out.
    for (unsigned i = 0; i < NumElems; ++i) {
        SDValue Elt = Elts[i];

        if (!Elt.getNode() ||
                (Elt.getOpcode() != ISD::UNDEF && !ISD::isNON_EXTLoad(Elt.getNode())))
            return SDValue();
        if (!LDBase) {
            if (Elt.getNode()->getOpcode() == ISD::UNDEF)
                return SDValue();
            LDBase = cast<LoadSDNode>(Elt.getNode());
            LastLoadedElt = i;
            continue;
        }
        if (Elt.getOpcode() == ISD::UNDEF)
            continue;

        LoadSDNode *LD = cast<LoadSDNode>(Elt);
        if (!DAG.isConsecutiveLoad(LD, LDBase, EltVT.getSizeInBits()/8, i))
            return SDValue();
        LastLoadedElt = i;
    }

    // If we have found an entire vector of loads and undefs, then return a large
    // load of the entire vector width starting at the base pointer.  If we found
    // consecutive loads for the low half, generate a vzext_load node.
    if (LastLoadedElt == NumElems - 1) {

        if (isAfterLegalize &&
                !DAG.getTargetLoweringInfo().isOperationLegal(ISD::LOAD, VT))
            return SDValue();

        SDValue NewLd = SDValue();

        if (DAG.InferPtrAlignment(LDBase->getBasePtr()) >= 16)
            NewLd = DAG.getLoad(VT, DL, LDBase->getChain(), LDBase->getBasePtr(),
                    LDBase->getPointerInfo(),
                    LDBase->isVolatile(), LDBase->isNonTemporal(),
                    LDBase->isInvariant(), 0);
        NewLd = DAG.getLoad(VT, DL, LDBase->getChain(), LDBase->getBasePtr(),
                LDBase->getPointerInfo(),
                LDBase->isVolatile(), LDBase->isNonTemporal(),
                LDBase->isInvariant(), LDBase->getAlignment());

        if (LDBase->hasAnyUseOfValue(1)) {
            SDValue NewChain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                    SDValue(LDBase, 1),
                    SDValue(NewLd.getNode(), 1));
            DAG.ReplaceAllUsesOfValueWith(SDValue(LDBase, 1), NewChain);
            DAG.UpdateNodeOperands(NewChain.getNode(), SDValue(LDBase, 1),
                    SDValue(NewLd.getNode(), 1));
        }

        return NewLd;
    }
    if (NumElems == 4 && LastLoadedElt == 1 &&
            DAG.getTargetLoweringInfo().isTypeLegal(MVT::v2i64)) {
        SDVTList Tys = DAG.getVTList(MVT::v2i64, MVT::Other);
        SDValue Ops[] = { LDBase->getChain(), LDBase->getBasePtr() };
        SDValue ResNode =
            DAG.getMemIntrinsicNode(Cse523ISD::VZEXT_LOAD, DL, Tys, Ops,
                    array_lengthof(Ops), MVT::i64,
                    LDBase->getPointerInfo(),
                    LDBase->getAlignment(),
                    false/*isVolatile*/, true/*ReadMem*/,
                    false/*WriteMem*/);

        // Make sure the newly-created LOAD is in the same position as LDBase in
        // terms of dependency. We create a TokenFactor for LDBase and ResNode, and
        // update uses of LDBase's output chain to use the TokenFactor.
        if (LDBase->hasAnyUseOfValue(1)) {
            SDValue NewChain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                    SDValue(LDBase, 1), SDValue(ResNode.getNode(), 1));
            DAG.ReplaceAllUsesOfValueWith(SDValue(LDBase, 1), NewChain);
            DAG.UpdateNodeOperands(NewChain.getNode(), SDValue(LDBase, 1),
                    SDValue(ResNode.getNode(), 1));
        }

        return DAG.getNode(ISD::BITCAST, DL, VT, ResNode);
    }
    return SDValue();
}

// Lower BUILD_VECTOR operation for v8i1 and v16i1 types.
SDValue
Cse523TargetLowering::LowerBUILD_VECTORvXi1(SDValue Op, SelectionDAG &DAG) const {
    llvm_unreachable("Unsupported BUILD_VECTOR operation");
    return SDValue();
}

SDValue
Cse523TargetLowering::LowerBUILD_VECTOR(SDValue Op, SelectionDAG &DAG) const {
    llvm_unreachable("Unsupported BUILD_VECTOR operation");
    return SDValue();
}

SDValue
Cse523TargetLowering::LowerVECTOR_SHUFFLE(SDValue Op, SelectionDAG &DAG) const {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

SDValue
Cse523TargetLowering::LowerEXTRACT_VECTOR_ELT(SDValue Op,
        SelectionDAG &DAG) const {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

SDValue
Cse523TargetLowering::LowerINSERT_VECTOR_ELT(SDValue Op, SelectionDAG &DAG) const {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

static SDValue LowerSCALAR_TO_VECTOR(SDValue Op, SelectionDAG &DAG) {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

// Lower a node with an EXTRACT_SUBVECTOR opcode.  This may result in
// a simple subregister reference or explicit instructions to grab
// upper bits of a vector.
static SDValue LowerEXTRACT_SUBVECTOR(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

// Lower a node with an INSERT_SUBVECTOR opcode.  This may result in a
// simple superregister reference or explicit instructions to insert
// the upper bits of a vector.
static SDValue LowerINSERT_SUBVECTOR(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

// ConstantPool, JumpTable, GlobalAddress, and ExternalSymbol are lowered as
// their target countpart wrapped in the Cse523ISD::Wrapper node. Suppose N is
// one of the above mentioned nodes. It has to be wrapped because otherwise
// Select(N) returns N. So the raw TargetGlobalAddress nodes, etc. can only
// be used to form addressing mode. These wrapped nodes will be selected
// into MOV32ri.
SDValue
Cse523TargetLowering::LowerConstantPool(SDValue Op, SelectionDAG &DAG) const {
    ConstantPoolSDNode *CP = cast<ConstantPoolSDNode>(Op);

    // In PIC mode (unless we're in RIPRel PIC mode) we add an offset to the
    // global base reg.
    unsigned char OpFlag = 0;
    unsigned WrapperKind = Cse523ISD::Wrapper;
    CodeModel::Model M = getTargetMachine().getCodeModel();

    if (Subtarget->isPICStyleRIPRel() &&
            (M == CodeModel::Small || M == CodeModel::Kernel))
        WrapperKind = Cse523ISD::WrapperRIP;
    else if (Subtarget->isPICStyleGOT())
        OpFlag = Cse523II::MO_GOTOFF;
    else if (Subtarget->isPICStyleStubPIC())
        OpFlag = Cse523II::MO_PIC_BASE_OFFSET;

    SDValue Result = DAG.getTargetConstantPool(CP->getConstVal(), getPointerTy(),
            CP->getAlignment(),
            CP->getOffset(), OpFlag);
    SDLoc DL(CP);
    Result = DAG.getNode(WrapperKind, DL, getPointerTy(), Result);
    // With PIC, the address is actually $g + Offset.
    if (OpFlag) {
        Result = DAG.getNode(ISD::ADD, DL, getPointerTy(),
                DAG.getNode(Cse523ISD::GlobalBaseReg,
                    SDLoc(), getPointerTy()),
                Result);
    }

    return Result;
}

SDValue Cse523TargetLowering::LowerJumpTable(SDValue Op, SelectionDAG &DAG) const {
    JumpTableSDNode *JT = cast<JumpTableSDNode>(Op);

    // In PIC mode (unless we're in RIPRel PIC mode) we add an offset to the
    // global base reg.
    unsigned char OpFlag = 0;
    unsigned WrapperKind = Cse523ISD::Wrapper;
    CodeModel::Model M = getTargetMachine().getCodeModel();

    if (Subtarget->isPICStyleRIPRel() &&
            (M == CodeModel::Small || M == CodeModel::Kernel))
        WrapperKind = Cse523ISD::WrapperRIP;
    else if (Subtarget->isPICStyleGOT())
        OpFlag = Cse523II::MO_GOTOFF;
    else if (Subtarget->isPICStyleStubPIC())
        OpFlag = Cse523II::MO_PIC_BASE_OFFSET;

    SDValue Result = DAG.getTargetJumpTable(JT->getIndex(), getPointerTy(),
            OpFlag);
    SDLoc DL(JT);
    Result = DAG.getNode(WrapperKind, DL, getPointerTy(), Result);

    // With PIC, the address is actually $g + Offset.
    if (OpFlag)
        Result = DAG.getNode(ISD::ADD, DL, getPointerTy(),
                DAG.getNode(Cse523ISD::GlobalBaseReg,
                    SDLoc(), getPointerTy()),
                Result);

    return Result;
}

SDValue
Cse523TargetLowering::LowerExternalSymbol(SDValue Op, SelectionDAG &DAG) const {
    const char *Sym = cast<ExternalSymbolSDNode>(Op)->getSymbol();

    // In PIC mode (unless we're in RIPRel PIC mode) we add an offset to the
    // global base reg.
    unsigned char OpFlag = 0;
    unsigned WrapperKind = Cse523ISD::Wrapper;
    CodeModel::Model M = getTargetMachine().getCodeModel();

    if (Subtarget->isPICStyleRIPRel() &&
            (M == CodeModel::Small || M == CodeModel::Kernel)) {
        if (Subtarget->isTargetDarwin() || Subtarget->isTargetELF())
            OpFlag = Cse523II::MO_GOTPCREL;
        WrapperKind = Cse523ISD::WrapperRIP;
    } else if (Subtarget->isPICStyleGOT()) {
        OpFlag = Cse523II::MO_GOT;
    } else if (Subtarget->isPICStyleStubPIC()) {
        OpFlag = Cse523II::MO_DARWIN_NONLAZY_PIC_BASE;
    } else if (Subtarget->isPICStyleStubNoDynamic()) {
        OpFlag = Cse523II::MO_DARWIN_NONLAZY;
    }

    SDValue Result = DAG.getTargetExternalSymbol(Sym, getPointerTy(), OpFlag);

    SDLoc DL(Op);
    Result = DAG.getNode(WrapperKind, DL, getPointerTy(), Result);

    // With PIC, the address is actually $g + Offset.
    if (getTargetMachine().getRelocationModel() == Reloc::PIC_ &&
            !Subtarget->is64Bit()) {
        Result = DAG.getNode(ISD::ADD, DL, getPointerTy(),
                DAG.getNode(Cse523ISD::GlobalBaseReg,
                    SDLoc(), getPointerTy()),
                Result);
    }

    // For symbols that require a load from a stub to get the address, emit the
    // load.
    if (isGlobalStubReference(OpFlag))
        Result = DAG.getLoad(getPointerTy(), DL, DAG.getEntryNode(), Result,
                MachinePointerInfo::getGOT(), false, false, false, 0);

    return Result;
}

SDValue
Cse523TargetLowering::LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const {
    // Create the TargetBlockAddressAddress node.
    unsigned char OpFlags =
        Subtarget->ClassifyBlockAddressReference();
    CodeModel::Model M = getTargetMachine().getCodeModel();
    const BlockAddress *BA = cast<BlockAddressSDNode>(Op)->getBlockAddress();
    int64_t Offset = cast<BlockAddressSDNode>(Op)->getOffset();
    SDLoc dl(Op);
    SDValue Result = DAG.getTargetBlockAddress(BA, getPointerTy(), Offset,
            OpFlags);

    if (Subtarget->isPICStyleRIPRel() &&
            (M == CodeModel::Small || M == CodeModel::Kernel))
        Result = DAG.getNode(Cse523ISD::WrapperRIP, dl, getPointerTy(), Result);
    else
        Result = DAG.getNode(Cse523ISD::Wrapper, dl, getPointerTy(), Result);

    // With PIC, the address is actually $g + Offset.
    if (isGlobalRelativeToPICBase(OpFlags)) {
        Result = DAG.getNode(ISD::ADD, dl, getPointerTy(),
                DAG.getNode(Cse523ISD::GlobalBaseReg, dl, getPointerTy()),
                Result);
    }

    return Result;
}

SDValue
Cse523TargetLowering::LowerGlobalAddress(const GlobalValue *GV, SDLoc dl,
        int64_t Offset, SelectionDAG &DAG) const {
    // Create the TargetGlobalAddress node, folding in the constant
    // offset if it is legal.
    unsigned char OpFlags =
        Subtarget->ClassifyGlobalReference(GV, getTargetMachine());
    CodeModel::Model M = getTargetMachine().getCodeModel();
    SDValue Result;
    if (OpFlags == Cse523II::MO_NO_FLAG &&
            Cse523::isOffsetSuitableForCodeModel(Offset, M)) {
        // A direct static reference to a global.
        Result = DAG.getTargetGlobalAddress(GV, dl, getPointerTy(), Offset);
        Offset = 0;
    } else {
        Result = DAG.getTargetGlobalAddress(GV, dl, getPointerTy(), 0, OpFlags);
    }

    if (Subtarget->isPICStyleRIPRel() &&
            (M == CodeModel::Small || M == CodeModel::Kernel))
        Result = DAG.getNode(Cse523ISD::WrapperRIP, dl, getPointerTy(), Result);
    else
        Result = DAG.getNode(Cse523ISD::Wrapper, dl, getPointerTy(), Result);

    // With PIC, the address is actually $g + Offset.
    if (isGlobalRelativeToPICBase(OpFlags)) {
        Result = DAG.getNode(ISD::ADD, dl, getPointerTy(),
                DAG.getNode(Cse523ISD::GlobalBaseReg, dl, getPointerTy()),
                Result);
    }

    // For globals that require a load from a stub to get the address, emit the
    // load.
    if (isGlobalStubReference(OpFlags))
        Result = DAG.getLoad(getPointerTy(), dl, DAG.getEntryNode(), Result,
                MachinePointerInfo::getGOT(), false, false, false, 0);

    // If there was a non-zero offset that we didn't fold, create an explicit
    // addition for it.
    if (Offset != 0)
        Result = DAG.getNode(ISD::ADD, dl, getPointerTy(), Result,
                DAG.getConstant(Offset, getPointerTy()));

    return Result;
}

SDValue
Cse523TargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const {
    const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
    int64_t Offset = cast<GlobalAddressSDNode>(Op)->getOffset();
    return LowerGlobalAddress(GV, SDLoc(Op), Offset, DAG);
}

static SDValue
GetTLSADDR(SelectionDAG &DAG, SDValue Chain, GlobalAddressSDNode *GA,
        SDValue *InFlag, const EVT PtrVT, unsigned ReturnReg,
        unsigned char OperandFlags, bool LocalDynamic = false) {
    MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
    SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
    SDLoc dl(GA);
    SDValue TGA = DAG.getTargetGlobalAddress(GA->getGlobal(), dl,
            GA->getValueType(0),
            GA->getOffset(),
            OperandFlags);

    Cse523ISD::NodeType CallType = LocalDynamic ? Cse523ISD::TLSBASEADDR
        : Cse523ISD::TLSADDR;

    if (InFlag) {
        SDValue Ops[] = { Chain,  TGA, *InFlag };
        Chain = DAG.getNode(CallType, dl, NodeTys, Ops, array_lengthof(Ops));
    } else {
        SDValue Ops[]  = { Chain, TGA };
        Chain = DAG.getNode(CallType, dl, NodeTys, Ops, array_lengthof(Ops));
    }

    // TLSADDR will be codegen'ed as call. Inform MFI that function has calls.
    MFI->setAdjustsStack(true);

    SDValue Flag = Chain.getValue(1);
    return DAG.getCopyFromReg(Chain, dl, ReturnReg, PtrVT, Flag);
}

// Lower ISD::GlobalTLSAddress using the "general dynamic" model, 64 bit
static SDValue
LowerToTLSGeneralDynamicModel64(GlobalAddressSDNode *GA, SelectionDAG &DAG,
        const EVT PtrVT) {
    return GetTLSADDR(DAG, DAG.getEntryNode(), GA, NULL, PtrVT,
            Cse523::RAX, Cse523II::MO_TLSGD);
}

static SDValue LowerToTLSLocalDynamicModel(GlobalAddressSDNode *GA,
        SelectionDAG &DAG,
        const EVT PtrVT,
        bool is64Bit) {
    SDLoc dl(GA);

    // Get the start address of the TLS block for this module.
    Cse523MachineFunctionInfo* MFI = DAG.getMachineFunction()
        .getInfo<Cse523MachineFunctionInfo>();
    MFI->incNumLocalDynamicTLSAccesses();

    SDValue Base;
    Base = GetTLSADDR(DAG, DAG.getEntryNode(), GA, NULL, PtrVT, Cse523::RAX,
            Cse523II::MO_TLSLD, /*LocalDynamic=*/true);

    // Note: the CleanupLocalDynamicTLSPass will remove redundant computations
    // of Base.

    // Build x@dtpoff.
    unsigned char OperandFlags = Cse523II::MO_DTPOFF;
    unsigned WrapperKind = Cse523ISD::Wrapper;
    SDValue TGA = DAG.getTargetGlobalAddress(GA->getGlobal(), dl,
            GA->getValueType(0),
            GA->getOffset(), OperandFlags);
    SDValue Offset = DAG.getNode(WrapperKind, dl, PtrVT, TGA);

    // Add x@dtpoff with the base.
    return DAG.getNode(ISD::ADD, dl, PtrVT, Offset, Base);
}

// Lower ISD::GlobalTLSAddress using the "initial exec" or "local exec" model.
static SDValue LowerToTLSExecModel(GlobalAddressSDNode *GA, SelectionDAG &DAG,
        const EVT PtrVT, TLSModel::Model model,
        bool is64Bit, bool isPIC) {
    SDLoc dl(GA);

    // Get the Thread Pointer, which is %gs:0 (32-bit) or %fs:0 (64-bit).
    Value *Ptr = Constant::getNullValue(Type::getInt8PtrTy(*DAG.getContext(),
                is64Bit ? 257 : 256));

    SDValue ThreadPointer =
        DAG.getLoad(PtrVT, dl, DAG.getEntryNode(), DAG.getIntPtrConstant(0),
                MachinePointerInfo(Ptr), false, false, false, 0);

    unsigned char OperandFlags = 0;
    // Most TLS accesses are not RIP relative, even on cse523-64.  One exception is
    // initialexec.
    unsigned WrapperKind = Cse523ISD::Wrapper;
    if (model == TLSModel::LocalExec) {
        OperandFlags = is64Bit ? Cse523II::MO_TPOFF : Cse523II::MO_NTPOFF;
    } else if (model == TLSModel::InitialExec) {
        if (is64Bit) {
            OperandFlags = Cse523II::MO_GOTTPOFF;
            WrapperKind = Cse523ISD::WrapperRIP;
        } else {
            OperandFlags = isPIC ? Cse523II::MO_GOTNTPOFF : Cse523II::MO_INDNTPOFF;
        }
    } else {
        llvm_unreachable("Unexpected model");
    }

    // emit "addl x@ntpoff,%eax" (local exec)
    // or "addl x@indntpoff,%eax" (initial exec)
    // or "addl x@gotntpoff(%ebx) ,%eax" (initial exec, 32-bit pic)
    SDValue TGA =
        DAG.getTargetGlobalAddress(GA->getGlobal(), dl, GA->getValueType(0),
                GA->getOffset(), OperandFlags);
    SDValue Offset = DAG.getNode(WrapperKind, dl, PtrVT, TGA);

    if (model == TLSModel::InitialExec) {
        if (isPIC && !is64Bit) {
            Offset = DAG.getNode(ISD::ADD, dl, PtrVT,
                    DAG.getNode(Cse523ISD::GlobalBaseReg, SDLoc(), PtrVT),
                    Offset);
        }

        Offset = DAG.getLoad(PtrVT, dl, DAG.getEntryNode(), Offset,
                MachinePointerInfo::getGOT(), false, false, false, 0);
    }

    // The address of the thread local variable is the add of the thread
    // pointer with the offset of the variable.
    return DAG.getNode(ISD::ADD, dl, PtrVT, ThreadPointer, Offset);
}

SDValue
Cse523TargetLowering::LowerGlobalTLSAddress(SDValue Op, SelectionDAG &DAG) const {

    GlobalAddressSDNode *GA = cast<GlobalAddressSDNode>(Op);
    const GlobalValue *GV = GA->getGlobal();

    if (Subtarget->isTargetELF()) {
        TLSModel::Model model = getTargetMachine().getTLSModel(GV);

        switch (model) {
            case TLSModel::GeneralDynamic:
                return LowerToTLSGeneralDynamicModel64(GA, DAG, getPointerTy());
            case TLSModel::LocalDynamic:
                return LowerToTLSLocalDynamicModel(GA, DAG, getPointerTy(),
                        Subtarget->is64Bit());
            case TLSModel::InitialExec:
            case TLSModel::LocalExec:
                return LowerToTLSExecModel(GA, DAG, getPointerTy(), model,
                        Subtarget->is64Bit(),
                        getTargetMachine().getRelocationModel() == Reloc::PIC_);
        }
        llvm_unreachable("Unknown TLS model.");
    }

    if (Subtarget->isTargetDarwin()) {
        // Darwin only has one model of TLS.  Lower to that.
        unsigned char OpFlag = 0;
        unsigned WrapperKind = Subtarget->isPICStyleRIPRel() ?
            Cse523ISD::WrapperRIP : Cse523ISD::Wrapper;

        // In PIC mode (unless we're in RIPRel PIC mode) we add an offset to the
        // global base reg.
        bool PIC32 = (getTargetMachine().getRelocationModel() == Reloc::PIC_) &&
            !Subtarget->is64Bit();
        if (PIC32)
            OpFlag = Cse523II::MO_TLVP_PIC_BASE;
        else
            OpFlag = Cse523II::MO_TLVP;
        SDLoc DL(Op);
        SDValue Result = DAG.getTargetGlobalAddress(GA->getGlobal(), DL,
                GA->getValueType(0),
                GA->getOffset(), OpFlag);
        SDValue Offset = DAG.getNode(WrapperKind, DL, getPointerTy(), Result);

        // With PIC32, the address is actually $g + Offset.
        if (PIC32)
            Offset = DAG.getNode(ISD::ADD, DL, getPointerTy(),
                    DAG.getNode(Cse523ISD::GlobalBaseReg,
                        SDLoc(), getPointerTy()),
                    Offset);

        // Lowering the machine isd will make sure everything is in the right
        // location.
        SDValue Chain = DAG.getEntryNode();
        SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
        SDValue Args[] = { Chain, Offset };
        Chain = DAG.getNode(Cse523ISD::TLSCALL, DL, NodeTys, Args, 2);

        // TLSCALL will be codegen'ed as call. Inform MFI that function has calls.
        MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
        MFI->setAdjustsStack(true);

        // And our return value (tls address) is in the standard call return value
        // location.
        unsigned Reg = Cse523::RAX;
        return DAG.getCopyFromReg(Chain, DL, Reg, getPointerTy(),
                Chain.getValue(1));
    }

    if (Subtarget->isTargetWindows() || Subtarget->isTargetMingw()) {
        // Just use the implicit TLS architecture
        // Need to generate someting similar to:
        //   mov     rdx, qword [gs:abs 58H]; Load pointer to ThreadLocalStorage
        //                                  ; from TEB
        //   mov     ecx, dword [rel _tls_index]: Load index (from C runtime)
        //   mov     rcx, qword [rdx+rcx*8]
        //   mov     eax, .tls$:tlsvar
        //   [rax+rcx] contains the address
        // Windows 64bit: gs:0x58
        // Windows 32bit: fs:__tls_array

        // If GV is an alias then use the aliasee for determining
        // thread-localness.
        if (const GlobalAlias *GA = dyn_cast<GlobalAlias>(GV))
            GV = GA->resolveAliasedGlobal(false);
        SDLoc dl(GA);
        SDValue Chain = DAG.getEntryNode();

        // Get the Thread Pointer, which is %fs:__tls_array (32-bit) or
        // %gs:0x58 (64-bit). On MinGW, __tls_array is not available, so directly
        // use its literal value of 0x2C.
        Value *Ptr = Constant::getNullValue(Subtarget->is64Bit()
                ? Type::getInt8PtrTy(*DAG.getContext(),
                    256)
                : Type::getInt32PtrTy(*DAG.getContext(),
                    257));

        SDValue TlsArray = Subtarget->is64Bit() ? DAG.getIntPtrConstant(0x58) :
            (Subtarget->isTargetMingw() ? DAG.getIntPtrConstant(0x2C) :
             DAG.getExternalSymbol("_tls_array", getPointerTy()));

        SDValue ThreadPointer = DAG.getLoad(getPointerTy(), dl, Chain, TlsArray,
                MachinePointerInfo(Ptr),
                false, false, false, 0);

        // Load the _tls_index variable
        SDValue IDX = DAG.getExternalSymbol("_tls_index", getPointerTy());
        if (Subtarget->is64Bit())
            IDX = DAG.getExtLoad(ISD::ZEXTLOAD, dl, getPointerTy(), Chain,
                    IDX, MachinePointerInfo(), MVT::i32,
                    false, false, 0);
        else
            IDX = DAG.getLoad(getPointerTy(), dl, Chain, IDX, MachinePointerInfo(),
                    false, false, false, 0);

        SDValue Scale = DAG.getConstant(Log2_64_Ceil(TD->getPointerSize()),
                getPointerTy());
        IDX = DAG.getNode(ISD::SHL, dl, getPointerTy(), IDX, Scale);

        SDValue res = DAG.getNode(ISD::ADD, dl, getPointerTy(), ThreadPointer, IDX);
        res = DAG.getLoad(getPointerTy(), dl, Chain, res, MachinePointerInfo(),
                false, false, false, 0);

        // Get the offset of start of .tls section
        SDValue TGA = DAG.getTargetGlobalAddress(GA->getGlobal(), dl,
                GA->getValueType(0),
                GA->getOffset(), Cse523II::MO_SECREL);
        SDValue Offset = DAG.getNode(Cse523ISD::Wrapper, dl, getPointerTy(), TGA);

        // The address of the thread local variable is the add of the thread
        // pointer with the offset of the variable.
        return DAG.getNode(ISD::ADD, dl, getPointerTy(), res, Offset);
    }

    llvm_unreachable("TLS not implemented for this target.");
}

/// LowerShiftParts - Lower SRA_PARTS and friends, which return two i32 values
/// and take a 2 x i32 value to shift plus a shift amount.
static SDValue LowerShiftParts(SDValue Op, SelectionDAG &DAG) {
    assert(Op.getNumOperands() == 3 && "Not a double-shift!");
    MVT VT = Op.getSimpleValueType();
    unsigned VTBits = VT.getSizeInBits();
    SDLoc dl(Op);
    bool isSRA = Op.getOpcode() == ISD::SRA_PARTS;
    SDValue ShOpLo = Op.getOperand(0);
    SDValue ShOpHi = Op.getOperand(1);
    SDValue ShAmt  = Op.getOperand(2);
    // Cse523ISD::SHLD and Cse523ISD::SHRD have defined overflow behavior but the
    // generic ISD nodes haven't. Insert an AND to be safe, it's optimized away
    // during isel.
    SDValue SafeShAmt = DAG.getNode(ISD::AND, dl, MVT::i64, ShAmt,
            DAG.getConstant(VTBits - 1, MVT::i64));
    SDValue Tmp1 = isSRA ? DAG.getNode(ISD::SRA, dl, VT, ShOpHi,
            DAG.getConstant(VTBits - 1, MVT::i64))
        : DAG.getConstant(0, VT);

    SDValue Tmp2, Tmp3;
    if (Op.getOpcode() == ISD::SHL_PARTS) {
        Tmp2 = DAG.getNode(Cse523ISD::SHLD, dl, VT, ShOpHi, ShOpLo, ShAmt);
        Tmp3 = DAG.getNode(ISD::SHL, dl, VT, ShOpLo, SafeShAmt);
    } else {
        Tmp2 = DAG.getNode(Cse523ISD::SHRD, dl, VT, ShOpLo, ShOpHi, ShAmt);
        Tmp3 = DAG.getNode(isSRA ? ISD::SRA : ISD::SRL, dl, VT, ShOpHi, SafeShAmt);
    }

    // If the shift amount is larger or equal than the width of a part we can't
    // rely on the results of shld/shrd. Insert a test and select the appropriate
    // values for large shift amounts.
    SDValue AndNode = DAG.getNode(ISD::AND, dl, MVT::i64, ShAmt,
            DAG.getConstant(VTBits, MVT::i64));
    SDValue Cond = DAG.getNode(Cse523ISD::CMP, dl, MVT::i64,
            AndNode, DAG.getConstant(0, MVT::i64));

    SDValue Hi, Lo;
    SDValue CC = DAG.getConstant(Cse523::COND_NE, MVT::i64);
    SDValue Ops0[4] = { Tmp2, Tmp3, CC, Cond };
    SDValue Ops1[4] = { Tmp3, Tmp1, CC, Cond };

    if (Op.getOpcode() == ISD::SHL_PARTS) {
        Hi = DAG.getNode(Cse523ISD::CMOV, dl, VT, Ops0, 4);
        Lo = DAG.getNode(Cse523ISD::CMOV, dl, VT, Ops1, 4);
    } else {
        Lo = DAG.getNode(Cse523ISD::CMOV, dl, VT, Ops0, 4);
        Hi = DAG.getNode(Cse523ISD::CMOV, dl, VT, Ops1, 4);
    }

    SDValue Ops[2] = { Lo, Hi };
    return DAG.getMergeValues(Ops, array_lengthof(Ops), dl);
}

SDValue Cse523TargetLowering::LowerSINT_TO_FP(SDValue Op,
        SelectionDAG &DAG) const {
    MVT SrcVT = Op.getOperand(0).getSimpleValueType();

    if (SrcVT.isVector())
        return SDValue();

    assert(SrcVT <= MVT::i64 && SrcVT >= MVT::i16 &&
            "Unknown SINT_TO_FP to lower!");

    // These are really Legal; return the operand so the caller accepts it as
    // Legal.
    if (SrcVT == MVT::i32 && isScalarFPTypeInSSEReg(Op.getValueType()))
        return Op;
    if (SrcVT == MVT::i64 && isScalarFPTypeInSSEReg(Op.getValueType()) &&
            Subtarget->is64Bit()) {
        return Op;
    }

    SDLoc dl(Op);
    unsigned Size = SrcVT.getSizeInBits()/8;
    MachineFunction &MF = DAG.getMachineFunction();
    int SSFI = MF.getFrameInfo()->CreateStackObject(Size, Size, false);
    SDValue StackSlot = DAG.getFrameIndex(SSFI, getPointerTy());
    SDValue Chain = DAG.getStore(DAG.getEntryNode(), dl, Op.getOperand(0),
            StackSlot,
            MachinePointerInfo::getFixedStack(SSFI),
            false, false, 0);
    return BuildFILD(Op, SrcVT, Chain, StackSlot, DAG);
}

SDValue Cse523TargetLowering::BuildFILD(SDValue Op, EVT SrcVT, SDValue Chain,
        SDValue StackSlot,
        SelectionDAG &DAG) const {
    // Build the FILD
    SDLoc DL(Op);
    SDVTList Tys;
    bool useSSE = isScalarFPTypeInSSEReg(Op.getValueType());
    if (useSSE)
        Tys = DAG.getVTList(MVT::f64, MVT::Other, MVT::Glue);
    else
        Tys = DAG.getVTList(Op.getValueType(), MVT::Other);

    unsigned ByteSize = SrcVT.getSizeInBits()/8;

    FrameIndexSDNode *FI = dyn_cast<FrameIndexSDNode>(StackSlot);
    MachineMemOperand *MMO;
    if (FI) {
        int SSFI = FI->getIndex();
        MMO =
            DAG.getMachineFunction()
            .getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                    MachineMemOperand::MOLoad, ByteSize, ByteSize);
    } else {
        MMO = cast<LoadSDNode>(StackSlot)->getMemOperand();
        StackSlot = StackSlot.getOperand(1);
    }
    SDValue Ops[] = { Chain, StackSlot, DAG.getValueType(SrcVT) };
    SDValue Result = DAG.getMemIntrinsicNode(useSSE ? Cse523ISD::FILD_FLAG :
            Cse523ISD::FILD, DL,
            Tys, Ops, array_lengthof(Ops),
            SrcVT, MMO);

    if (useSSE) {
        Chain = Result.getValue(1);
        SDValue InFlag = Result.getValue(2);

        // FIXME: Currently the FST is flagged to the FILD_FLAG. This
        // shouldn't be necessary except that RFP cannot be live across
        // multiple blocks. When stackifier is fixed, they can be uncoupled.
        MachineFunction &MF = DAG.getMachineFunction();
        unsigned SSFISize = Op.getValueType().getSizeInBits()/8;
        int SSFI = MF.getFrameInfo()->CreateStackObject(SSFISize, SSFISize, false);
        SDValue StackSlot = DAG.getFrameIndex(SSFI, getPointerTy());
        Tys = DAG.getVTList(MVT::Other);
        SDValue Ops[] = {
            Chain, Result, StackSlot, DAG.getValueType(Op.getValueType()), InFlag
        };
        MachineMemOperand *MMO =
            DAG.getMachineFunction()
            .getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                    MachineMemOperand::MOStore, SSFISize, SSFISize);

        Chain = DAG.getMemIntrinsicNode(Cse523ISD::FST, DL, Tys,
                Ops, array_lengthof(Ops),
                Op.getValueType(), MMO);
        Result = DAG.getLoad(Op.getValueType(), DL, Chain, StackSlot,
                MachinePointerInfo::getFixedStack(SSFI),
                false, false, false, 0);
    }

    return Result;
}

// LowerUINT_TO_FP_i64 - 64-bit unsigned integer to double expansion.
SDValue Cse523TargetLowering::LowerUINT_TO_FP_i64(SDValue Op,
        SelectionDAG &DAG) const {
    // This algorithm is not obvious. Here it is what we're trying to output:
    /*
       movq       %rax,  %xmm0
       punpckldq  (c0),  %xmm0  // c0: (uint4){ 0x43300000U, 0x45300000U, 0U, 0U }
       subpd      (c1),  %xmm0  // c1: (double2){ 0x1.0p52, 0x1.0p52 * 0x1.0p32 }
#ifdef __SSE3__
haddpd   %xmm0, %xmm0
#else
pshufd   $0x4e, %xmm0, %xmm1
addpd    %xmm1, %xmm0
#endif
*/

    SDLoc dl(Op);
    LLVMContext *Context = DAG.getContext();

    // Build some magic constants.
    static const uint32_t CV0[] = { 0x43300000, 0x45300000, 0, 0 };
    Constant *C0 = ConstantDataVector::get(*Context, CV0);
    SDValue CPIdx0 = DAG.getConstantPool(C0, getPointerTy(), 16);

    SmallVector<Constant*,2> CV1;
    CV1.push_back(
            ConstantFP::get(*Context, APFloat(APFloat::IEEEdouble,
                    APInt(64, 0x4330000000000000ULL))));
    CV1.push_back(
            ConstantFP::get(*Context, APFloat(APFloat::IEEEdouble,
                    APInt(64, 0x4530000000000000ULL))));
    Constant *C1 = ConstantVector::get(CV1);
    SDValue CPIdx1 = DAG.getConstantPool(C1, getPointerTy(), 16);

    // Load the 64-bit value into an XMM register.
    SDValue XR1 = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, MVT::v2i64,
            Op.getOperand(0));
    SDValue CLod0 = DAG.getLoad(MVT::v4i32, dl, DAG.getEntryNode(), CPIdx0,
            MachinePointerInfo::getConstantPool(),
            false, false, false, 16);
    SDValue Unpck1 = getUnpackl(DAG, dl, MVT::v4i32,
            DAG.getNode(ISD::BITCAST, dl, MVT::v4i32, XR1),
            CLod0);

    SDValue CLod1 = DAG.getLoad(MVT::v2f64, dl, CLod0.getValue(1), CPIdx1,
            MachinePointerInfo::getConstantPool(),
            false, false, false, 16);
    SDValue XR2F = DAG.getNode(ISD::BITCAST, dl, MVT::v2f64, Unpck1);
    SDValue Sub = DAG.getNode(ISD::FSUB, dl, MVT::v2f64, XR2F, CLod1);
    SDValue Result;

    if (Subtarget->hasSSE3()) {
        // FIXME: The 'haddpd' instruction may be slower than 'movhlps + addsd'.
        Result = DAG.getNode(Cse523ISD::FHADD, dl, MVT::v2f64, Sub, Sub);
    } else {
        SDValue S2F = DAG.getNode(ISD::BITCAST, dl, MVT::v4i32, Sub);
        SDValue Shuffle = getTargetShuffleNode(Cse523ISD::PSHUFD, dl, MVT::v4i32,
                S2F, 0x4E, DAG);
        Result = DAG.getNode(ISD::FADD, dl, MVT::v2f64,
                DAG.getNode(ISD::BITCAST, dl, MVT::v2f64, Shuffle),
                Sub);
    }

    return DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::f64, Result,
            DAG.getIntPtrConstant(0));
}

// LowerUINT_TO_FP_i32 - 32-bit unsigned integer to float expansion.
SDValue Cse523TargetLowering::LowerUINT_TO_FP_i32(SDValue Op,
        SelectionDAG &DAG) const {
    SDLoc dl(Op);
    // FP constant to bias correct the final result.
    SDValue Bias = DAG.getConstantFP(BitsToDouble(0x4330000000000000ULL),
            MVT::f64);

    // Load the 32-bit value into an XMM register.
    SDValue Load = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, MVT::v4i32,
            Op.getOperand(0));

    // Zero out the upper parts of the register.
    Load = getShuffleVectorZeroOrUndef(Load, 0, true, Subtarget, DAG);

    Load = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::f64,
            DAG.getNode(ISD::BITCAST, dl, MVT::v2f64, Load),
            DAG.getIntPtrConstant(0));

    // Or the load with the bias.
    SDValue Or = DAG.getNode(ISD::OR, dl, MVT::v2i64,
            DAG.getNode(ISD::BITCAST, dl, MVT::v2i64,
                DAG.getNode(ISD::SCALAR_TO_VECTOR, dl,
                    MVT::v2f64, Load)),
            DAG.getNode(ISD::BITCAST, dl, MVT::v2i64,
                DAG.getNode(ISD::SCALAR_TO_VECTOR, dl,
                    MVT::v2f64, Bias)));
    Or = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::f64,
            DAG.getNode(ISD::BITCAST, dl, MVT::v2f64, Or),
            DAG.getIntPtrConstant(0));

    // Subtract the bias.
    SDValue Sub = DAG.getNode(ISD::FSUB, dl, MVT::f64, Or, Bias);

    // Handle final rounding.
    EVT DestVT = Op.getValueType();

    if (DestVT.bitsLT(MVT::f64))
        return DAG.getNode(ISD::FP_ROUND, dl, DestVT, Sub,
                DAG.getIntPtrConstant(0));
    if (DestVT.bitsGT(MVT::f64))
        return DAG.getNode(ISD::FP_EXTEND, dl, DestVT, Sub);

    // Handle final rounding.
    return Sub;
}

SDValue Cse523TargetLowering::lowerUINT_TO_FP_vec(SDValue Op,
        SelectionDAG &DAG) const {
    SDValue N0 = Op.getOperand(0);
    MVT SVT = N0.getSimpleValueType();
    SDLoc dl(Op);

    assert((SVT == MVT::v4i8 || SVT == MVT::v4i16 ||
                SVT == MVT::v8i8 || SVT == MVT::v8i16) &&
            "Custom UINT_TO_FP is not supported!");

    MVT NVT = MVT::getVectorVT(MVT::i32, SVT.getVectorNumElements());
    return DAG.getNode(ISD::SINT_TO_FP, dl, Op.getValueType(),
            DAG.getNode(ISD::ZERO_EXTEND, dl, NVT, N0));
}

SDValue Cse523TargetLowering::LowerUINT_TO_FP(SDValue Op,
        SelectionDAG &DAG) const {
    SDValue N0 = Op.getOperand(0);
    SDLoc dl(Op);

    if (Op.getValueType().isVector())
        return lowerUINT_TO_FP_vec(Op, DAG);

    // Since UINT_TO_FP is legal (it's marked custom), dag combiner won't
    // optimize it to a SINT_TO_FP when the sign bit is known zero. Perform
    // the optimization here.
    if (DAG.SignBitIsZero(N0))
        return DAG.getNode(ISD::SINT_TO_FP, dl, Op.getValueType(), N0);

    MVT SrcVT = N0.getSimpleValueType();
    MVT DstVT = Op.getSimpleValueType();
    if (SrcVT == MVT::i64 && DstVT == MVT::f64 && Cse523ScalarSSEf64)
        return LowerUINT_TO_FP_i64(Op, DAG);
    if (SrcVT == MVT::i32 && Cse523ScalarSSEf64)
        return LowerUINT_TO_FP_i32(Op, DAG);
    if (Subtarget->is64Bit() && SrcVT == MVT::i64 && DstVT == MVT::f32)
        return SDValue();

    // Make a 64-bit buffer, and use it to build an FILD.
    SDValue StackSlot = DAG.CreateStackTemporary(MVT::i64);
    if (SrcVT == MVT::i32) {
        SDValue WordOff = DAG.getConstant(4, getPointerTy());
        SDValue OffsetSlot = DAG.getNode(ISD::ADD, dl,
                getPointerTy(), StackSlot, WordOff);
        SDValue Store1 = DAG.getStore(DAG.getEntryNode(), dl, Op.getOperand(0),
                StackSlot, MachinePointerInfo(),
                false, false, 0);
        SDValue Store2 = DAG.getStore(Store1, dl, DAG.getConstant(0, MVT::i32),
                OffsetSlot, MachinePointerInfo(),
                false, false, 0);
        SDValue Fild = BuildFILD(Op, MVT::i64, Store2, StackSlot, DAG);
        return Fild;
    }

    assert(SrcVT == MVT::i64 && "Unexpected type in UINT_TO_FP");
    SDValue Store = DAG.getStore(DAG.getEntryNode(), dl, Op.getOperand(0),
            StackSlot, MachinePointerInfo(),
            false, false, 0);
    // For i64 source, we need to add the appropriate power of 2 if the input
    // was negative.  This is the same as the optimization in
    // DAGTypeLegalizer::ExpandIntOp_UNIT_TO_FP, and for it to be safe here,
    // we must be careful to do the computation in x87 extended precision, not
    // in SSE. (The generic code can't know it's OK to do this, or how to.)
    int SSFI = cast<FrameIndexSDNode>(StackSlot)->getIndex();
    MachineMemOperand *MMO =
        DAG.getMachineFunction()
        .getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                MachineMemOperand::MOLoad, 8, 8);

    SDVTList Tys = DAG.getVTList(MVT::f80, MVT::Other);
    SDValue Ops[] = { Store, StackSlot, DAG.getValueType(MVT::i64) };
    SDValue Fild = DAG.getMemIntrinsicNode(Cse523ISD::FILD, dl, Tys, Ops,
            array_lengthof(Ops), MVT::i64, MMO);

    APInt FF(32, 0x5F800000ULL);

    // Check whether the sign bit is set.
    SDValue SignSet = DAG.getSetCC(dl,
            getSetCCResultType(*DAG.getContext(), MVT::i64),
            Op.getOperand(0), DAG.getConstant(0, MVT::i64),
            ISD::SETLT);

    // Build a 64 bit pair (0, FF) in the constant pool, with FF in the lo bits.
    SDValue FudgePtr = DAG.getConstantPool(
            ConstantInt::get(*DAG.getContext(), FF.zext(64)),
            getPointerTy());

    // Get a pointer to FF if the sign bit was set, or to 0 otherwise.
    SDValue Zero = DAG.getIntPtrConstant(0);
    SDValue Four = DAG.getIntPtrConstant(4);
    SDValue Offset = DAG.getNode(ISD::SELECT, dl, Zero.getValueType(), SignSet,
            Zero, Four);
    FudgePtr = DAG.getNode(ISD::ADD, dl, getPointerTy(), FudgePtr, Offset);

    // Load the value out, extending it from f32 to f80.
    // FIXME: Avoid the extend by constructing the right constant pool?
    SDValue Fudge = DAG.getExtLoad(ISD::EXTLOAD, dl, MVT::f80, DAG.getEntryNode(),
            FudgePtr, MachinePointerInfo::getConstantPool(),
            MVT::f32, false, false, 4);
    // Extend everything to 80 bits to force it to be done on x87.
    SDValue Add = DAG.getNode(ISD::FADD, dl, MVT::f80, Fild, Fudge);
    return DAG.getNode(ISD::FP_ROUND, dl, DstVT, Add, DAG.getIntPtrConstant(0));
}

std::pair<SDValue,SDValue>
Cse523TargetLowering:: FP_TO_INTHelper(SDValue Op, SelectionDAG &DAG,
        bool IsSigned, bool IsReplace) const {
    SDLoc DL(Op);

    EVT DstTy = Op.getValueType();

    if (!IsSigned && !isIntegerTypeFTOL(DstTy)) {
        assert(DstTy == MVT::i32 && "Unexpected FP_TO_UINT");
        DstTy = MVT::i64;
    }

    assert(DstTy.getSimpleVT() <= MVT::i64 &&
            DstTy.getSimpleVT() >= MVT::i16 &&
            "Unknown FP_TO_INT to lower!");

    // These are really Legal.
    if (DstTy == MVT::i32 &&
            isScalarFPTypeInSSEReg(Op.getOperand(0).getValueType()))
        return std::make_pair(SDValue(), SDValue());
    if (Subtarget->is64Bit() &&
            DstTy == MVT::i64 &&
            isScalarFPTypeInSSEReg(Op.getOperand(0).getValueType()))
        return std::make_pair(SDValue(), SDValue());

    // We lower FP->int64 either into FISTP64 followed by a load from a temporary
    // stack slot, or into the FTOL runtime function.
    MachineFunction &MF = DAG.getMachineFunction();
    unsigned MemSize = DstTy.getSizeInBits()/8;
    int SSFI = MF.getFrameInfo()->CreateStackObject(MemSize, MemSize, false);
    SDValue StackSlot = DAG.getFrameIndex(SSFI, getPointerTy());

    unsigned Opc;
    if (!IsSigned && isIntegerTypeFTOL(DstTy))
        Opc = Cse523ISD::WIN_FTOL;
    else
        switch (DstTy.getSimpleVT().SimpleTy) {
            default: llvm_unreachable("Invalid FP_TO_SINT to lower!");
            case MVT::i16: Opc = Cse523ISD::FP_TO_INT16_IN_MEM; break;
            case MVT::i32: Opc = Cse523ISD::FP_TO_INT32_IN_MEM; break;
            case MVT::i64: Opc = Cse523ISD::FP_TO_INT64_IN_MEM; break;
        }

    SDValue Chain = DAG.getEntryNode();
    SDValue Value = Op.getOperand(0);
    EVT TheVT = Op.getOperand(0).getValueType();
    // FIXME This causes a redundant load/store if the SSE-class value is already
    // in memory, such as if it is on the callstack.
    if (isScalarFPTypeInSSEReg(TheVT)) {
        assert(DstTy == MVT::i64 && "Invalid FP_TO_SINT to lower!");
        Chain = DAG.getStore(Chain, DL, Value, StackSlot,
                MachinePointerInfo::getFixedStack(SSFI),
                false, false, 0);
        SDVTList Tys = DAG.getVTList(Op.getOperand(0).getValueType(), MVT::Other);
        SDValue Ops[] = {
            Chain, StackSlot, DAG.getValueType(TheVT)
        };

        MachineMemOperand *MMO =
            MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                    MachineMemOperand::MOLoad, MemSize, MemSize);
        Value = DAG.getMemIntrinsicNode(Cse523ISD::FLD, DL, Tys, Ops,
                array_lengthof(Ops), DstTy, MMO);
        Chain = Value.getValue(1);
        SSFI = MF.getFrameInfo()->CreateStackObject(MemSize, MemSize, false);
        StackSlot = DAG.getFrameIndex(SSFI, getPointerTy());
    }

    MachineMemOperand *MMO =
        MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                MachineMemOperand::MOStore, MemSize, MemSize);

    if (Opc != Cse523ISD::WIN_FTOL) {
        // Build the FP_TO_INT*_IN_MEM
        SDValue Ops[] = { Chain, Value, StackSlot };
        SDValue FIST = DAG.getMemIntrinsicNode(Opc, DL, DAG.getVTList(MVT::Other),
                Ops, array_lengthof(Ops), DstTy,
                MMO);
        return std::make_pair(FIST, StackSlot);
    } else {
        SDValue ftol = DAG.getNode(Cse523ISD::WIN_FTOL, DL,
                DAG.getVTList(MVT::Other, MVT::Glue),
                Chain, Value);
        assert(0);
        // TODO: Replaced to RAX and RDX
        SDValue eax = DAG.getCopyFromReg(ftol, DL, Cse523::RAX,
                MVT::i32, ftol.getValue(1));
        SDValue edx = DAG.getCopyFromReg(eax.getValue(1), DL, Cse523::RDX,
                MVT::i32, eax.getValue(2));
        SDValue Ops[] = { eax, edx };
        SDValue pair = IsReplace
            ? DAG.getNode(ISD::BUILD_PAIR, DL, MVT::i64, Ops, array_lengthof(Ops))
            : DAG.getMergeValues(Ops, array_lengthof(Ops), DL);
        return std::make_pair(pair, SDValue());
    }
}

static SDValue LowerAVXExtend(SDValue Op, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    MVT VT = Op->getSimpleValueType(0);
    SDValue In = Op->getOperand(0);
    MVT InVT = In.getSimpleValueType();
    SDLoc dl(Op);

    // Optimize vectors in AVX mode:
    //
    //   v8i16 -> v8i32
    //   Use vpunpcklwd for 4 lower elements  v8i16 -> v4i32.
    //   Use vpunpckhwd for 4 upper elements  v8i16 -> v4i32.
    //   Concat upper and lower parts.
    //
    //   v4i32 -> v4i64
    //   Use vpunpckldq for 4 lower elements  v4i32 -> v2i64.
    //   Use vpunpckhdq for 4 upper elements  v4i32 -> v2i64.
    //   Concat upper and lower parts.
    //

    if (((VT != MVT::v16i16) || (InVT != MVT::v16i8)) &&
            ((VT != MVT::v8i32) || (InVT != MVT::v8i16)) &&
            ((VT != MVT::v4i64) || (InVT != MVT::v4i32)))
        return SDValue();

    if (Subtarget->hasInt256())
        return DAG.getNode(Cse523ISD::VZEXT, dl, VT, In);

    SDValue ZeroVec = getZeroVector(InVT, Subtarget, DAG, dl);
    SDValue Undef = DAG.getUNDEF(InVT);
    bool NeedZero = Op.getOpcode() == ISD::ZERO_EXTEND;
    SDValue OpLo = getUnpackl(DAG, dl, InVT, In, NeedZero ? ZeroVec : Undef);
    SDValue OpHi = getUnpackh(DAG, dl, InVT, In, NeedZero ? ZeroVec : Undef);

    MVT HVT = MVT::getVectorVT(VT.getVectorElementType(),
            VT.getVectorNumElements()/2);

    OpLo = DAG.getNode(ISD::BITCAST, dl, HVT, OpLo);
    OpHi = DAG.getNode(ISD::BITCAST, dl, HVT, OpHi);

    return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT, OpLo, OpHi);
}

static  SDValue LowerZERO_EXTEND_AVX512(SDValue Op,
        SelectionDAG &DAG) {
    MVT VT = Op->getSimpleValueType(0);
    SDValue In = Op->getOperand(0);
    MVT InVT = In.getSimpleValueType();
    SDLoc DL(Op);
    unsigned int NumElts = VT.getVectorNumElements();
    if (NumElts != 8 && NumElts != 16)
        return SDValue();

    if (VT.is512BitVector() && InVT.getVectorElementType() != MVT::i1)
        return DAG.getNode(Cse523ISD::VZEXT, DL, VT, In);

    EVT ExtVT = (NumElts == 8)? MVT::v8i64 : MVT::v16i32;
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    // Now we have only mask extension
    assert(InVT.getVectorElementType() == MVT::i1);
    SDValue Cst = DAG.getTargetConstant(1, ExtVT.getScalarType());
    const Constant *C = (dyn_cast<ConstantSDNode>(Cst))->getConstantIntValue();
    SDValue CP = DAG.getConstantPool(C, TLI.getPointerTy());
    unsigned Alignment = cast<ConstantPoolSDNode>(CP)->getAlignment();
    SDValue Ld = DAG.getLoad(Cst.getValueType(), DL, DAG.getEntryNode(), CP,
            MachinePointerInfo::getConstantPool(),
            false, false, false, Alignment);

    SDValue Brcst = DAG.getNode(Cse523ISD::VBROADCASTM, DL, ExtVT, In, Ld);
    if (VT.is512BitVector())
        return Brcst;
    return DAG.getNode(Cse523ISD::VTRUNC, DL, VT, Brcst);
}

static SDValue LowerANY_EXTEND(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    if (Subtarget->hasFp256()) {
        SDValue Res = LowerAVXExtend(Op, DAG, Subtarget);
        if (Res.getNode())
            return Res;
    }

    return SDValue();
}

static SDValue LowerZERO_EXTEND(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    SDLoc DL(Op);
    MVT VT = Op.getSimpleValueType();
    SDValue In = Op.getOperand(0);
    MVT SVT = In.getSimpleValueType();

    if (VT.is512BitVector() || SVT.getVectorElementType() == MVT::i1)
        return LowerZERO_EXTEND_AVX512(Op, DAG);

    if (Subtarget->hasFp256()) {
        SDValue Res = LowerAVXExtend(Op, DAG, Subtarget);
        if (Res.getNode())
            return Res;
    }

    assert(!VT.is256BitVector() || !SVT.is128BitVector() ||
            VT.getVectorNumElements() != SVT.getVectorNumElements());
    return SDValue();
}

SDValue Cse523TargetLowering::LowerTRUNCATE(SDValue Op, SelectionDAG &DAG) const {
    SDLoc DL(Op);
    MVT VT = Op.getSimpleValueType();
    SDValue In = Op.getOperand(0);
    MVT InVT = In.getSimpleValueType();

    if (VT == MVT::i1) {
        assert((InVT.isInteger() && (InVT.getSizeInBits() <= 64)) &&
                "Invalid scalar TRUNCATE operation");
        if (InVT == MVT::i32)
            return SDValue();
        if (InVT.getSizeInBits() == 64)
            In = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::i32, In);
        else if (InVT.getSizeInBits() < 32)
            In = DAG.getNode(ISD::ANY_EXTEND, DL, MVT::i32, In);
        return DAG.getNode(ISD::TRUNCATE, DL, VT, In);
    }
    assert(VT.getVectorNumElements() == InVT.getVectorNumElements() &&
            "Invalid TRUNCATE operation");

    if (InVT.is512BitVector() || VT.getVectorElementType() == MVT::i1) {
        if (VT.getVectorElementType().getSizeInBits() >=8)
            return DAG.getNode(Cse523ISD::VTRUNC, DL, VT, In);

        assert(VT.getVectorElementType() == MVT::i1 && "Unexpected vector type");
        unsigned NumElts = InVT.getVectorNumElements();
        assert ((NumElts == 8 || NumElts == 16) && "Unexpected vector type");
        if (InVT.getSizeInBits() < 512) {
            MVT ExtVT = (NumElts == 16)? MVT::v16i32 : MVT::v8i64;
            In = DAG.getNode(ISD::SIGN_EXTEND, DL, ExtVT, In);
            InVT = ExtVT;
        }

        SDValue Cst = DAG.getTargetConstant(1, InVT.getVectorElementType());
        const Constant *C = (dyn_cast<ConstantSDNode>(Cst))->getConstantIntValue();
        SDValue CP = DAG.getConstantPool(C, getPointerTy());
        unsigned Alignment = cast<ConstantPoolSDNode>(CP)->getAlignment();
        SDValue Ld = DAG.getLoad(Cst.getValueType(), DL, DAG.getEntryNode(), CP,
                MachinePointerInfo::getConstantPool(),
                false, false, false, Alignment);
        SDValue OneV = DAG.getNode(Cse523ISD::VBROADCAST, DL, InVT, Ld);
        SDValue And = DAG.getNode(ISD::AND, DL, InVT, OneV, In);
        return DAG.getNode(Cse523ISD::TESTM, DL, VT, And, And);
    }

    if ((VT == MVT::v4i32) && (InVT == MVT::v4i64)) {
        // On AVX2, v4i64 -> v4i32 becomes VPERMD.
        if (Subtarget->hasInt256()) {
            static const int ShufMask[] = {0, 2, 4, 6, -1, -1, -1, -1};
            In = DAG.getNode(ISD::BITCAST, DL, MVT::v8i32, In);
            In = DAG.getVectorShuffle(MVT::v8i32, DL, In, DAG.getUNDEF(MVT::v8i32),
                    ShufMask);
            return DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, VT, In,
                    DAG.getIntPtrConstant(0));
        }

        // On AVX, v4i64 -> v4i32 becomes a sequence that uses PSHUFD and MOVLHPS.
        SDValue OpLo = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::v2i64, In,
                DAG.getIntPtrConstant(0));
        SDValue OpHi = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::v2i64, In,
                DAG.getIntPtrConstant(2));

        OpLo = DAG.getNode(ISD::BITCAST, DL, MVT::v4i32, OpLo);
        OpHi = DAG.getNode(ISD::BITCAST, DL, MVT::v4i32, OpHi);

        // The PSHUFD mask:
        static const int ShufMask1[] = {0, 2, 0, 0};
        SDValue Undef = DAG.getUNDEF(VT);
        OpLo = DAG.getVectorShuffle(VT, DL, OpLo, Undef, ShufMask1);
        OpHi = DAG.getVectorShuffle(VT, DL, OpHi, Undef, ShufMask1);

        // The MOVLHPS mask:
        static const int ShufMask2[] = {0, 1, 4, 5};
        return DAG.getVectorShuffle(VT, DL, OpLo, OpHi, ShufMask2);
    }

    if ((VT == MVT::v8i16) && (InVT == MVT::v8i32)) {
        // On AVX2, v8i32 -> v8i16 becomed PSHUFB.
        if (Subtarget->hasInt256()) {
            In = DAG.getNode(ISD::BITCAST, DL, MVT::v32i8, In);

            SmallVector<SDValue,32> pshufbMask;
            for (unsigned i = 0; i < 2; ++i) {
                pshufbMask.push_back(DAG.getConstant(0x0, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0x1, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0x4, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0x5, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0x8, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0x9, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0xc, MVT::i8));
                pshufbMask.push_back(DAG.getConstant(0xd, MVT::i8));
                for (unsigned j = 0; j < 8; ++j)
                    pshufbMask.push_back(DAG.getConstant(0x80, MVT::i8));
            }
            SDValue BV = DAG.getNode(ISD::BUILD_VECTOR, DL, MVT::v32i8,
                    &pshufbMask[0], 32);
            In = DAG.getNode(Cse523ISD::PSHUFB, DL, MVT::v32i8, In, BV);
            In = DAG.getNode(ISD::BITCAST, DL, MVT::v4i64, In);

            static const int ShufMask[] = {0,  2,  -1,  -1};
            In = DAG.getVectorShuffle(MVT::v4i64, DL,  In, DAG.getUNDEF(MVT::v4i64),
                    &ShufMask[0]);
            In = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::v2i64, In,
                    DAG.getIntPtrConstant(0));
            return DAG.getNode(ISD::BITCAST, DL, VT, In);
        }

        SDValue OpLo = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::v4i32, In,
                DAG.getIntPtrConstant(0));

        SDValue OpHi = DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, MVT::v4i32, In,
                DAG.getIntPtrConstant(4));

        OpLo = DAG.getNode(ISD::BITCAST, DL, MVT::v16i8, OpLo);
        OpHi = DAG.getNode(ISD::BITCAST, DL, MVT::v16i8, OpHi);

        // The PSHUFB mask:
        static const int ShufMask1[] = {0,  1,  4,  5,  8,  9, 12, 13,
            -1, -1, -1, -1, -1, -1, -1, -1};

        SDValue Undef = DAG.getUNDEF(MVT::v16i8);
        OpLo = DAG.getVectorShuffle(MVT::v16i8, DL, OpLo, Undef, ShufMask1);
        OpHi = DAG.getVectorShuffle(MVT::v16i8, DL, OpHi, Undef, ShufMask1);

        OpLo = DAG.getNode(ISD::BITCAST, DL, MVT::v4i32, OpLo);
        OpHi = DAG.getNode(ISD::BITCAST, DL, MVT::v4i32, OpHi);

        // The MOVLHPS Mask:
        static const int ShufMask2[] = {0, 1, 4, 5};
        SDValue res = DAG.getVectorShuffle(MVT::v4i32, DL, OpLo, OpHi, ShufMask2);
        return DAG.getNode(ISD::BITCAST, DL, MVT::v8i16, res);
    }

    // Handle truncation of V256 to V128 using shuffles.
    if (!VT.is128BitVector() || !InVT.is256BitVector())
        return SDValue();

    assert(Subtarget->hasFp256() && "256-bit vector without AVX!");

    unsigned NumElems = VT.getVectorNumElements();
    MVT NVT = MVT::getVectorVT(VT.getVectorElementType(), NumElems * 2);

    SmallVector<int, 16> MaskVec(NumElems * 2, -1);
    // Prepare truncation shuffle mask
    for (unsigned i = 0; i != NumElems; ++i)
        MaskVec[i] = i * 2;
    SDValue V = DAG.getVectorShuffle(NVT, DL,
            DAG.getNode(ISD::BITCAST, DL, NVT, In),
            DAG.getUNDEF(NVT), &MaskVec[0]);
    return DAG.getNode(ISD::EXTRACT_SUBVECTOR, DL, VT, V,
            DAG.getIntPtrConstant(0));
}

SDValue Cse523TargetLowering::LowerFP_TO_SINT(SDValue Op,
        SelectionDAG &DAG) const {
    MVT VT = Op.getSimpleValueType();
    if (VT.isVector()) {
        if (VT == MVT::v8i16)
            return DAG.getNode(ISD::TRUNCATE, SDLoc(Op), VT,
                    DAG.getNode(ISD::FP_TO_SINT, SDLoc(Op),
                        MVT::v8i32, Op.getOperand(0)));
        return SDValue();
    }

    std::pair<SDValue,SDValue> Vals = FP_TO_INTHelper(Op, DAG,
            /*IsSigned=*/ true, /*IsReplace=*/ false);
    SDValue FIST = Vals.first, StackSlot = Vals.second;
    // If FP_TO_INTHelper failed, the node is actually supposed to be Legal.
    if (FIST.getNode() == 0) return Op;

    if (StackSlot.getNode())
        // Load the result.
        return DAG.getLoad(Op.getValueType(), SDLoc(Op),
                FIST, StackSlot, MachinePointerInfo(),
                false, false, false, 0);

    // The node is the result.
    return FIST;
}

SDValue Cse523TargetLowering::LowerFP_TO_UINT(SDValue Op,
        SelectionDAG &DAG) const {
    std::pair<SDValue,SDValue> Vals = FP_TO_INTHelper(Op, DAG,
            /*IsSigned=*/ false, /*IsReplace=*/ false);
    SDValue FIST = Vals.first, StackSlot = Vals.second;
    assert(FIST.getNode() && "Unexpected failure");

    if (StackSlot.getNode())
        // Load the result.
        return DAG.getLoad(Op.getValueType(), SDLoc(Op),
                FIST, StackSlot, MachinePointerInfo(),
                false, false, false, 0);

    // The node is the result.
    return FIST;
}

static SDValue LowerFP_EXTEND(SDValue Op, SelectionDAG &DAG) {
    SDLoc DL(Op);
    MVT VT = Op.getSimpleValueType();
    SDValue In = Op.getOperand(0);
    MVT SVT = In.getSimpleValueType();

    assert(SVT == MVT::v2f32 && "Only customize MVT::v2f32 type legalization!");

    return DAG.getNode(Cse523ISD::VFPEXT, DL, VT,
            DAG.getNode(ISD::CONCAT_VECTORS, DL, MVT::v4f32,
                In, DAG.getUNDEF(SVT)));
}

static SDValue LowerFABS(SDValue Op, SelectionDAG &DAG) {
    LLVMContext *Context = DAG.getContext();
    SDLoc dl(Op);
    MVT VT = Op.getSimpleValueType();
    MVT EltVT = VT;
    unsigned NumElts = VT == MVT::f64 ? 2 : 4;
    if (VT.isVector()) {
        EltVT = VT.getVectorElementType();
        NumElts = VT.getVectorNumElements();
    }
    Constant *C;
    if (EltVT == MVT::f64)
        C = ConstantFP::get(*Context, APFloat(APFloat::IEEEdouble,
                    APInt(64, ~(1ULL << 63))));
    else
        C = ConstantFP::get(*Context, APFloat(APFloat::IEEEsingle,
                    APInt(32, ~(1U << 31))));
    C = ConstantVector::getSplat(NumElts, C);
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    SDValue CPIdx = DAG.getConstantPool(C, TLI.getPointerTy());
    unsigned Alignment = cast<ConstantPoolSDNode>(CPIdx)->getAlignment();
    SDValue Mask = DAG.getLoad(VT, dl, DAG.getEntryNode(), CPIdx,
            MachinePointerInfo::getConstantPool(),
            false, false, false, Alignment);
    if (VT.isVector()) {
        MVT ANDVT = VT.is128BitVector() ? MVT::v2i64 : MVT::v4i64;
        return DAG.getNode(ISD::BITCAST, dl, VT,
                DAG.getNode(ISD::AND, dl, ANDVT,
                    DAG.getNode(ISD::BITCAST, dl, ANDVT,
                        Op.getOperand(0)),
                    DAG.getNode(ISD::BITCAST, dl, ANDVT, Mask)));
    }
    return DAG.getNode(Cse523ISD::FAND, dl, VT, Op.getOperand(0), Mask);
}

static SDValue LowerFNEG(SDValue Op, SelectionDAG &DAG) {
    LLVMContext *Context = DAG.getContext();
    SDLoc dl(Op);
    MVT VT = Op.getSimpleValueType();
    MVT EltVT = VT;
    unsigned NumElts = VT == MVT::f64 ? 2 : 4;
    if (VT.isVector()) {
        EltVT = VT.getVectorElementType();
        NumElts = VT.getVectorNumElements();
    }
    Constant *C;
    if (EltVT == MVT::f64)
        C = ConstantFP::get(*Context, APFloat(APFloat::IEEEdouble,
                    APInt(64, 1ULL << 63)));
    else
        C = ConstantFP::get(*Context, APFloat(APFloat::IEEEsingle,
                    APInt(32, 1U << 31)));
    C = ConstantVector::getSplat(NumElts, C);
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    SDValue CPIdx = DAG.getConstantPool(C, TLI.getPointerTy());
    unsigned Alignment = cast<ConstantPoolSDNode>(CPIdx)->getAlignment();
    SDValue Mask = DAG.getLoad(VT, dl, DAG.getEntryNode(), CPIdx,
            MachinePointerInfo::getConstantPool(),
            false, false, false, Alignment);
    if (VT.isVector()) {
        MVT XORVT = MVT::getVectorVT(MVT::i64, VT.getSizeInBits()/64);
        return DAG.getNode(ISD::BITCAST, dl, VT,
                DAG.getNode(ISD::XOR, dl, XORVT,
                    DAG.getNode(ISD::BITCAST, dl, XORVT,
                        Op.getOperand(0)),
                    DAG.getNode(ISD::BITCAST, dl, XORVT, Mask)));
    }

    return DAG.getNode(Cse523ISD::FXOR, dl, VT, Op.getOperand(0), Mask);
}

static SDValue LowerFCOPYSIGN(SDValue Op, SelectionDAG &DAG) {
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    LLVMContext *Context = DAG.getContext();
    SDValue Op0 = Op.getOperand(0);
    SDValue Op1 = Op.getOperand(1);
    SDLoc dl(Op);
    MVT VT = Op.getSimpleValueType();
    MVT SrcVT = Op1.getSimpleValueType();

    // If second operand is smaller, extend it first.
    if (SrcVT.bitsLT(VT)) {
        Op1 = DAG.getNode(ISD::FP_EXTEND, dl, VT, Op1);
        SrcVT = VT;
    }
    // And if it is bigger, shrink it first.
    if (SrcVT.bitsGT(VT)) {
        Op1 = DAG.getNode(ISD::FP_ROUND, dl, VT, Op1, DAG.getIntPtrConstant(1));
        SrcVT = VT;
    }

    // At this point the operands and the result should have the same
    // type, and that won't be f80 since that is not custom lowered.

    // First get the sign bit of second operand.
    SmallVector<Constant*,4> CV;
    if (SrcVT == MVT::f64) {
        const fltSemantics &Sem = APFloat::IEEEdouble;
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(64, 1ULL << 63))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(64, 0))));
    } else {
        const fltSemantics &Sem = APFloat::IEEEsingle;
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 1U << 31))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
    }
    Constant *C = ConstantVector::get(CV);
    SDValue CPIdx = DAG.getConstantPool(C, TLI.getPointerTy(), 16);
    SDValue Mask1 = DAG.getLoad(SrcVT, dl, DAG.getEntryNode(), CPIdx,
            MachinePointerInfo::getConstantPool(),
            false, false, false, 16);
    SDValue SignBit = DAG.getNode(Cse523ISD::FAND, dl, SrcVT, Op1, Mask1);

    // Shift sign bit right or left if the two operands have different types.
    if (SrcVT.bitsGT(VT)) {
        // Op0 is MVT::f32, Op1 is MVT::f64.
        SignBit = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, MVT::v2f64, SignBit);
        SignBit = DAG.getNode(Cse523ISD::FSRL, dl, MVT::v2f64, SignBit,
                DAG.getConstant(32, MVT::i32));
        SignBit = DAG.getNode(ISD::BITCAST, dl, MVT::v4f32, SignBit);
        SignBit = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::f32, SignBit,
                DAG.getIntPtrConstant(0));
    }

    // Clear first operand sign bit.
    CV.clear();
    if (VT == MVT::f64) {
        const fltSemantics &Sem = APFloat::IEEEdouble;
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem,
                        APInt(64, ~(1ULL << 63)))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(64, 0))));
    } else {
        const fltSemantics &Sem = APFloat::IEEEsingle;
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem,
                        APInt(32, ~(1U << 31)))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
        CV.push_back(ConstantFP::get(*Context, APFloat(Sem, APInt(32, 0))));
    }
    C = ConstantVector::get(CV);
    CPIdx = DAG.getConstantPool(C, TLI.getPointerTy(), 16);
    SDValue Mask2 = DAG.getLoad(VT, dl, DAG.getEntryNode(), CPIdx,
            MachinePointerInfo::getConstantPool(),
            false, false, false, 16);
    SDValue Val = DAG.getNode(Cse523ISD::FAND, dl, VT, Op0, Mask2);

    // Or the value with the sign bit.
    return DAG.getNode(Cse523ISD::FOR, dl, VT, Val, SignBit);
}

static SDValue LowerFGETSIGN(SDValue Op, SelectionDAG &DAG) {
    SDValue N0 = Op.getOperand(0);
    SDLoc dl(Op);
    MVT VT = Op.getSimpleValueType();

    // Lower ISD::FGETSIGN to (AND (Cse523ISD::FGETSIGNcse523 ...) 1).
    SDValue xFGETSIGN = DAG.getNode(Cse523ISD::FGETSIGNcse523, dl, VT, N0,
            DAG.getConstant(1, VT));
    return DAG.getNode(ISD::AND, dl, VT, xFGETSIGN, DAG.getConstant(1, VT));
}

// LowerVectorAllZeroTest - Check whether an OR'd tree is PTEST-able.
//
static SDValue LowerVectorAllZeroTest(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    assert(Op.getOpcode() == ISD::OR && "Only check OR'd tree.");

    if (!Subtarget->hasSSE41())
        return SDValue();

    if (!Op->hasOneUse())
        return SDValue();

    SDNode *N = Op.getNode();
    SDLoc DL(N);

    SmallVector<SDValue, 8> Opnds;
    DenseMap<SDValue, unsigned> VecInMap;
    EVT VT = MVT::Other;

    // Recognize a special case where a vector is casted into wide integer to
    // test all 0s.
    Opnds.push_back(N->getOperand(0));
    Opnds.push_back(N->getOperand(1));

    for (unsigned Slot = 0, e = Opnds.size(); Slot < e; ++Slot) {
        SmallVectorImpl<SDValue>::const_iterator I = Opnds.begin() + Slot;
        // BFS traverse all OR'd operands.
        if (I->getOpcode() == ISD::OR) {
            Opnds.push_back(I->getOperand(0));
            Opnds.push_back(I->getOperand(1));
            // Re-evaluate the number of nodes to be traversed.
            e += 2; // 2 more nodes (LHS and RHS) are pushed.
            continue;
        }

        // Quit if a non-EXTRACT_VECTOR_ELT
        if (I->getOpcode() != ISD::EXTRACT_VECTOR_ELT)
            return SDValue();

        // Quit if without a constant index.
        SDValue Idx = I->getOperand(1);
        if (!isa<ConstantSDNode>(Idx))
            return SDValue();

        SDValue ExtractedFromVec = I->getOperand(0);
        DenseMap<SDValue, unsigned>::iterator M = VecInMap.find(ExtractedFromVec);
        if (M == VecInMap.end()) {
            VT = ExtractedFromVec.getValueType();
            // Quit if not 128/256-bit vector.
            if (!VT.is128BitVector() && !VT.is256BitVector())
                return SDValue();
            // Quit if not the same type.
            if (VecInMap.begin() != VecInMap.end() &&
                    VT != VecInMap.begin()->first.getValueType())
                return SDValue();
            M = VecInMap.insert(std::make_pair(ExtractedFromVec, 0)).first;
        }
        M->second |= 1U << cast<ConstantSDNode>(Idx)->getZExtValue();
    }

    assert((VT.is128BitVector() || VT.is256BitVector()) &&
            "Not extracted from 128-/256-bit vector.");

    unsigned FullMask = (1U << VT.getVectorNumElements()) - 1U;
    SmallVector<SDValue, 8> VecIns;

    for (DenseMap<SDValue, unsigned>::const_iterator
            I = VecInMap.begin(), E = VecInMap.end(); I != E; ++I) {
        // Quit if not all elements are used.
        if (I->second != FullMask)
            return SDValue();
        VecIns.push_back(I->first);
    }

    EVT TestVT = VT.is128BitVector() ? MVT::v2i64 : MVT::v4i64;

    // Cast all vectors into TestVT for PTEST.
    for (unsigned i = 0, e = VecIns.size(); i < e; ++i)
        VecIns[i] = DAG.getNode(ISD::BITCAST, DL, TestVT, VecIns[i]);

    // If more than one full vectors are evaluated, OR them first before PTEST.
    for (unsigned Slot = 0, e = VecIns.size(); e - Slot > 1; Slot += 2, e += 1) {
        // Each iteration will OR 2 nodes and append the result until there is only
        // 1 node left, i.e. the final OR'd value of all vectors.
        SDValue LHS = VecIns[Slot];
        SDValue RHS = VecIns[Slot + 1];
        VecIns.push_back(DAG.getNode(ISD::OR, DL, TestVT, LHS, RHS));
    }

    return DAG.getNode(Cse523ISD::PTEST, DL, MVT::i32,
            VecIns.back(), VecIns.back());
}

/// Emit nodes that will be selected as "test Op0,Op0", or something
/// equivalent.
SDValue Cse523TargetLowering::EmitTest(SDValue Op, unsigned Cse523CC,
        SelectionDAG &DAG) const {
    SDLoc dl(Op);

    if (Op.getValueType() == MVT::i1)
        // KORTEST instruction should be selected
        return DAG.getNode(Cse523ISD::CMP, dl, MVT::i64, Op,
                DAG.getConstant(0, Op.getValueType()));

    // CF and OF aren't always set the way we want. Determine which
    // of these we need.
    bool NeedCF = false;
    bool NeedOF = false;
    switch (Cse523CC) {
        default: break;
        case Cse523::COND_A: case Cse523::COND_AE:
        case Cse523::COND_B: case Cse523::COND_BE:
                 NeedCF = true;
                 break;
        case Cse523::COND_G: case Cse523::COND_GE:
        case Cse523::COND_L: case Cse523::COND_LE:
        case Cse523::COND_O: case Cse523::COND_NO:
                 NeedOF = true;
                 break;
    }
    // See if we can use the EFLAGS value from the operand instead of
    // doing a separate TEST. TEST always sets OF and CF to 0, so unless
    // we prove that the arithmetic won't overflow, we can't use OF or CF.
    if (Op.getResNo() != 0 || NeedOF || NeedCF) {
        // Emit a CMP with 0, which is the TEST pattern.
        return DAG.getNode(Cse523ISD::CMP, dl, MVT::i64, Op,
                DAG.getConstant(0, Op.getValueType()));
    }
    unsigned Opcode = 0;
    unsigned NumOperands = 0;

    // Truncate operations may prevent the merge of the SETCC instruction
    // and the arithmetic instruction before it. Attempt to truncate the operands
    // of the arithmetic instruction and use a reduced bit-width instruction.
    bool NeedTruncation = false;
    SDValue ArithOp = Op;
    if (Op->getOpcode() == ISD::TRUNCATE && Op->hasOneUse()) {
        SDValue Arith = Op->getOperand(0);
        // Both the trunc and the arithmetic op need to have one user each.
        if (Arith->hasOneUse())
            switch (Arith.getOpcode()) {
                default: break;
                case ISD::ADD:
                case ISD::SUB:
                case ISD::AND:
                case ISD::OR:
                case ISD::XOR: {
                                   NeedTruncation = true;
                                   ArithOp = Arith;
                               }
            }
    }

    // NOTICE: In the code below we use ArithOp to hold the arithmetic operation
    // which may be the result of a CAST.  We use the variable 'Op', which is the
    // non-casted variable when we check for possible users.
    switch (ArithOp.getOpcode()) {
        case ISD::ADD:
            // Due to an isel shortcoming, be conservative if this add is likely to be
            // selected as part of a load-modify-store instruction. When the root node
            // in a match is a store, isel doesn't know how to remap non-chain non-flag
            // uses of other nodes in the match, such as the ADD in this case. This
            // leads to the ADD being left around and reselected, with the result being
            // two adds in the output.  Alas, even if none our users are stores, that
            // doesn't prove we're O.K.  Ergo, if we have any parents that aren't
            // CopyToReg or SETCC, eschew INC/DEC.  A better fix seems to require
            // climbing the DAG back to the root, and it doesn't seem to be worth the
            // effort.
            for (SDNode::use_iterator UI = Op.getNode()->use_begin(),
                    UE = Op.getNode()->use_end(); UI != UE; ++UI)
                if (UI->getOpcode() != ISD::CopyToReg &&
                        UI->getOpcode() != ISD::SETCC &&
                        UI->getOpcode() != ISD::STORE)
                    goto default_case;

            if (ConstantSDNode *C =
                    dyn_cast<ConstantSDNode>(ArithOp.getNode()->getOperand(1))) {
                // An add of one will be selected as an INC.
                if (C->getAPIntValue() == 1) {
                    Opcode = Cse523ISD::ADD;
                    NumOperands = 2;
                    break;
                }

                // An add of negative one (subtract of one) will be selected as a DEC.
                if (C->getAPIntValue().isAllOnesValue()) {
                    Opcode = Cse523ISD::SUB;
                    NumOperands = 2;
                    break;
                }
            }

            // Otherwise use a regular EFLAGS-setting add.
            Opcode = Cse523ISD::ADD;
            NumOperands = 2;
            break;
        case ISD::AND: {
                           // If the primary and result isn't used, don't bother using Cse523ISD::AND,
                           // because a TEST instruction will be better.
                           bool NonFlagUse = false;
                           for (SDNode::use_iterator UI = Op.getNode()->use_begin(),
                                   UE = Op.getNode()->use_end(); UI != UE; ++UI) {
                               SDNode *User = *UI;
                               unsigned UOpNo = UI.getOperandNo();
                               if (User->getOpcode() == ISD::TRUNCATE && User->hasOneUse()) {
                                   // Look pass truncate.
                                   UOpNo = User->use_begin().getOperandNo();
                                   User = *User->use_begin();
                               }

                               if (User->getOpcode() != ISD::BRCOND &&
                                       User->getOpcode() != ISD::SETCC &&
                                       !(User->getOpcode() == ISD::SELECT && UOpNo == 0)) {
                                   NonFlagUse = true;
                                   break;
                               }
                           }

                           if (!NonFlagUse)
                               break;
                       }
                       // FALL THROUGH
        case ISD::SUB:
        case ISD::OR:
        case ISD::XOR:
                       // Due to the ISEL shortcoming noted above, be conservative if this op is
                       // likely to be selected as part of a load-modify-store instruction.
                       for (SDNode::use_iterator UI = Op.getNode()->use_begin(),
                               UE = Op.getNode()->use_end(); UI != UE; ++UI)
                           if (UI->getOpcode() == ISD::STORE)
                               goto default_case;

                       // Otherwise use a regular EFLAGS-setting instruction.
                       switch (ArithOp.getOpcode()) {
                           default: llvm_unreachable("unexpected operator!");
                           case ISD::SUB: Opcode = Cse523ISD::SUB; break;
                           case ISD::XOR: Opcode = Cse523ISD::XOR; break;
                           case ISD::AND: Opcode = Cse523ISD::AND; break;
                           case ISD::OR: {
                                             if (!NeedTruncation && (Cse523CC == Cse523::COND_E || Cse523CC == Cse523::COND_NE)) {
                                                 SDValue EFLAGS = LowerVectorAllZeroTest(Op, Subtarget, DAG);
                                                 if (EFLAGS.getNode())
                                                     return EFLAGS;
                                             }
                                             Opcode = Cse523ISD::OR;
                                             break;
                                         }
                       }

                       NumOperands = 2;
                       break;
        case Cse523ISD::ADD:
        case Cse523ISD::SUB:
//        case Cse523ISD::INC:
//        case Cse523ISD::DEC:
        case Cse523ISD::OR:
        case Cse523ISD::XOR:
        case Cse523ISD::AND:
                       return SDValue(Op.getNode(), 1);
        default:
default_case:
                       break;
    }

    // If we found that truncation is beneficial, perform the truncation and
    // update 'Op'.
    if (NeedTruncation) {
        EVT VT = Op.getValueType();
        SDValue WideVal = Op->getOperand(0);
        EVT WideVT = WideVal.getValueType();
        unsigned ConvertedOp = 0;
        // Use a target machine opcode to prevent further DAGCombine
        // optimizations that may separate the arithmetic operations
        // from the setcc node.
        switch (WideVal.getOpcode()) {
            default: break;
            case ISD::ADD: ConvertedOp = Cse523ISD::ADD; break;
            case ISD::SUB: ConvertedOp = Cse523ISD::SUB; break;
            case ISD::AND: ConvertedOp = Cse523ISD::AND; break;
            case ISD::OR:  ConvertedOp = Cse523ISD::OR;  break;
            case ISD::XOR: ConvertedOp = Cse523ISD::XOR; break;
        }

        if (ConvertedOp) {
            const TargetLowering &TLI = DAG.getTargetLoweringInfo();
            if (TLI.isOperationLegal(WideVal.getOpcode(), WideVT)) {
                SDValue V0 = DAG.getNode(ISD::TRUNCATE, dl, VT, WideVal.getOperand(0));
                SDValue V1 = DAG.getNode(ISD::TRUNCATE, dl, VT, WideVal.getOperand(1));
                Op = DAG.getNode(ConvertedOp, dl, VT, V0, V1);
            }
        }
    }

    if (Opcode == 0)
        // Emit a CMP with 0, which is the TEST pattern.
        return DAG.getNode(Cse523ISD::CMP, dl, MVT::i64, Op,
                DAG.getConstant(0, Op.getValueType()));

    SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::i64);
    SmallVector<SDValue, 4> Ops;
    for (unsigned i = 0; i != NumOperands; ++i)
        Ops.push_back(Op.getOperand(i));

    SDValue New = DAG.getNode(Opcode, dl, VTs, &Ops[0], NumOperands);
    DAG.ReplaceAllUsesWith(Op, New);
    return SDValue(New.getNode(), 1);
}

/// Emit nodes that will be selected as "cmp Op0,Op1", or something
/// equivalent.
SDValue Cse523TargetLowering::EmitCmp(SDValue Op0, SDValue Op1, unsigned Cse523CC,
        SelectionDAG &DAG) const {
    SDLoc dl(Op0);
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op1)) {
        if (C->getAPIntValue() == 0)
            return EmitTest(Op0, Cse523CC, DAG);

        if (Op0.getValueType() == MVT::i1)
            llvm_unreachable("Unexpected comparison operation for MVT::i1 operands");
    }

    if ((Op0.getValueType() == MVT::i8 || Op0.getValueType() == MVT::i16 ||
                Op0.getValueType() == MVT::i32 || Op0.getValueType() == MVT::i64)) {
        // Do the comparison at i32 if it's smaller. This avoids subregister
        // aliasing issues. Keep the smaller reference if we're optimizing for
        // size, however, as that'll allow better folding of memory operations.
        if (Op0.getValueType() != MVT::i32 && Op0.getValueType() != MVT::i64 &&
                !DAG.getMachineFunction().getFunction()->getAttributes().hasAttribute(
                    AttributeSet::FunctionIndex, Attribute::MinSize)) {
            unsigned ExtendOp =
                isCse523CCUnsigned(Cse523CC) ? ISD::ZERO_EXTEND : ISD::SIGN_EXTEND;
            Op0 = DAG.getNode(ExtendOp, dl, MVT::i64, Op0);
            Op1 = DAG.getNode(ExtendOp, dl, MVT::i64, Op1);
        }
        // Use SUB instead of CMP to enable CSE between SUB and CMP.
        SDVTList VTs = DAG.getVTList(Op0.getValueType(), MVT::i64);
        SDValue Sub = DAG.getNode(Cse523ISD::SUB, dl, VTs,
                Op0, Op1);
        return SDValue(Sub.getNode(), 1);
    }
    return DAG.getNode(Cse523ISD::CMP, dl, MVT::i64, Op0, Op1);
}

/// Convert a comparison if required by the subtarget.
SDValue Cse523TargetLowering::ConvertCmpIfNecessary(SDValue Cmp,
        SelectionDAG &DAG) const {
    // If the subtarget does not support the FUCOMI instruction, floating-point
    // comparisons have to be converted.
    if (Subtarget->hasCMov() ||
            Cmp.getOpcode() != Cse523ISD::CMP ||
            !Cmp.getOperand(0).getValueType().isFloatingPoint() ||
            !Cmp.getOperand(1).getValueType().isFloatingPoint())
        return Cmp;
    
    llvm_unreachable("Unsupported CMP for Floating Points");
}

static bool isAllOnes(SDValue V) {
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(V);
    return C && C->isAllOnesValue();
}

/// LowerToBT - Result of 'and' is compared against zero. Turn it into a BT node
/// if it's possible.
SDValue Cse523TargetLowering::LowerToBT(SDValue And, ISD::CondCode CC,
        SDLoc dl, SelectionDAG &DAG) const {
    // No BIT Instructions supported
    return SDValue();
}

/// \brief - Turns an ISD::CondCode into a value suitable for SSE floating point
/// mask CMPs.
static int translateCse523FSETCC(ISD::CondCode SetCCOpcode, SDValue &Op0,
        SDValue &Op1) {
    llvm_unreachable("Unsupported SETCC for Floating Points");
    return 0;
}

// Lower256IntVSETCC - Break a VSETCC 256-bit integer VSETCC into two new 128
// ones, and then concatenate the result back.
static SDValue Lower256IntVSETCC(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();

    assert(VT.is256BitVector() && Op.getOpcode() == ISD::SETCC &&
            "Unsupported value type for operation");

    unsigned NumElems = VT.getVectorNumElements();
    SDLoc dl(Op);
    SDValue CC = Op.getOperand(2);

    // Extract the LHS vectors
    SDValue LHS = Op.getOperand(0);
    SDValue LHS1 = Extract128BitVector(LHS, 0, DAG, dl);
    SDValue LHS2 = Extract128BitVector(LHS, NumElems/2, DAG, dl);

    // Extract the RHS vectors
    SDValue RHS = Op.getOperand(1);
    SDValue RHS1 = Extract128BitVector(RHS, 0, DAG, dl);
    SDValue RHS2 = Extract128BitVector(RHS, NumElems/2, DAG, dl);

    // Issue the operation on the smaller types and concatenate the result back
    MVT EltVT = VT.getVectorElementType();
    MVT NewVT = MVT::getVectorVT(EltVT, NumElems/2);
    return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT,
            DAG.getNode(Op.getOpcode(), dl, NewVT, LHS1, RHS1, CC),
            DAG.getNode(Op.getOpcode(), dl, NewVT, LHS2, RHS2, CC));
}

static SDValue LowerIntVSETCC_AVX512(SDValue Op, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    SDValue Op0 = Op.getOperand(0);
    SDValue Op1 = Op.getOperand(1);
    SDValue CC = Op.getOperand(2);
    MVT VT = Op.getSimpleValueType();
    SDLoc dl(Op);

    assert(Op0.getValueType().getVectorElementType().getSizeInBits() >= 32 &&
            Op.getValueType().getScalarType() == MVT::i1 &&
            "Cannot set masked compare for this operation");

    ISD::CondCode SetCCOpcode = cast<CondCodeSDNode>(CC)->get();
    unsigned  Opc = 0;
    bool Unsigned = false;
    bool Swap = false;
    unsigned SSECC;
    switch (SetCCOpcode) {
        default: llvm_unreachable("Unexpected SETCC condition");
        case ISD::SETNE:  SSECC = 4; break;
        case ISD::SETEQ:  Opc = Cse523ISD::PCMPEQM; break;
        case ISD::SETUGT: SSECC = 6; Unsigned = true; break;
        case ISD::SETLT:  Swap = true; //fall-through
        case ISD::SETGT:  Opc = Cse523ISD::PCMPGTM; break;
        case ISD::SETULT: SSECC = 1; Unsigned = true; break;
        case ISD::SETUGE: SSECC = 5; Unsigned = true; break; //NLT
        case ISD::SETGE:  Swap = true; SSECC = 2; break; // LE + swap
        case ISD::SETULE: Unsigned = true; //fall-through
        case ISD::SETLE:  SSECC = 2; break;
    }

    if (Swap)
        std::swap(Op0, Op1);
    if (Opc)
        return DAG.getNode(Opc, dl, VT, Op0, Op1);
    Opc = Unsigned ? Cse523ISD::CMPMU: Cse523ISD::CMPM;
    return DAG.getNode(Opc, dl, VT, Op0, Op1,
            DAG.getConstant(SSECC, MVT::i8));
}

/// \brief Try to turn a VSETULT into a VSETULE by modifying its second
/// operand \p Op1.  If non-trivial (for example because it's not constant)
/// return an empty value.
static SDValue ChangeVSETULTtoVSETULE(SDValue Op1, SelectionDAG &DAG)
{
    BuildVectorSDNode *BV = dyn_cast<BuildVectorSDNode>(Op1.getNode());
    if (!BV)
        return SDValue();

    MVT VT = Op1.getSimpleValueType();
    MVT EVT = VT.getVectorElementType();
    unsigned n = VT.getVectorNumElements();
    SmallVector<SDValue, 8> ULTOp1;

    for (unsigned i = 0; i < n; ++i) {
        ConstantSDNode *Elt = dyn_cast<ConstantSDNode>(BV->getOperand(i));
        if (!Elt || Elt->isOpaque() || Elt->getValueType(0) != EVT)
            return SDValue();

        // Avoid underflow.
        APInt Val = Elt->getAPIntValue();
        if (Val == 0)
            return SDValue();

        ULTOp1.push_back(DAG.getConstant(Val - 1, EVT));
    }

    return DAG.getNode(ISD::BUILD_VECTOR, SDLoc(Op1), VT, ULTOp1.data(),
            ULTOp1.size());
}

SDValue Cse523TargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {

    MVT VT = Op.getSimpleValueType();

    if (VT.isVector()) llvm_unreachable("Unsupported Vector Instructions");

    assert((VT == MVT::i64) && "SetCC type must be 64-bit integer");

    SDValue Op0 = Op.getOperand(0);
    SDValue Op1 = Op.getOperand(1);
    SDLoc dl(Op);
    ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();

    // Look for X == 0, X == 1, X != 0, or X != 1.  We can simplify some forms of
    // these.
    if (Op1.getOpcode() == ISD::Constant &&
            (cast<ConstantSDNode>(Op1)->getZExtValue() == 1 ||
             cast<ConstantSDNode>(Op1)->isNullValue()) &&
            (CC == ISD::SETEQ || CC == ISD::SETNE)) {

        // If the input is a setcc, then reuse the input setcc or use a new one with
        // the inverted condition.
        if (Op0.getOpcode() == Cse523ISD::SETCC) {
            Cse523::CondCode CCode = (Cse523::CondCode)Op0.getConstantOperandVal(0);
            bool Invert = (CC == ISD::SETNE) ^
                cast<ConstantSDNode>(Op1)->isNullValue();
            if (!Invert)
                return Op0;

            CCode = Cse523::GetOppositeBranchCondition(CCode);
            SDValue SetCC = DAG.getNode(Cse523ISD::SETCC, dl, MVT::i64,
                    DAG.getConstant(CCode, MVT::i64),
                    Op0.getOperand(1));
            return SetCC;
        }
    }

    bool isFP = Op1.getSimpleValueType().isFloatingPoint();
    unsigned Cse523CC = TranslateCse523CC(CC, isFP, Op0, Op1, DAG);
    if (Cse523CC == Cse523::COND_INVALID)
        return SDValue();

    SDValue EFLAGS = EmitCmp(Op0, Op1, Cse523CC, DAG);
    EFLAGS = ConvertCmpIfNecessary(EFLAGS, DAG);
    SDValue SetCC = DAG.getNode(Cse523ISD::SETCC, dl, MVT::i64,
            DAG.getConstant(Cse523CC, MVT::i64), EFLAGS);
    return SetCC;
}

// isCse523LogicalCmp - Return true if opcode is a Cse523 logical comparison.
static bool isCse523LogicalCmp(SDValue Op) {
    unsigned Opc = Op.getNode()->getOpcode();
    if (Opc == Cse523ISD::CMP || Opc == Cse523ISD::COMI || Opc == Cse523ISD::UCOMI ||
            Opc == Cse523ISD::SAHF)
        return true;
    if (Op.getResNo() == 1 &&
            (Opc == Cse523ISD::ADD ||
             Opc == Cse523ISD::SUB ||
             Opc == Cse523ISD::ADC ||
//             Opc == Cse523ISD::SBB ||
             Opc == Cse523ISD::SMUL ||
             Opc == Cse523ISD::UMUL ||
//             Opc == Cse523ISD::INC ||
//             Opc == Cse523ISD::DEC ||
             Opc == Cse523ISD::OR ||
             Opc == Cse523ISD::XOR ||
             Opc == Cse523ISD::AND))
        return true;

    if (Op.getResNo() == 2 && Opc == Cse523ISD::UMUL)
        return true;

    return false;
}

static bool isZero(SDValue V) {
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(V);
    return C && C->isNullValue();
}

static bool isTruncWithZeroHighBitsInput(SDValue V, SelectionDAG &DAG) {
    if (V.getOpcode() != ISD::TRUNCATE)
        return false;

    SDValue VOp0 = V.getOperand(0);
    unsigned InBits = VOp0.getValueSizeInBits();
    unsigned Bits = V.getValueSizeInBits();
    return DAG.MaskedValueIsZero(VOp0, APInt::getHighBitsSet(InBits,InBits-Bits));
}

SDValue Cse523TargetLowering::LowerSELECT(SDValue Op, SelectionDAG &DAG) const {
    bool addTest = true;
    SDValue Cond  = Op.getOperand(0);
    SDValue Op1 = Op.getOperand(1);
    SDValue Op2 = Op.getOperand(2);
    SDLoc DL(Op);
    EVT VT = Op1.getValueType();
    SDValue CC;

    // Lower fp selects into a CMP/AND/ANDN/OR sequence when the necessary SSE ops
    // are available. Otherwise fp cmovs get lowered into a less efficient branch
    // sequence later on.
    if (Cond.getOpcode() == ISD::SETCC &&
            ((Subtarget->hasSSE2() && (VT == MVT::f32 || VT == MVT::f64)) ||
             (Subtarget->hasSSE1() && VT == MVT::f32)) &&
            VT == Cond.getOperand(0).getValueType() && Cond->hasOneUse()) {
        llvm_unreachable("FP Unsupported");
    }

    if (Cond.getOpcode() == ISD::SETCC) {
        SDValue NewCond = LowerSETCC(Cond, DAG);
        if (NewCond.getNode())
            Cond = NewCond;
    }

    // (select (x == 0), -1, y) -> (sign_bit (x - 1)) | y
    // (select (x == 0), y, -1) -> ~(sign_bit (x - 1)) | y
    // (select (x != 0), y, -1) -> (sign_bit (x - 1)) | y
    // (select (x != 0), -1, y) -> ~(sign_bit (x - 1)) | y
    if (Cond.getOpcode() == Cse523ISD::SETCC &&
            Cond.getOperand(1).getOpcode() == Cse523ISD::CMP &&
            isZero(Cond.getOperand(1).getOperand(1))) {
        SDValue Cmp = Cond.getOperand(1);

        unsigned CondCode =cast<ConstantSDNode>(Cond.getOperand(0))->getZExtValue();

        if ((isAllOnes(Op1) || isAllOnes(Op2)) &&
                (CondCode == Cse523::COND_E || CondCode == Cse523::COND_NE)) {
            SDValue Y = isAllOnes(Op2) ? Op1 : Op2;

            SDValue CmpOp0 = Cmp.getOperand(0);
            // Apply further optimizations for special cases
            // (select (x != 0), -1, 0) -> neg & sbb
            // (select (x == 0), 0, -1) -> neg & sbb
            if (ConstantSDNode *YC = dyn_cast<ConstantSDNode>(Y))
                if (YC->isNullValue() &&
                        (isAllOnes(Op1) == (CondCode == Cse523::COND_NE))) {
                    SDVTList VTs = DAG.getVTList(CmpOp0.getValueType(), MVT::i64);
                    SDValue Neg = DAG.getNode(Cse523ISD::SUB, DL, VTs,
                            DAG.getConstant(0, CmpOp0.getValueType()),
                            CmpOp0);
                    SDValue Res = DAG.getNode(Cse523ISD::SETCC_CARRY, DL, Op.getValueType(),
                            DAG.getConstant(Cse523::COND_B, MVT::i64),
                            SDValue(Neg.getNode(), 1));
                    return Res;
                }

            Cmp = DAG.getNode(Cse523ISD::CMP, DL, MVT::i64,
                    CmpOp0, DAG.getConstant(1, CmpOp0.getValueType()));
            Cmp = ConvertCmpIfNecessary(Cmp, DAG);

            SDValue Res =   // Res = 0 or -1.
                DAG.getNode(Cse523ISD::SETCC_CARRY, DL, Op.getValueType(),
                        DAG.getConstant(Cse523::COND_B, MVT::i64), Cmp);

            if (isAllOnes(Op1) != (CondCode == Cse523::COND_E))
                Res = DAG.getNOT(DL, Res, Res.getValueType());

            ConstantSDNode *N2C = dyn_cast<ConstantSDNode>(Op2);
            if (N2C == 0 || !N2C->isNullValue())
                Res = DAG.getNode(ISD::OR, DL, Res.getValueType(), Res, Y);
            return Res;
        }
    }

    // Look past (and (setcc_carry (cmp ...)), 1).
    if (Cond.getOpcode() == ISD::AND &&
            Cond.getOperand(0).getOpcode() == Cse523ISD::SETCC_CARRY) {
        ConstantSDNode *C = dyn_cast<ConstantSDNode>(Cond.getOperand(1));
        if (C && C->getAPIntValue() == 1)
            Cond = Cond.getOperand(0);
    }

    // If condition flag is set by a Cse523ISD::CMP, then use it as the condition
    // setting operand in place of the Cse523ISD::SETCC.
    unsigned CondOpcode = Cond.getOpcode();
    if (CondOpcode == Cse523ISD::SETCC ||
            CondOpcode == Cse523ISD::SETCC_CARRY) {
        CC = Cond.getOperand(0);

        SDValue Cmp = Cond.getOperand(1);
        unsigned Opc = Cmp.getOpcode();
        MVT VT = Op.getSimpleValueType();

        bool IllegalFPCMov = false;
        if (VT.isFloatingPoint() && !VT.isVector() &&
                !isScalarFPTypeInSSEReg(VT))  // FPStack?
            IllegalFPCMov = !hasFPCMov(cast<ConstantSDNode>(CC)->getSExtValue());

        if ((isCse523LogicalCmp(Cmp) && !IllegalFPCMov) ||
                Opc == Cse523ISD::BT) { // FIXME
            Cond = Cmp;
            addTest = false;
        }
    } else if (CondOpcode == ISD::USUBO || CondOpcode == ISD::SSUBO ||
            CondOpcode == ISD::UADDO || CondOpcode == ISD::SADDO ||
            ((CondOpcode == ISD::UMULO || CondOpcode == ISD::SMULO) &&
             Cond.getOperand(0).getValueType() != MVT::i64)) {
        SDValue LHS = Cond.getOperand(0);
        SDValue RHS = Cond.getOperand(1);
        unsigned Cse523Opcode;
        unsigned Cse523Cond;
        SDVTList VTs;
        switch (CondOpcode) {
            case ISD::UADDO: Cse523Opcode = Cse523ISD::ADD; Cse523Cond = Cse523::COND_B; break;
            case ISD::SADDO: Cse523Opcode = Cse523ISD::ADD; Cse523Cond = Cse523::COND_O; break;
            case ISD::USUBO: Cse523Opcode = Cse523ISD::SUB; Cse523Cond = Cse523::COND_B; break;
            case ISD::SSUBO: Cse523Opcode = Cse523ISD::SUB; Cse523Cond = Cse523::COND_O; break;
            case ISD::UMULO: Cse523Opcode = Cse523ISD::UMUL; Cse523Cond = Cse523::COND_O; break;
            case ISD::SMULO: Cse523Opcode = Cse523ISD::SMUL; Cse523Cond = Cse523::COND_O; break;
            default: llvm_unreachable("unexpected overflowing operator");
        }
        if (CondOpcode == ISD::UMULO)
            VTs = DAG.getVTList(LHS.getValueType(), LHS.getValueType(),
                    MVT::i64);
        else
            VTs = DAG.getVTList(LHS.getValueType(), MVT::i64);

        SDValue Cse523Op = DAG.getNode(Cse523Opcode, DL, VTs, LHS, RHS);

        if (CondOpcode == ISD::UMULO)
            Cond = Cse523Op.getValue(2);
        else
            Cond = Cse523Op.getValue(1);

        CC = DAG.getConstant(Cse523Cond, MVT::i64);
        addTest = false;
    }

    if (addTest) {
        // Look pass the truncate if the high bits are known zero.
        if (isTruncWithZeroHighBitsInput(Cond, DAG))
            Cond = Cond.getOperand(0);

        // We know the result of AND is compared against zero. Try to match
        // it to BT.
        if (Cond.getOpcode() == ISD::AND && Cond.hasOneUse()) {
            SDValue NewSetCC = LowerToBT(Cond, ISD::SETNE, DL, DAG);
            if (NewSetCC.getNode()) {
                CC = NewSetCC.getOperand(0);
                Cond = NewSetCC.getOperand(1);
                addTest = false;
            }
        }
    }

    if (addTest) {
        CC = DAG.getConstant(Cse523::COND_NE, MVT::i64);
        Cond = EmitTest(Cond, Cse523::COND_NE, DAG);
    }

    // a <  b ? -1 :  0 -> RES = ~setcc_carry
    // a <  b ?  0 : -1 -> RES = setcc_carry
    // a >= b ? -1 :  0 -> RES = setcc_carry
    // a >= b ?  0 : -1 -> RES = ~setcc_carry
    if (Cond.getOpcode() == Cse523ISD::SUB) {
        Cond = ConvertCmpIfNecessary(Cond, DAG);
        unsigned CondCode = cast<ConstantSDNode>(CC)->getZExtValue();

        if ((CondCode == Cse523::COND_AE || CondCode == Cse523::COND_B) &&
                (isAllOnes(Op1) || isAllOnes(Op2)) && (isZero(Op1) || isZero(Op2))) {
            SDValue Res = DAG.getNode(Cse523ISD::SETCC_CARRY, DL, Op.getValueType(),
                    DAG.getConstant(Cse523::COND_B, MVT::i64), Cond);
            if (isAllOnes(Op1) != (CondCode == Cse523::COND_B))
                return DAG.getNOT(DL, Res, Res.getValueType());
            return Res;
        }
    }


    // Cse523ISD::CMOV means set the result (which is operand 1) to the RHS if
    // condition is true.
    SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
    SDValue Ops[] = { Op2, Op1, CC, Cond };
    return DAG.getNode(Cse523ISD::CMOV, DL, VTs, Ops, array_lengthof(Ops));
}

static SDValue LowerSIGN_EXTEND_AVX512(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op->getSimpleValueType(0);
    SDValue In = Op->getOperand(0);
    MVT InVT = In.getSimpleValueType();
    SDLoc dl(Op);

    unsigned int NumElts = VT.getVectorNumElements();
    if (NumElts != 8 && NumElts != 16)
        return SDValue();

    if (VT.is512BitVector() && InVT.getVectorElementType() != MVT::i1)
        return DAG.getNode(Cse523ISD::VSEXT, dl, VT, In);

    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    assert (InVT.getVectorElementType() == MVT::i1 && "Unexpected vector type");

    MVT ExtVT = (NumElts == 8) ? MVT::v8i64 : MVT::v16i32;
    Constant *C = ConstantInt::get(*DAG.getContext(),
            APInt::getAllOnesValue(ExtVT.getScalarType().getSizeInBits()));

    SDValue CP = DAG.getConstantPool(C, TLI.getPointerTy());
    unsigned Alignment = cast<ConstantPoolSDNode>(CP)->getAlignment();
    SDValue Ld = DAG.getLoad(ExtVT.getScalarType(), dl, DAG.getEntryNode(), CP,
            MachinePointerInfo::getConstantPool(),
            false, false, false, Alignment);
    SDValue Brcst = DAG.getNode(Cse523ISD::VBROADCASTM, dl, ExtVT, In, Ld);
    if (VT.is512BitVector())
        return Brcst;
    return DAG.getNode(Cse523ISD::VTRUNC, dl, VT, Brcst);
}

static SDValue LowerSIGN_EXTEND(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    MVT VT = Op->getSimpleValueType(0);
    SDValue In = Op->getOperand(0);
    MVT InVT = In.getSimpleValueType();
    SDLoc dl(Op);

    if (VT.is512BitVector() || InVT.getVectorElementType() == MVT::i1)
        return LowerSIGN_EXTEND_AVX512(Op, DAG);

    if ((VT != MVT::v4i64 || InVT != MVT::v4i32) &&
            (VT != MVT::v8i32 || InVT != MVT::v8i16) &&
            (VT != MVT::v16i16 || InVT != MVT::v16i8))
        return SDValue();

    if (Subtarget->hasInt256())
        return DAG.getNode(Cse523ISD::VSEXT, dl, VT, In);

    // Optimize vectors in AVX mode
    // Sign extend  v8i16 to v8i32 and
    //              v4i32 to v4i64
    //
    // Divide input vector into two parts
    // for v4i32 the shuffle mask will be { 0, 1, -1, -1} {2, 3, -1, -1}
    // use vpmovsx instruction to extend v4i32 -> v2i64; v8i16 -> v4i32
    // concat the vectors to original VT

    unsigned NumElems = InVT.getVectorNumElements();
    SDValue Undef = DAG.getUNDEF(InVT);

    SmallVector<int,8> ShufMask1(NumElems, -1);
    for (unsigned i = 0; i != NumElems/2; ++i)
        ShufMask1[i] = i;

    SDValue OpLo = DAG.getVectorShuffle(InVT, dl, In, Undef, &ShufMask1[0]);

    SmallVector<int,8> ShufMask2(NumElems, -1);
    for (unsigned i = 0; i != NumElems/2; ++i)
        ShufMask2[i] = i + NumElems/2;

    SDValue OpHi = DAG.getVectorShuffle(InVT, dl, In, Undef, &ShufMask2[0]);

    MVT HalfVT = MVT::getVectorVT(VT.getScalarType(),
            VT.getVectorNumElements()/2);

    OpLo = DAG.getNode(Cse523ISD::VSEXT, dl, HalfVT, OpLo);
    OpHi = DAG.getNode(Cse523ISD::VSEXT, dl, HalfVT, OpHi);

    return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT, OpLo, OpHi);
}

// isAndOrOfSingleUseSetCCs - Return true if node is an ISD::AND or
// ISD::OR of two Cse523ISD::SETCC nodes each of which has no other use apart
// from the AND / OR.
static bool isAndOrOfSetCCs(SDValue Op, unsigned &Opc) {
    Opc = Op.getOpcode();
    if (Opc != ISD::OR && Opc != ISD::AND)
        return false;
    return (Op.getOperand(0).getOpcode() == Cse523ISD::SETCC &&
            Op.getOperand(0).hasOneUse() &&
            Op.getOperand(1).getOpcode() == Cse523ISD::SETCC &&
            Op.getOperand(1).hasOneUse());
}

// isXor1OfSetCC - Return true if node is an ISD::XOR of a Cse523ISD::SETCC and
// 1 and that the SETCC node has a single use.
static bool isXor1OfSetCC(SDValue Op) {
    if (Op.getOpcode() != ISD::XOR)
        return false;
    ConstantSDNode *N1C = dyn_cast<ConstantSDNode>(Op.getOperand(1));
    if (N1C && N1C->getAPIntValue() == 1) {
        return Op.getOperand(0).getOpcode() == Cse523ISD::SETCC &&
            Op.getOperand(0).hasOneUse();
    }
    return false;
}

SDValue Cse523TargetLowering::LowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
    bool addTest = true;
    SDValue Chain = Op.getOperand(0);
    SDValue Cond  = Op.getOperand(1);
    SDValue Dest  = Op.getOperand(2);
    SDLoc dl(Op);
    SDValue CC;
    bool Inverted = false;

    if (Cond.getOpcode() == ISD::SETCC) {
        // Check for setcc([su]{add,sub,mul}o == 0).
        if (cast<CondCodeSDNode>(Cond.getOperand(2))->get() == ISD::SETEQ &&
                isa<ConstantSDNode>(Cond.getOperand(1)) &&
                cast<ConstantSDNode>(Cond.getOperand(1))->isNullValue() &&
                Cond.getOperand(0).getResNo() == 1 &&
                (Cond.getOperand(0).getOpcode() == ISD::SADDO ||
                 Cond.getOperand(0).getOpcode() == ISD::UADDO ||
                 Cond.getOperand(0).getOpcode() == ISD::SSUBO ||
                 Cond.getOperand(0).getOpcode() == ISD::USUBO ||
                 Cond.getOperand(0).getOpcode() == ISD::SMULO ||
                 Cond.getOperand(0).getOpcode() == ISD::UMULO)) {
            Inverted = true;
            Cond = Cond.getOperand(0);
        } else {
            SDValue NewCond = LowerSETCC(Cond, DAG);
            if (NewCond.getNode())
                Cond = NewCond;
        }
    }
#if 0
    // FIXME: LowerXALUO doesn't handle these!!
    else if (Cond.getOpcode() == Cse523ISD::ADD  ||
            Cond.getOpcode() == Cse523ISD::SUB  ||
            Cond.getOpcode() == Cse523ISD::SMUL ||
            Cond.getOpcode() == Cse523ISD::UMUL)
        Cond = LowerXALUO(Cond, DAG);
#endif

    // Look pass (and (setcc_carry (cmp ...)), 1).
    if (Cond.getOpcode() == ISD::AND &&
            Cond.getOperand(0).getOpcode() == Cse523ISD::SETCC_CARRY) {
        ConstantSDNode *C = dyn_cast<ConstantSDNode>(Cond.getOperand(1));
        if (C && C->getAPIntValue() == 1)
            Cond = Cond.getOperand(0);
    }

    // If condition flag is set by a Cse523ISD::CMP, then use it as the condition
    // setting operand in place of the Cse523ISD::SETCC.
    unsigned CondOpcode = Cond.getOpcode();
    if (CondOpcode == Cse523ISD::SETCC ||
            CondOpcode == Cse523ISD::SETCC_CARRY) {
        CC = Cond.getOperand(0);

        SDValue Cmp = Cond.getOperand(1);
        unsigned Opc = Cmp.getOpcode();
        // FIXME: WHY THE SPECIAL CASING OF LogicalCmp??
        if (isCse523LogicalCmp(Cmp) || Opc == Cse523ISD::BT) {
            Cond = Cmp;
            addTest = false;
        } else {
            switch (cast<ConstantSDNode>(CC)->getZExtValue()) {
                default: break;
                case Cse523::COND_O:
                case Cse523::COND_B:
                         // These can only come from an arithmetic instruction with overflow,
                         // e.g. SADDO, UADDO.
                         Cond = Cond.getNode()->getOperand(1);
                         addTest = false;
                         break;
            }
        }
    }
    CondOpcode = Cond.getOpcode();
    if (CondOpcode == ISD::UADDO || CondOpcode == ISD::SADDO ||
            CondOpcode == ISD::USUBO || CondOpcode == ISD::SSUBO ||
            ((CondOpcode == ISD::UMULO || CondOpcode == ISD::SMULO) &&
             Cond.getOperand(0).getValueType() != MVT::i64)) {
        SDValue LHS = Cond.getOperand(0);
        SDValue RHS = Cond.getOperand(1);
        unsigned Cse523Opcode;
        unsigned Cse523Cond;
        SDVTList VTs;
        // Keep this in sync with LowerXALUO, otherwise we might create redundant
        // instructions that can't be removed afterwards (i.e. Cse523ISD::ADD and
        // Cse523ISD::INC).
        switch (CondOpcode) {
            case ISD::UADDO: Cse523Opcode = Cse523ISD::ADD; Cse523Cond = Cse523::COND_B; break;
            case ISD::SADDO:
                             if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(RHS))
                                 if (C->isOne()) {
                                     Cse523Opcode = Cse523ISD::ADD; Cse523Cond = Cse523::COND_O;
                                     break;
                                 }
                             Cse523Opcode = Cse523ISD::ADD; Cse523Cond = Cse523::COND_O; break;
            case ISD::USUBO: Cse523Opcode = Cse523ISD::SUB; Cse523Cond = Cse523::COND_B; break;
            case ISD::SSUBO:
                             if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(RHS))
                                 if (C->isOne()) {
                                     Cse523Opcode = Cse523ISD::SUB; Cse523Cond = Cse523::COND_O;
                                     break;
                                 }
                             Cse523Opcode = Cse523ISD::SUB; Cse523Cond = Cse523::COND_O; break;
            case ISD::UMULO: Cse523Opcode = Cse523ISD::UMUL; Cse523Cond = Cse523::COND_O; break;
            case ISD::SMULO: Cse523Opcode = Cse523ISD::SMUL; Cse523Cond = Cse523::COND_O; break;
            default: llvm_unreachable("unexpected overflowing operator");
        }
        if (Inverted)
            Cse523Cond = Cse523::GetOppositeBranchCondition((Cse523::CondCode)Cse523Cond);
        if (CondOpcode == ISD::UMULO)
            VTs = DAG.getVTList(LHS.getValueType(), LHS.getValueType(),
                    MVT::i64);
        else
            VTs = DAG.getVTList(LHS.getValueType(), MVT::i64);

        SDValue Cse523Op = DAG.getNode(Cse523Opcode, dl, VTs, LHS, RHS);

        if (CondOpcode == ISD::UMULO)
            Cond = Cse523Op.getValue(2);
        else
            Cond = Cse523Op.getValue(1);

        CC = DAG.getConstant(Cse523Cond, MVT::i64);
        addTest = false;
    } else {
        unsigned CondOpc;
        if (Cond.hasOneUse() && isAndOrOfSetCCs(Cond, CondOpc)) {
            SDValue Cmp = Cond.getOperand(0).getOperand(1);
            if (CondOpc == ISD::OR) {
                // Also, recognize the pattern generated by an FCMP_UNE. We can emit
                // two branches instead of an explicit OR instruction with a
                // separate test.
                if (Cmp == Cond.getOperand(1).getOperand(1) &&
                        isCse523LogicalCmp(Cmp)) {
                    Cse523::CondCode CCode =
                        (Cse523::CondCode)Cond.getOperand(0).getConstantOperandVal(0);
                    CC = DAG.getConstant(CCode, MVT::i64);

                    Chain = DAG.getNode(Cse523ISD::BRCOND, dl, Op.getValueType(),
                            Chain, Dest, CC, Cmp);
                    CC = Cond.getOperand(1).getOperand(0);
                    Cond = Cmp;
                    addTest = false;
                }
            } else { // ISD::AND
                // Also, recognize the pattern generated by an FCMP_OEQ. We can emit
                // two branches instead of an explicit AND instruction with a
                // separate test. However, we only do this if this block doesn't
                // have a fall-through edge, because this requires an explicit
                // jmp when the condition is false.
                if (Cmp == Cond.getOperand(1).getOperand(1) &&
                        isCse523LogicalCmp(Cmp) &&
                        Op.getNode()->hasOneUse()) {
                    Cse523::CondCode CCode =
                        (Cse523::CondCode)Cond.getOperand(0).getConstantOperandVal(0);
                    CCode = Cse523::GetOppositeBranchCondition(CCode);
                    CC = DAG.getConstant(CCode, MVT::i64);
                    SDNode *User = *Op.getNode()->use_begin();
                    // Look for an unconditional branch following this conditional branch.
                    // We need this because we need to reverse the successors in order
                    // to implement FCMP_OEQ.
                    if (User->getOpcode() == ISD::BR) {
                        SDValue FalseBB = User->getOperand(1);
                        SDNode *NewBR =
                            DAG.UpdateNodeOperands(User, User->getOperand(0), Dest);
                        assert(NewBR == User);
                        (void)NewBR;
                        Dest = FalseBB;

                        Chain = DAG.getNode(Cse523ISD::BRCOND, dl, Op.getValueType(),
                                Chain, Dest, CC, Cmp);
                        Cse523::CondCode CCode =
                            (Cse523::CondCode)Cond.getOperand(1).getConstantOperandVal(0);
                        CCode = Cse523::GetOppositeBranchCondition(CCode);
                        CC = DAG.getConstant(CCode, MVT::i64);
                        Cond = Cmp;
                        addTest = false;
                    }
                }
            }
        } else if (Cond.hasOneUse() && isXor1OfSetCC(Cond)) {
            // Recognize for xorb (setcc), 1 patterns. The xor inverts the condition.
            // It should be transformed during dag combiner except when the condition
            // is set by a arithmetics with overflow node.
            Cse523::CondCode CCode =
                (Cse523::CondCode)Cond.getOperand(0).getConstantOperandVal(0);
            CCode = Cse523::GetOppositeBranchCondition(CCode);
            CC = DAG.getConstant(CCode, MVT::i64);
            Cond = Cond.getOperand(0).getOperand(1);
            addTest = false;
        } else if (Cond.getOpcode() == ISD::SETCC &&
                cast<CondCodeSDNode>(Cond.getOperand(2))->get() == ISD::SETOEQ) {
            // For FCMP_OEQ, we can emit
            // two branches instead of an explicit AND instruction with a
            // separate test. However, we only do this if this block doesn't
            // have a fall-through edge, because this requires an explicit
            // jmp when the condition is false.
            if (Op.getNode()->hasOneUse()) {
                SDNode *User = *Op.getNode()->use_begin();
                // Look for an unconditional branch following this conditional branch.
                // We need this because we need to reverse the successors in order
                // to implement FCMP_OEQ.
                if (User->getOpcode() == ISD::BR) {
                    SDValue FalseBB = User->getOperand(1);
                    SDNode *NewBR =
                        DAG.UpdateNodeOperands(User, User->getOperand(0), Dest);
                    assert(NewBR == User);
                    (void)NewBR;
                    Dest = FalseBB;

                    SDValue Cmp = DAG.getNode(Cse523ISD::CMP, dl, MVT::i64,
                            Cond.getOperand(0), Cond.getOperand(1));
                    Cmp = ConvertCmpIfNecessary(Cmp, DAG);
                    CC = DAG.getConstant(Cse523::COND_NE, MVT::i64);
                    Chain = DAG.getNode(Cse523ISD::BRCOND, dl, Op.getValueType(),
                            Chain, Dest, CC, Cmp);
                    CC = DAG.getConstant(Cse523::COND_P, MVT::i64);
                    Cond = Cmp;
                    addTest = false;
                }
            }
        } else if (Cond.getOpcode() == ISD::SETCC &&
                cast<CondCodeSDNode>(Cond.getOperand(2))->get() == ISD::SETUNE) {
            // For FCMP_UNE, we can emit
            // two branches instead of an explicit AND instruction with a
            // separate test. However, we only do this if this block doesn't
            // have a fall-through edge, because this requires an explicit
            // jmp when the condition is false.
            if (Op.getNode()->hasOneUse()) {
                SDNode *User = *Op.getNode()->use_begin();
                // Look for an unconditional branch following this conditional branch.
                // We need this because we need to reverse the successors in order
                // to implement FCMP_UNE.
                if (User->getOpcode() == ISD::BR) {
                    SDValue FalseBB = User->getOperand(1);
                    SDNode *NewBR =
                        DAG.UpdateNodeOperands(User, User->getOperand(0), Dest);
                    assert(NewBR == User);
                    (void)NewBR;

                    SDValue Cmp = DAG.getNode(Cse523ISD::CMP, dl, MVT::i64,
                            Cond.getOperand(0), Cond.getOperand(1));
                    Cmp = ConvertCmpIfNecessary(Cmp, DAG);
                    CC = DAG.getConstant(Cse523::COND_NE, MVT::i64);
                    Chain = DAG.getNode(Cse523ISD::BRCOND, dl, Op.getValueType(),
                            Chain, Dest, CC, Cmp);
                    CC = DAG.getConstant(Cse523::COND_NP, MVT::i64);
                    Cond = Cmp;
                    addTest = false;
                    Dest = FalseBB;
                }
            }
        }
    }

    if (addTest) {
        // Look pass the truncate if the high bits are known zero.
        if (isTruncWithZeroHighBitsInput(Cond, DAG))
            Cond = Cond.getOperand(0);

        // We know the result of AND is compared against zero. Try to match
        // it to BT.
        if (Cond.getOpcode() == ISD::AND && Cond.hasOneUse()) {
            SDValue NewSetCC = LowerToBT(Cond, ISD::SETNE, dl, DAG);
            if (NewSetCC.getNode()) {
                CC = NewSetCC.getOperand(0);
                Cond = NewSetCC.getOperand(1);
                addTest = false;
            }
        }
    }

    if (addTest) {
        CC = DAG.getConstant(Cse523::COND_NE, MVT::i64);
        Cond = EmitTest(Cond, Cse523::COND_NE, DAG);
    }
    Cond = ConvertCmpIfNecessary(Cond, DAG);
    return DAG.getNode(Cse523ISD::BRCOND, dl, Op.getValueType(),
            Chain, Dest, CC, Cond);
}

// Lower dynamic stack allocation to _alloca call for Cygwin/Mingw targets.
// Calls to _alloca is needed to probe the stack when allocating more than 4k
// bytes in one go. Touching the stack at 4K increments is necessary to ensure
// that the guard pages used by the OS virtual memory manager are allocated in
// correct sequence.
SDValue
Cse523TargetLowering::LowerDYNAMIC_STACKALLOC(SDValue Op,
        SelectionDAG &DAG) const {
    assert((Subtarget->isTargetCygMing() || Subtarget->isTargetWindows() ||
                getTargetMachine().Options.EnableSegmentedStacks) &&
            "This should be used only on Windows targets or when segmented stacks "
            "are being used");
    assert(!Subtarget->isTargetMacho() && "Not implemented");
    SDLoc dl(Op);

    // Get the inputs.
    SDValue Chain = Op.getOperand(0);
    SDValue Size  = Op.getOperand(1);
    unsigned Align = cast<ConstantSDNode>(Op.getOperand(2))->getZExtValue();
    EVT VT = Op.getNode()->getValueType(0);

    bool Is64Bit = Subtarget->is64Bit();
    EVT SPTy = MVT::i64;

    if (getTargetMachine().Options.EnableSegmentedStacks) {
        MachineFunction &MF = DAG.getMachineFunction();
        MachineRegisterInfo &MRI = MF.getRegInfo();

        // The 64 bit implementation of segmented stacks needs to clobber both r10
        // r11. This makes it impossible to use it along with nested parameters.
        const Function *F = MF.getFunction();

        for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end();
                I != E; ++I)
            if (I->hasNestAttr())
                report_fatal_error("Cannot use segmented stacks with functions that "
                        "have nested arguments.");

        const TargetRegisterClass *AddrRegClass = getRegClassFor(MVT::i64);
        unsigned Vreg = MRI.createVirtualRegister(AddrRegClass);
        Chain = DAG.getCopyToReg(Chain, dl, Vreg, Size);
        SDValue Value = DAG.getNode(Cse523ISD::SEG_ALLOCA, dl, SPTy, Chain,
                DAG.getRegister(Vreg, SPTy));
        SDValue Ops1[2] = { Value, Chain };
        return DAG.getMergeValues(Ops1, 2, dl);
    } else {
        SDValue Flag;
        unsigned Reg = Cse523::RAX;

        Chain = DAG.getCopyToReg(Chain, dl, Reg, Size, Flag);
        Flag = Chain.getValue(1);
        SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);

        Chain = DAG.getNode(Cse523ISD::WIN_ALLOCA, dl, NodeTys, Chain, Flag);

        const Cse523RegisterInfo *RegInfo =
            static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
        unsigned SPReg = RegInfo->getStackRegister();
        SDValue SP = DAG.getCopyFromReg(Chain, dl, SPReg, SPTy);
        Chain = SP.getValue(1);

        if (Align) {
            SP = DAG.getNode(ISD::AND, dl, VT, SP.getValue(0),
                    DAG.getConstant(-(uint64_t)Align, VT));
            Chain = DAG.getCopyToReg(Chain, dl, SPReg, SP);
        }

        SDValue Ops1[2] = { SP, Chain };
        return DAG.getMergeValues(Ops1, 2, dl);
    }
}

SDValue Cse523TargetLowering::LowerVASTART(SDValue Op, SelectionDAG &DAG) const {
    MachineFunction &MF = DAG.getMachineFunction();
    Cse523MachineFunctionInfo *FuncInfo = MF.getInfo<Cse523MachineFunctionInfo>();

    const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
    SDLoc DL(Op);

    if (!Subtarget->is64Bit() || Subtarget->isTargetWin64()) {
        // vastart just stores the address of the VarArgsFrameIndex slot into the
        // memory location argument.
        SDValue FR = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                getPointerTy());
        return DAG.getStore(Op.getOperand(0), DL, FR, Op.getOperand(1),
                MachinePointerInfo(SV), false, false, 0);
    }

    // __va_list_tag:
    //   gp_offset         (0 - 6 * 8)
    //   fp_offset         (48 - 48 + 8 * 16)
    //   overflow_arg_area (point to parameters coming in memory).
    //   reg_save_area
    SmallVector<SDValue, 8> MemOps;
    SDValue FIN = Op.getOperand(1);
    // Store gp_offset
    SDValue Store = DAG.getStore(Op.getOperand(0), DL,
            DAG.getConstant(FuncInfo->getVarArgsGPOffset(),
                MVT::i64),
            FIN, MachinePointerInfo(SV), false, false, 0);
    MemOps.push_back(Store);

    // Store fp_offset
    FIN = DAG.getNode(ISD::ADD, DL, getPointerTy(),
            FIN, DAG.getIntPtrConstant(4));
    Store = DAG.getStore(Op.getOperand(0), DL,
            DAG.getConstant(FuncInfo->getVarArgsFPOffset(),
                MVT::i64),
            FIN, MachinePointerInfo(SV, 4), false, false, 0);
    MemOps.push_back(Store);

    // Store ptr to overflow_arg_area
    FIN = DAG.getNode(ISD::ADD, DL, getPointerTy(),
            FIN, DAG.getIntPtrConstant(4));
    SDValue OVFIN = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
            getPointerTy());
    Store = DAG.getStore(Op.getOperand(0), DL, OVFIN, FIN,
            MachinePointerInfo(SV, 8),
            false, false, 0);
    MemOps.push_back(Store);

    // Store ptr to reg_save_area.
    FIN = DAG.getNode(ISD::ADD, DL, getPointerTy(),
            FIN, DAG.getIntPtrConstant(8));
    SDValue RSFIN = DAG.getFrameIndex(FuncInfo->getRegSaveFrameIndex(),
            getPointerTy());
    Store = DAG.getStore(Op.getOperand(0), DL, RSFIN, FIN,
            MachinePointerInfo(SV, 16), false, false, 0);
    MemOps.push_back(Store);
    return DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
            &MemOps[0], MemOps.size());
}

SDValue Cse523TargetLowering::LowerVAARG(SDValue Op, SelectionDAG &DAG) const {
    assert(Subtarget->is64Bit() &&
            "LowerVAARG only handles 64-bit va_arg!");
    assert((Subtarget->isTargetLinux() ||
                Subtarget->isTargetDarwin()) &&
            "Unhandled target in LowerVAARG");
    assert(Op.getNode()->getNumOperands() == 4);
    SDValue Chain = Op.getOperand(0);
    SDValue SrcPtr = Op.getOperand(1);
    const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
    unsigned Align = Op.getConstantOperandVal(3);
    SDLoc dl(Op);

    EVT ArgVT = Op.getNode()->getValueType(0);
    Type *ArgTy = ArgVT.getTypeForEVT(*DAG.getContext());
    uint32_t ArgSize = getDataLayout()->getTypeAllocSize(ArgTy);
    uint8_t ArgMode;

    // Decide which area this value should be read from.
    // TODO: Implement the AMD64 ABI in its entirety. This simple
    // selection mechanism works only for the basic types.
    if (ArgVT == MVT::f80) {
        llvm_unreachable("va_arg for f80 not yet implemented");
    } else if (ArgVT.isFloatingPoint() && ArgSize <= 16 /*bytes*/) {
        ArgMode = 2;  // Argument passed in XMM register. Use fp_offset.
    } else if (ArgVT.isInteger() && ArgSize <= 32 /*bytes*/) {
        ArgMode = 1;  // Argument passed in GPR64 register(s). Use gp_offset.
    } else {
        llvm_unreachable("Unhandled argument type in LowerVAARG");
    }

    if (ArgMode == 2) {
        // Sanity Check: Make sure using fp_offset makes sense.
        assert(!getTargetMachine().Options.UseSoftFloat &&
                !(DAG.getMachineFunction()
                    .getFunction()->getAttributes()
                    .hasAttribute(AttributeSet::FunctionIndex,
                        Attribute::NoImplicitFloat)) &&
                Subtarget->hasSSE1());
    }

    // Insert VAARG_64 node into the DAG
    // VAARG_64 returns two values: Variable Argument Address, Chain
    SmallVector<SDValue, 11> InstOps;
    InstOps.push_back(Chain);
    InstOps.push_back(SrcPtr);
    InstOps.push_back(DAG.getConstant(ArgSize, MVT::i64));
    InstOps.push_back(DAG.getConstant(ArgMode, MVT::i8));
    InstOps.push_back(DAG.getConstant(Align, MVT::i64));
    SDVTList VTs = DAG.getVTList(getPointerTy(), MVT::Other);
    SDValue VAARG = DAG.getMemIntrinsicNode(Cse523ISD::VAARG_64, dl,
            VTs, &InstOps[0], InstOps.size(),
            MVT::i64,
            MachinePointerInfo(SV),
            /*Align=*/0,
            /*Volatile=*/false,
            /*ReadMem=*/true,
            /*WriteMem=*/true);
    Chain = VAARG.getValue(1);

    // Load the next argument and return it
    return DAG.getLoad(ArgVT, dl,
            Chain,
            VAARG,
            MachinePointerInfo(),
            false, false, false, 0);
}

static SDValue LowerVACOPY(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    // Cse523-64 va_list is a struct { i32, i32, i8*, i8* }.
    assert(Subtarget->is64Bit() && "This code only handles 64-bit va_copy!");
    SDValue Chain = Op.getOperand(0);
    SDValue DstPtr = Op.getOperand(1);
    SDValue SrcPtr = Op.getOperand(2);
    const Value *DstSV = cast<SrcValueSDNode>(Op.getOperand(3))->getValue();
    const Value *SrcSV = cast<SrcValueSDNode>(Op.getOperand(4))->getValue();
    SDLoc DL(Op);

    return DAG.getMemcpy(Chain, DL, DstPtr, SrcPtr,
            DAG.getIntPtrConstant(24), 8, /*isVolatile*/false,
            false,
            MachinePointerInfo(DstSV), MachinePointerInfo(SrcSV));
}

// getTargetVShiftByConstNode - Handle vector element shifts where the shift
// amount is a constant. Takes immediate version of shift as input.
static SDValue getTargetVShiftByConstNode(unsigned Opc, SDLoc dl, MVT VT,
        SDValue SrcOp, uint64_t ShiftAmt,
        SelectionDAG &DAG) {
    MVT ElementType = VT.getVectorElementType();

    // Check for ShiftAmt >= element width
    if (ShiftAmt >= ElementType.getSizeInBits()) {
        if (Opc == Cse523ISD::VSRAI)
            ShiftAmt = ElementType.getSizeInBits() - 1;
        else
            return DAG.getConstant(0, VT);
    }

    assert((Opc == Cse523ISD::VSHLI || Opc == Cse523ISD::VSRLI || Opc == Cse523ISD::VSRAI)
            && "Unknown target vector shift-by-constant node");

    // Fold this packed vector shift into a build vector if SrcOp is a
    // vector of Constants or UNDEFs, and SrcOp valuetype is the same as VT.
    if (VT == SrcOp.getSimpleValueType() &&
            ISD::isBuildVectorOfConstantSDNodes(SrcOp.getNode())) {
        SmallVector<SDValue, 8> Elts;
        unsigned NumElts = SrcOp->getNumOperands();
        ConstantSDNode *ND;

        switch(Opc) {
            default: llvm_unreachable(0);
            case Cse523ISD::VSHLI:
                     for (unsigned i=0; i!=NumElts; ++i) {
                         SDValue CurrentOp = SrcOp->getOperand(i);
                         if (CurrentOp->getOpcode() == ISD::UNDEF) {
                             Elts.push_back(CurrentOp);
                             continue;
                         }
                         ND = cast<ConstantSDNode>(CurrentOp);
                         const APInt &C = ND->getAPIntValue();
                         Elts.push_back(DAG.getConstant(C.shl(ShiftAmt), ElementType));
                     }
                     break;
            case Cse523ISD::VSRLI:
                     for (unsigned i=0; i!=NumElts; ++i) {
                         SDValue CurrentOp = SrcOp->getOperand(i);
                         if (CurrentOp->getOpcode() == ISD::UNDEF) {
                             Elts.push_back(CurrentOp);
                             continue;
                         }
                         ND = cast<ConstantSDNode>(CurrentOp);
                         const APInt &C = ND->getAPIntValue();
                         Elts.push_back(DAG.getConstant(C.lshr(ShiftAmt), ElementType));
                     }
                     break;
            case Cse523ISD::VSRAI:
                     for (unsigned i=0; i!=NumElts; ++i) {
                         SDValue CurrentOp = SrcOp->getOperand(i);
                         if (CurrentOp->getOpcode() == ISD::UNDEF) {
                             Elts.push_back(CurrentOp);
                             continue;
                         }
                         ND = cast<ConstantSDNode>(CurrentOp);
                         const APInt &C = ND->getAPIntValue();
                         Elts.push_back(DAG.getConstant(C.ashr(ShiftAmt), ElementType));
                     }
                     break;
        }

        return DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &Elts[0], NumElts);
    }

    return DAG.getNode(Opc, dl, VT, SrcOp, DAG.getConstant(ShiftAmt, MVT::i8));
}

// getTargetVShiftNode - Handle vector element shifts where the shift amount
// may or may not be a constant. Takes immediate version of shift as input.
static SDValue getTargetVShiftNode(unsigned Opc, SDLoc dl, MVT VT,
        SDValue SrcOp, SDValue ShAmt,
        SelectionDAG &DAG) {
    assert(ShAmt.getValueType() == MVT::i32 && "ShAmt is not i32");

    // Catch shift-by-constant.
    if (ConstantSDNode *CShAmt = dyn_cast<ConstantSDNode>(ShAmt))
        return getTargetVShiftByConstNode(Opc, dl, VT, SrcOp,
                CShAmt->getZExtValue(), DAG);

    // Change opcode to non-immediate version
    switch (Opc) {
        default: llvm_unreachable("Unknown target vector shift node");
        case Cse523ISD::VSHLI: Opc = Cse523ISD::VSHL; break;
        case Cse523ISD::VSRLI: Opc = Cse523ISD::VSRL; break;
        case Cse523ISD::VSRAI: Opc = Cse523ISD::VSRA; break;
    }

    // Need to build a vector containing shift amount
    // Shift amount is 32-bits, but SSE instructions read 64-bit, so fill with 0
    SDValue ShOps[4];
    ShOps[0] = ShAmt;
    ShOps[1] = DAG.getConstant(0, MVT::i32);
    ShOps[2] = ShOps[3] = DAG.getUNDEF(MVT::i32);
    ShAmt = DAG.getNode(ISD::BUILD_VECTOR, dl, MVT::v4i32, &ShOps[0], 4);

    // The return type has to be a 128-bit type with the same element
    // type as the input type.
    MVT EltVT = VT.getVectorElementType();
    EVT ShVT = MVT::getVectorVT(EltVT, 128/EltVT.getSizeInBits());

    ShAmt = DAG.getNode(ISD::BITCAST, dl, ShVT, ShAmt);
    return DAG.getNode(Opc, dl, VT, SrcOp, ShAmt);
}

static SDValue LowerINTRINSIC_WO_CHAIN(SDValue Op, SelectionDAG &DAG) {
    assert(0);
    return SDValue();    // Don't custom lower most intrinsics.
}

static SDValue getGatherNode(unsigned Opc, SDValue Op, SelectionDAG &DAG,
        SDValue Base, SDValue Index,
        SDValue ScaleOp, SDValue Chain,
        const Cse523Subtarget * Subtarget) {
    SDLoc dl(Op);
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(ScaleOp);
    assert(C && "Invalid scale type");
    SDValue Scale = DAG.getTargetConstant(C->getZExtValue(), MVT::i8);
    SDValue Src = getZeroVector(Op.getValueType(), Subtarget, DAG, dl);
    EVT MaskVT = MVT::getVectorVT(MVT::i1,
            Index.getSimpleValueType().getVectorNumElements());
    SDValue MaskInReg = DAG.getConstant(~0, MaskVT);
    SDVTList VTs = DAG.getVTList(Op.getValueType(), MaskVT, MVT::Other);
    SDValue Disp = DAG.getTargetConstant(0, MVT::i64);
    SDValue Segment = DAG.getRegister(0, MVT::i64);
    SDValue Ops[] = {Src, MaskInReg, Base, Scale, Index, Disp, Segment, Chain};
    SDNode *Res = DAG.getMachineNode(Opc, dl, VTs, Ops);
    SDValue RetOps[] = { SDValue(Res, 0), SDValue(Res, 2) };
    return DAG.getMergeValues(RetOps, array_lengthof(RetOps), dl);
}

static SDValue getMGatherNode(unsigned Opc, SDValue Op, SelectionDAG &DAG,
        SDValue Src, SDValue Mask, SDValue Base,
        SDValue Index, SDValue ScaleOp, SDValue Chain,
        const Cse523Subtarget * Subtarget) {
    SDLoc dl(Op);
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(ScaleOp);
    assert(C && "Invalid scale type");
    SDValue Scale = DAG.getTargetConstant(C->getZExtValue(), MVT::i8);
    EVT MaskVT = MVT::getVectorVT(MVT::i1,
            Index.getSimpleValueType().getVectorNumElements());
    SDValue MaskInReg = DAG.getNode(ISD::BITCAST, dl, MaskVT, Mask);
    SDVTList VTs = DAG.getVTList(Op.getValueType(), MaskVT, MVT::Other);
    SDValue Disp = DAG.getTargetConstant(0, MVT::i64);
    SDValue Segment = DAG.getRegister(0, MVT::i64);
    if (Src.getOpcode() == ISD::UNDEF)
        Src = getZeroVector(Op.getValueType(), Subtarget, DAG, dl);
    SDValue Ops[] = {Src, MaskInReg, Base, Scale, Index, Disp, Segment, Chain};
    SDNode *Res = DAG.getMachineNode(Opc, dl, VTs, Ops);
    SDValue RetOps[] = { SDValue(Res, 0), SDValue(Res, 2) };
    return DAG.getMergeValues(RetOps, array_lengthof(RetOps), dl);
}

static SDValue getScatterNode(unsigned Opc, SDValue Op, SelectionDAG &DAG,
        SDValue Src, SDValue Base, SDValue Index,
        SDValue ScaleOp, SDValue Chain) {
    SDLoc dl(Op);
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(ScaleOp);
    assert(C && "Invalid scale type");
    SDValue Scale = DAG.getTargetConstant(C->getZExtValue(), MVT::i8);
    SDValue Disp = DAG.getTargetConstant(0, MVT::i64);
    SDValue Segment = DAG.getRegister(0, MVT::i64);
    EVT MaskVT = MVT::getVectorVT(MVT::i1,
            Index.getSimpleValueType().getVectorNumElements());
    SDValue MaskInReg = DAG.getConstant(~0, MaskVT);
    SDVTList VTs = DAG.getVTList(MaskVT, MVT::Other);
    SDValue Ops[] = {Base, Scale, Index, Disp, Segment, MaskInReg, Src, Chain};
    SDNode *Res = DAG.getMachineNode(Opc, dl, VTs, Ops);
    return SDValue(Res, 1);
}

static SDValue getMScatterNode(unsigned Opc, SDValue Op, SelectionDAG &DAG,
        SDValue Src, SDValue Mask, SDValue Base,
        SDValue Index, SDValue ScaleOp, SDValue Chain) {
    SDLoc dl(Op);
    ConstantSDNode *C = dyn_cast<ConstantSDNode>(ScaleOp);
    assert(C && "Invalid scale type");
    SDValue Scale = DAG.getTargetConstant(C->getZExtValue(), MVT::i8);
    SDValue Disp = DAG.getTargetConstant(0, MVT::i64);
    SDValue Segment = DAG.getRegister(0, MVT::i64);
    EVT MaskVT = MVT::getVectorVT(MVT::i1,
            Index.getSimpleValueType().getVectorNumElements());
    SDValue MaskInReg = DAG.getNode(ISD::BITCAST, dl, MaskVT, Mask);
    SDVTList VTs = DAG.getVTList(MaskVT, MVT::Other);
    SDValue Ops[] = {Base, Scale, Index, Disp, Segment, MaskInReg, Src, Chain};
    SDNode *Res = DAG.getMachineNode(Opc, dl, VTs, Ops);
    return SDValue(Res, 1);
}

static SDValue LowerINTRINSIC_W_CHAIN(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    return SDValue();    // Don't custom lower most intrinsics.
}

SDValue Cse523TargetLowering::LowerRETURNADDR(SDValue Op,
        SelectionDAG &DAG) const {
    MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
    MFI->setReturnAddressIsTaken(true);

    if (verifyReturnAddressArgumentIsConstant(Op, DAG))
        return SDValue();

    unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
    SDLoc dl(Op);
    EVT PtrVT = getPointerTy();

    if (Depth > 0) {
        SDValue FrameAddr = LowerFRAMEADDR(Op, DAG);
        const Cse523RegisterInfo *RegInfo =
            static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
        SDValue Offset = DAG.getConstant(RegInfo->getSlotSize(), PtrVT);
        return DAG.getLoad(PtrVT, dl, DAG.getEntryNode(),
                DAG.getNode(ISD::ADD, dl, PtrVT,
                    FrameAddr, Offset),
                MachinePointerInfo(), false, false, false, 0);
    }

    // Just load the return address.
    SDValue RetAddrFI = getReturnAddressFrameIndex(DAG);
    return DAG.getLoad(PtrVT, dl, DAG.getEntryNode(),
            RetAddrFI, MachinePointerInfo(), false, false, false, 0);
}

SDValue Cse523TargetLowering::LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const {
    MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
    MFI->setFrameAddressIsTaken(true);

    EVT VT = Op.getValueType();
    SDLoc dl(Op);  // FIXME probably not meaningful
    unsigned Depth = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    unsigned FrameReg = RegInfo->getFrameRegister(DAG.getMachineFunction());
    assert((FrameReg == Cse523::RBP && VT == MVT::i64) && "Invalid Frame Register!");
    SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), dl, FrameReg, VT);
    while (Depth--)
        FrameAddr = DAG.getLoad(VT, dl, DAG.getEntryNode(), FrameAddr,
                MachinePointerInfo(),
                false, false, false, 0);
    return FrameAddr;
}

SDValue Cse523TargetLowering::LowerFRAME_TO_ARGS_OFFSET(SDValue Op,
        SelectionDAG &DAG) const {
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    return DAG.getIntPtrConstant(2 * RegInfo->getSlotSize());
}

SDValue Cse523TargetLowering::LowerEH_RETURN(SDValue Op, SelectionDAG &DAG) const {
    SDValue Chain     = Op.getOperand(0);
    SDValue Offset    = Op.getOperand(1);
    SDValue Handler   = Op.getOperand(2);
    SDLoc dl      (Op);

    EVT PtrVT = getPointerTy();
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    unsigned FrameReg = RegInfo->getFrameRegister(DAG.getMachineFunction());
    assert(FrameReg == Cse523::RBP/* && VT == MVT::i64)*/ && "Invalid Frame Register!");
    SDValue Frame = DAG.getCopyFromReg(DAG.getEntryNode(), dl, FrameReg, PtrVT);
    unsigned StoreAddrReg = Cse523::RCX;

    SDValue StoreAddr = DAG.getNode(ISD::ADD, dl, PtrVT, Frame,
            DAG.getIntPtrConstant(RegInfo->getSlotSize()));
    StoreAddr = DAG.getNode(ISD::ADD, dl, PtrVT, StoreAddr, Offset);
    Chain = DAG.getStore(Chain, dl, Handler, StoreAddr, MachinePointerInfo(),
            false, false, 0);
    Chain = DAG.getCopyToReg(Chain, dl, StoreAddrReg, StoreAddr);

    return DAG.getNode(Cse523ISD::EH_RETURN, dl, MVT::Other, Chain,
            DAG.getRegister(StoreAddrReg, PtrVT));
}

SDValue Cse523TargetLowering::lowerEH_SJLJ_SETJMP(SDValue Op,
        SelectionDAG &DAG) const {
    SDLoc DL(Op);
    return DAG.getNode(Cse523ISD::EH_SJLJ_SETJMP, DL,
            DAG.getVTList(MVT::i64, MVT::Other),
            Op.getOperand(0), Op.getOperand(1));
}

SDValue Cse523TargetLowering::lowerEH_SJLJ_LONGJMP(SDValue Op,
        SelectionDAG &DAG) const {
    SDLoc DL(Op);
    return DAG.getNode(Cse523ISD::EH_SJLJ_LONGJMP, DL, MVT::Other,
            Op.getOperand(0), Op.getOperand(1));
}

static SDValue LowerADJUST_TRAMPOLINE(SDValue Op, SelectionDAG &DAG) {
    return Op.getOperand(0);
}

SDValue Cse523TargetLowering::LowerINIT_TRAMPOLINE(SDValue Op,
        SelectionDAG &DAG) const {
    SDValue Root = Op.getOperand(0);
    SDValue Trmp = Op.getOperand(1); // trampoline
    SDValue FPtr = Op.getOperand(2); // nested function
    SDValue Nest = Op.getOperand(3); // 'nest' parameter value
    SDLoc dl (Op);

    const Value *TrmpAddr = cast<SrcValueSDNode>(Op.getOperand(4))->getValue();
    const TargetRegisterInfo* TRI = getTargetMachine().getRegisterInfo();

    if (Subtarget->is64Bit()) {
        SDValue OutChains[6];

        // Large code-model.
        const unsigned char JMP64r  = 0xFF; // 64-bit jmp through register opcode.
        const unsigned char MOV64ri = 0xB8; // Cse523::MOV64ri opcode.

        const unsigned char N86R10 = TRI->getEncodingValue(Cse523::R10) & 0x7;
        const unsigned char N86R11 = TRI->getEncodingValue(Cse523::R11) & 0x7;

        const unsigned char REX_WB = 0x40 | 0x08 | 0x01; // REX prefix

        // Load the pointer to the nested function into R11.
        unsigned OpCode = ((MOV64ri | N86R11) << 8) | REX_WB; // movabsq r11
        SDValue Addr = Trmp;
        OutChains[0] = DAG.getStore(Root, dl, DAG.getConstant(OpCode, MVT::i16),
                Addr, MachinePointerInfo(TrmpAddr),
                false, false, 0);

        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(2, MVT::i64));
        OutChains[1] = DAG.getStore(Root, dl, FPtr, Addr,
                MachinePointerInfo(TrmpAddr, 2),
                false, false, 2);

        // Load the 'nest' parameter value into R10.
        // R10 is specified in Cse523CallingConv.td
        OpCode = ((MOV64ri | N86R10) << 8) | REX_WB; // movabsq r10
        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(10, MVT::i64));
        OutChains[2] = DAG.getStore(Root, dl, DAG.getConstant(OpCode, MVT::i16),
                Addr, MachinePointerInfo(TrmpAddr, 10),
                false, false, 0);

        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(12, MVT::i64));
        OutChains[3] = DAG.getStore(Root, dl, Nest, Addr,
                MachinePointerInfo(TrmpAddr, 12),
                false, false, 2);

        // Jump to the nested function.
        OpCode = (JMP64r << 8) | REX_WB; // jmpq *...
        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(20, MVT::i64));
        OutChains[4] = DAG.getStore(Root, dl, DAG.getConstant(OpCode, MVT::i16),
                Addr, MachinePointerInfo(TrmpAddr, 20),
                false, false, 0);

        unsigned char ModRM = N86R11 | (4 << 3) | (3 << 6); // ...r11
        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(22, MVT::i64));
        OutChains[5] = DAG.getStore(Root, dl, DAG.getConstant(ModRM, MVT::i8), Addr,
                MachinePointerInfo(TrmpAddr, 22),
                false, false, 0);

        return DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains, 6);
    } else {
        const Function *Func =
            cast<Function>(cast<SrcValueSDNode>(Op.getOperand(5))->getValue());
        CallingConv::ID CC = Func->getCallingConv();
        unsigned NestReg;

        switch (CC) {
            default:
                llvm_unreachable("Unsupported calling convention");
            case CallingConv::C:
            case CallingConv::X86_StdCall: {
                                                  // Pass 'nest' parameter in ECX.
                                                  // Must be kept in sync with Cse523CallingConv.td
                                                  // TODO: Replaced
                                                  NestReg = Cse523::RCX;

                                                  // Check that ECX wasn't needed by an 'inreg' parameter.
                                                  FunctionType *FTy = Func->getFunctionType();
                                                  const AttributeSet &Attrs = Func->getAttributes();

                                                  if (!Attrs.isEmpty() && !Func->isVarArg()) {
                                                      unsigned InRegCount = 0;
                                                      unsigned Idx = 1;

                                                      for (FunctionType::param_iterator I = FTy->param_begin(),
                                                              E = FTy->param_end(); I != E; ++I, ++Idx)
                                                          if (Attrs.hasAttribute(Idx, Attribute::InReg))
                                                              // FIXME: should only count parameters that are lowered to integers.
                                                              InRegCount += (TD->getTypeSizeInBits(*I) + 31) / 32;

                                                      if (InRegCount > 2) {
                                                          report_fatal_error("Nest register in use - reduce number of inreg"
                                                                  " parameters!");
                                                      }
                                                  }
                                                  break;
                                              }
            case CallingConv::X86_FastCall:
            case CallingConv::X86_ThisCall:
            case CallingConv::Fast:
                                              // Pass 'nest' parameter in EAX.
                                              // Must be kept in sync with Cse523CallingConv.td
                                              // TODO: Replaced
                                              NestReg = Cse523::RAX;
                                              break;
        }

        SDValue OutChains[4];
        SDValue Addr, Disp;

        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(10, MVT::i64));
        Disp = DAG.getNode(ISD::SUB, dl, MVT::i64, FPtr, Addr);

        // This is storing the opcode for MOV32ri.
        const unsigned char MOV32ri = 0xB8; // Cse523::MOV32ri's opcode byte.
        const unsigned char N86Reg = TRI->getEncodingValue(NestReg) & 0x7;
        OutChains[0] = DAG.getStore(Root, dl,
                DAG.getConstant(MOV32ri|N86Reg, MVT::i8),
                Trmp, MachinePointerInfo(TrmpAddr),
                false, false, 0);

        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(1, MVT::i64));
        OutChains[1] = DAG.getStore(Root, dl, Nest, Addr,
                MachinePointerInfo(TrmpAddr, 1),
                false, false, 1);

        const unsigned char JMP = 0xE9; // jmp <32bit dst> opcode.
        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(5, MVT::i64));
        OutChains[2] = DAG.getStore(Root, dl, DAG.getConstant(JMP, MVT::i8), Addr,
                MachinePointerInfo(TrmpAddr, 5),
                false, false, 1);

        Addr = DAG.getNode(ISD::ADD, dl, MVT::i64, Trmp,
                DAG.getConstant(6, MVT::i64));
        OutChains[3] = DAG.getStore(Root, dl, Disp, Addr,
                MachinePointerInfo(TrmpAddr, 6),
                false, false, 1);

        return DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains, 4);
    }
}

SDValue Cse523TargetLowering::LowerFLT_ROUNDS_(SDValue Op,
        SelectionDAG &DAG) const {
    /*
       The rounding mode is in bits 11:10 of FPSR, and has the following
settings:
00 Round to nearest
01 Round to -inf
10 Round to +inf
11 Round to 0

FLT_ROUNDS, on the other hand, expects the following:
-1 Undefined
0 Round to 0
1 Round to nearest
2 Round to +inf
3 Round to -inf

To perform the conversion, we do:
(((((FPSR & 0x800) >> 11) | ((FPSR & 0x400) >> 9)) + 1) & 3)
*/

    MachineFunction &MF = DAG.getMachineFunction();
    const TargetMachine &TM = MF.getTarget();
    const TargetFrameLowering &TFI = *TM.getFrameLowering();
    unsigned StackAlignment = TFI.getStackAlignment();
    MVT VT = Op.getSimpleValueType();
    SDLoc DL(Op);

    // Save FP Control Word to stack slot
    int SSFI = MF.getFrameInfo()->CreateStackObject(2, StackAlignment, false);
    SDValue StackSlot = DAG.getFrameIndex(SSFI, getPointerTy());

    MachineMemOperand *MMO =
        MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(SSFI),
                MachineMemOperand::MOStore, 2, 2);

    SDValue Ops[] = { DAG.getEntryNode(), StackSlot };
    SDValue Chain = DAG.getMemIntrinsicNode(Cse523ISD::FNSTCW16m, DL,
            DAG.getVTList(MVT::Other),
            Ops, array_lengthof(Ops), MVT::i16,
            MMO);

    // Load FP Control Word from stack slot
    SDValue CWD = DAG.getLoad(MVT::i16, DL, Chain, StackSlot,
            MachinePointerInfo(), false, false, false, 0);

    // Transform as necessary
    SDValue CWD1 =
        DAG.getNode(ISD::SRL, DL, MVT::i16,
                DAG.getNode(ISD::AND, DL, MVT::i16,
                    CWD, DAG.getConstant(0x800, MVT::i16)),
                DAG.getConstant(11, MVT::i8));
    SDValue CWD2 =
        DAG.getNode(ISD::SRL, DL, MVT::i16,
                DAG.getNode(ISD::AND, DL, MVT::i16,
                    CWD, DAG.getConstant(0x400, MVT::i16)),
                DAG.getConstant(9, MVT::i8));

    SDValue RetVal =
        DAG.getNode(ISD::AND, DL, MVT::i16,
                DAG.getNode(ISD::ADD, DL, MVT::i16,
                    DAG.getNode(ISD::OR, DL, MVT::i16, CWD1, CWD2),
                    DAG.getConstant(1, MVT::i16)),
                DAG.getConstant(3, MVT::i16));

    return DAG.getNode((VT.getSizeInBits() < 16 ?
                ISD::TRUNCATE : ISD::ZERO_EXTEND), DL, VT, RetVal);
}

static SDValue LowerCTLZ(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();
    EVT OpVT = VT;
    unsigned NumBits = VT.getSizeInBits();
    SDLoc dl(Op);

    Op = Op.getOperand(0);
    if (VT == MVT::i8) {
        // Zero extend to i32 since there is not an i8 bsr.
        OpVT = MVT::i64;
        Op = DAG.getNode(ISD::ZERO_EXTEND, dl, OpVT, Op);
    }

    // Issue a bsr (scan bits in reverse) which also sets EFLAGS.
    SDVTList VTs = DAG.getVTList(OpVT, MVT::i64);
    Op = DAG.getNode(Cse523ISD::BSR, dl, VTs, Op);

    // If src is zero (i.e. bsr sets ZF), returns NumBits.
    SDValue Ops[] = {
        Op,
        DAG.getConstant(NumBits+NumBits-1, OpVT),
        DAG.getConstant(Cse523::COND_E, MVT::i8),
        Op.getValue(1)
    };
    Op = DAG.getNode(Cse523ISD::CMOV, dl, OpVT, Ops, array_lengthof(Ops));

    // Finally xor with NumBits-1.
    Op = DAG.getNode(ISD::XOR, dl, OpVT, Op, DAG.getConstant(NumBits-1, OpVT));

    if (VT == MVT::i8)
        Op = DAG.getNode(ISD::TRUNCATE, dl, MVT::i8, Op);
    return Op;
}

static SDValue LowerCTLZ_ZERO_UNDEF(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();
    EVT OpVT = VT;
    unsigned NumBits = VT.getSizeInBits();
    SDLoc dl(Op);

    Op = Op.getOperand(0);
    if (VT == MVT::i8) {
        // Zero extend to i32 since there is not an i8 bsr.
        OpVT = MVT::i64;
        Op = DAG.getNode(ISD::ZERO_EXTEND, dl, OpVT, Op);
    }

    // Issue a bsr (scan bits in reverse).
    SDVTList VTs = DAG.getVTList(OpVT, MVT::i64);
    Op = DAG.getNode(Cse523ISD::BSR, dl, VTs, Op);

    // And xor with NumBits-1.
    Op = DAG.getNode(ISD::XOR, dl, OpVT, Op, DAG.getConstant(NumBits-1, OpVT));

    if (VT == MVT::i8)
        Op = DAG.getNode(ISD::TRUNCATE, dl, MVT::i8, Op);
    return Op;
}

static SDValue LowerCTTZ(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();
    unsigned NumBits = VT.getSizeInBits();
    SDLoc dl(Op);
    Op = Op.getOperand(0);

    // Issue a bsf (scan bits forward) which also sets EFLAGS.
    SDVTList VTs = DAG.getVTList(VT, MVT::i64);
    Op = DAG.getNode(Cse523ISD::BSF, dl, VTs, Op);

    // If src is zero (i.e. bsf sets ZF), returns NumBits.
    SDValue Ops[] = {
        Op,
        DAG.getConstant(NumBits, VT),
        DAG.getConstant(Cse523::COND_E, MVT::i64),
        Op.getValue(1)
    };
    return DAG.getNode(Cse523ISD::CMOV, dl, VT, Ops, array_lengthof(Ops));
}

// Lower256IntArith - Break a 256-bit integer operation into two new 128-bit
// ones, and then concatenate the result back.
static SDValue Lower256IntArith(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();

    assert(VT.is256BitVector() && VT.isInteger() &&
            "Unsupported value type for operation");

    unsigned NumElems = VT.getVectorNumElements();
    SDLoc dl(Op);

    // Extract the LHS vectors
    SDValue LHS = Op.getOperand(0);
    SDValue LHS1 = Extract128BitVector(LHS, 0, DAG, dl);
    SDValue LHS2 = Extract128BitVector(LHS, NumElems/2, DAG, dl);

    // Extract the RHS vectors
    SDValue RHS = Op.getOperand(1);
    SDValue RHS1 = Extract128BitVector(RHS, 0, DAG, dl);
    SDValue RHS2 = Extract128BitVector(RHS, NumElems/2, DAG, dl);

    MVT EltVT = VT.getVectorElementType();
    MVT NewVT = MVT::getVectorVT(EltVT, NumElems/2);

    return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT,
            DAG.getNode(Op.getOpcode(), dl, NewVT, LHS1, RHS1),
            DAG.getNode(Op.getOpcode(), dl, NewVT, LHS2, RHS2));
}

static SDValue LowerADD(SDValue Op, SelectionDAG &DAG) {
    assert(Op.getSimpleValueType().is256BitVector() &&
            Op.getSimpleValueType().isInteger() &&
            "Only handle AVX 256-bit vector integer operation");
    return Lower256IntArith(Op, DAG);
}

static SDValue LowerSUB(SDValue Op, SelectionDAG &DAG) {
    assert(Op.getSimpleValueType().is256BitVector() &&
            Op.getSimpleValueType().isInteger() &&
            "Only handle AVX 256-bit vector integer operation");
    return Lower256IntArith(Op, DAG);
}

static SDValue LowerMUL(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    SDLoc dl(Op);
    MVT VT = Op.getSimpleValueType();

    // Decompose 256-bit ops into smaller 128-bit ops.
    if (VT.is256BitVector() && !Subtarget->hasInt256())
        return Lower256IntArith(Op, DAG);

    SDValue A = Op.getOperand(0);
    SDValue B = Op.getOperand(1);

    // Lower v4i32 mul as 2x shuffle, 2x pmuludq, 2x shuffle.
    if (VT == MVT::v4i32) {
        assert(Subtarget->hasSSE2() && !Subtarget->hasSSE41() &&
                "Should not custom lower when pmuldq is available!");

        // Extract the odd parts.
        static const int UnpackMask[] = { 1, -1, 3, -1 };
        SDValue Aodds = DAG.getVectorShuffle(VT, dl, A, A, UnpackMask);
        SDValue Bodds = DAG.getVectorShuffle(VT, dl, B, B, UnpackMask);

        // Multiply the even parts.
        SDValue Evens = DAG.getNode(Cse523ISD::PMULUDQ, dl, MVT::v2i64, A, B);
        // Now multiply odd parts.
        SDValue Odds = DAG.getNode(Cse523ISD::PMULUDQ, dl, MVT::v2i64, Aodds, Bodds);

        Evens = DAG.getNode(ISD::BITCAST, dl, VT, Evens);
        Odds = DAG.getNode(ISD::BITCAST, dl, VT, Odds);

        // Merge the two vectors back together with a shuffle. This expands into 2
        // shuffles.
        static const int ShufMask[] = { 0, 4, 2, 6 };
        return DAG.getVectorShuffle(VT, dl, Evens, Odds, ShufMask);
    }

    assert((VT == MVT::v2i64 || VT == MVT::v4i64 || VT == MVT::v8i64) &&
            "Only know how to lower V2I64/V4I64/V8I64 multiply");

    //  Ahi = psrlqi(a, 32);
    //  Bhi = psrlqi(b, 32);
    //
    //  AloBlo = pmuludq(a, b);
    //  AloBhi = pmuludq(a, Bhi);
    //  AhiBlo = pmuludq(Ahi, b);

    //  AloBhi = psllqi(AloBhi, 32);
    //  AhiBlo = psllqi(AhiBlo, 32);
    //  return AloBlo + AloBhi + AhiBlo;

    SDValue Ahi = getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl, VT, A, 32, DAG);
    SDValue Bhi = getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl, VT, B, 32, DAG);

    // Bit cast to 32-bit vectors for MULUDQ
    EVT MulVT = (VT == MVT::v2i64) ? MVT::v4i32 :
        (VT == MVT::v4i64) ? MVT::v8i32 : MVT::v16i32;
    A = DAG.getNode(ISD::BITCAST, dl, MulVT, A);
    B = DAG.getNode(ISD::BITCAST, dl, MulVT, B);
    Ahi = DAG.getNode(ISD::BITCAST, dl, MulVT, Ahi);
    Bhi = DAG.getNode(ISD::BITCAST, dl, MulVT, Bhi);

    SDValue AloBlo = DAG.getNode(Cse523ISD::PMULUDQ, dl, VT, A, B);
    SDValue AloBhi = DAG.getNode(Cse523ISD::PMULUDQ, dl, VT, A, Bhi);
    SDValue AhiBlo = DAG.getNode(Cse523ISD::PMULUDQ, dl, VT, Ahi, B);

    AloBhi = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, VT, AloBhi, 32, DAG);
    AhiBlo = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, VT, AhiBlo, 32, DAG);

    SDValue Res = DAG.getNode(ISD::ADD, dl, VT, AloBlo, AloBhi);
    return DAG.getNode(ISD::ADD, dl, VT, Res, AhiBlo);
}

static SDValue LowerSDIV(SDValue Op, SelectionDAG &DAG) {
    MVT VT = Op.getSimpleValueType();
    MVT EltTy = VT.getVectorElementType();
    unsigned NumElts = VT.getVectorNumElements();
    SDValue N0 = Op.getOperand(0);
    SDLoc dl(Op);

    // Lower sdiv X, pow2-const.
    BuildVectorSDNode *C = dyn_cast<BuildVectorSDNode>(Op.getOperand(1));
    if (!C)
        return SDValue();

    APInt SplatValue, SplatUndef;
    unsigned SplatBitSize;
    bool HasAnyUndefs;
    if (!C->isConstantSplat(SplatValue, SplatUndef, SplatBitSize,
                HasAnyUndefs) ||
            EltTy.getSizeInBits() < SplatBitSize)
        return SDValue();

    if ((SplatValue != 0) &&
            (SplatValue.isPowerOf2() || (-SplatValue).isPowerOf2())) {
        unsigned Lg2 = SplatValue.countTrailingZeros();
        // Splat the sign bit.
        SmallVector<SDValue, 16> Sz(NumElts,
                DAG.getConstant(EltTy.getSizeInBits() - 1,
                    EltTy));
        SDValue SGN = DAG.getNode(ISD::SRA, dl, VT, N0,
                DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &Sz[0],
                    NumElts));
        // Add (N0 < 0) ? abs2 - 1 : 0;
        SmallVector<SDValue, 16> Amt(NumElts,
                DAG.getConstant(EltTy.getSizeInBits() - Lg2,
                    EltTy));
        SDValue SRL = DAG.getNode(ISD::SRL, dl, VT, SGN,
                DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &Amt[0],
                    NumElts));
        SDValue ADD = DAG.getNode(ISD::ADD, dl, VT, N0, SRL);
        SmallVector<SDValue, 16> Lg2Amt(NumElts, DAG.getConstant(Lg2, EltTy));
        SDValue SRA = DAG.getNode(ISD::SRA, dl, VT, ADD,
                DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &Lg2Amt[0],
                    NumElts));

        // If we're dividing by a positive value, we're done.  Otherwise, we must
        // negate the result.
        if (SplatValue.isNonNegative())
            return SRA;

        SmallVector<SDValue, 16> V(NumElts, DAG.getConstant(0, EltTy));
        SDValue Zero = DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], NumElts);
        return DAG.getNode(ISD::SUB, dl, VT, Zero, SRA);
    }
    return SDValue();
}

static SDValue LowerScalarImmediateShift(SDValue Op, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    MVT VT = Op.getSimpleValueType();
    SDLoc dl(Op);
    SDValue R = Op.getOperand(0);
    SDValue Amt = Op.getOperand(1);

    // Optimize shl/srl/sra with constant shift amount.
    if (isSplatVector(Amt.getNode())) {
        SDValue SclrAmt = Amt->getOperand(0);
        if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(SclrAmt)) {
            uint64_t ShiftAmt = C->getZExtValue();

            if (VT == MVT::v2i64 || VT == MVT::v4i32 || VT == MVT::v8i16 ||
                    (Subtarget->hasInt256() &&
                     (VT == MVT::v4i64 || VT == MVT::v8i32 || VT == MVT::v16i16)) ||
                    (Subtarget->hasAVX512() &&
                     (VT == MVT::v8i64 || VT == MVT::v16i32))) {
                if (Op.getOpcode() == ISD::SHL)
                    return getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, VT, R, ShiftAmt,
                            DAG);
                if (Op.getOpcode() == ISD::SRL)
                    return getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl, VT, R, ShiftAmt,
                            DAG);
                if (Op.getOpcode() == ISD::SRA && VT != MVT::v2i64 && VT != MVT::v4i64)
                    return getTargetVShiftByConstNode(Cse523ISD::VSRAI, dl, VT, R, ShiftAmt,
                            DAG);
            }

            if (VT == MVT::v16i8) {
                if (Op.getOpcode() == ISD::SHL) {
                    // Make a large shift.
                    SDValue SHL = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl,
                            MVT::v8i16, R, ShiftAmt,
                            DAG);
                    SHL = DAG.getNode(ISD::BITCAST, dl, VT, SHL);
                    // Zero out the rightmost bits.
                    SmallVector<SDValue, 16> V(16,
                            DAG.getConstant(uint8_t(-1U << ShiftAmt),
                                MVT::i64));
                    return DAG.getNode(ISD::AND, dl, VT, SHL,
                            DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 16));
                }
                if (Op.getOpcode() == ISD::SRL) {
                    // Make a large shift.
                    SDValue SRL = getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl,
                            MVT::v8i16, R, ShiftAmt,
                            DAG);
                    SRL = DAG.getNode(ISD::BITCAST, dl, VT, SRL);
                    // Zero out the leftmost bits.
                    SmallVector<SDValue, 16> V(16,
                            DAG.getConstant(uint8_t(-1U) >> ShiftAmt,
                                MVT::i64));
                    return DAG.getNode(ISD::AND, dl, VT, SRL,
                            DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 16));
                }
                if (Op.getOpcode() == ISD::SRA) {
                    if (ShiftAmt == 7) {
                        // R s>> 7  ===  R s< 0
                        SDValue Zeros = getZeroVector(VT, Subtarget, DAG, dl);
                        return DAG.getNode(Cse523ISD::PCMPGT, dl, VT, Zeros, R);
                    }

                    // R s>> a === ((R u>> a) ^ m) - m
                    SDValue Res = DAG.getNode(ISD::SRL, dl, VT, R, Amt);
                    SmallVector<SDValue, 16> V(16, DAG.getConstant(128 >> ShiftAmt,
                                MVT::i64));
                    SDValue Mask = DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 16);
                    Res = DAG.getNode(ISD::XOR, dl, VT, Res, Mask);
                    Res = DAG.getNode(ISD::SUB, dl, VT, Res, Mask);
                    return Res;
                }
                llvm_unreachable("Unknown shift opcode.");
            }

            if (Subtarget->hasInt256() && VT == MVT::v32i8) {
                if (Op.getOpcode() == ISD::SHL) {
                    // Make a large shift.
                    SDValue SHL = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl,
                            MVT::v16i16, R, ShiftAmt,
                            DAG);
                    SHL = DAG.getNode(ISD::BITCAST, dl, VT, SHL);
                    // Zero out the rightmost bits.
                    SmallVector<SDValue, 32> V(32,
                            DAG.getConstant(uint8_t(-1U << ShiftAmt),
                                MVT::i64));
                    return DAG.getNode(ISD::AND, dl, VT, SHL,
                            DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 32));
                }
                if (Op.getOpcode() == ISD::SRL) {
                    // Make a large shift.
                    SDValue SRL = getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl,
                            MVT::v16i16, R, ShiftAmt,
                            DAG);
                    SRL = DAG.getNode(ISD::BITCAST, dl, VT, SRL);
                    // Zero out the leftmost bits.
                    SmallVector<SDValue, 32> V(32,
                            DAG.getConstant(uint8_t(-1U) >> ShiftAmt,
                                MVT::i64));
                    return DAG.getNode(ISD::AND, dl, VT, SRL,
                            DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 32));
                }
                if (Op.getOpcode() == ISD::SRA) {
                    if (ShiftAmt == 7) {
                        // R s>> 7  ===  R s< 0
                        SDValue Zeros = getZeroVector(VT, Subtarget, DAG, dl);
                        return DAG.getNode(Cse523ISD::PCMPGT, dl, VT, Zeros, R);
                    }

                    // R s>> a === ((R u>> a) ^ m) - m
                    SDValue Res = DAG.getNode(ISD::SRL, dl, VT, R, Amt);
                    SmallVector<SDValue, 32> V(32, DAG.getConstant(128 >> ShiftAmt,
                                MVT::i64));
                    SDValue Mask = DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &V[0], 32);
                    Res = DAG.getNode(ISD::XOR, dl, VT, Res, Mask);
                    Res = DAG.getNode(ISD::SUB, dl, VT, Res, Mask);
                    return Res;
                }
                llvm_unreachable("Unknown shift opcode.");
            }
        }
    }

    // Special case in 32-bit mode, where i64 is expanded into high and low parts.
    if (!Subtarget->is64Bit() &&
            (VT == MVT::v2i64 || (Subtarget->hasInt256() && VT == MVT::v4i64)) &&
            Amt.getOpcode() == ISD::BITCAST &&
            Amt.getOperand(0).getOpcode() == ISD::BUILD_VECTOR) {
        Amt = Amt.getOperand(0);
        unsigned Ratio = Amt.getSimpleValueType().getVectorNumElements() /
            VT.getVectorNumElements();
        unsigned RatioInLog2 = Log2_32_Ceil(Ratio);
        uint64_t ShiftAmt = 0;
        for (unsigned i = 0; i != Ratio; ++i) {
            ConstantSDNode *C = dyn_cast<ConstantSDNode>(Amt.getOperand(i));
            if (C == 0)
                return SDValue();
            // 6 == Log2(64)
            ShiftAmt |= C->getZExtValue() << (i * (1 << (6 - RatioInLog2)));
        }
        // Check remaining shift amounts.
        for (unsigned i = Ratio; i != Amt.getNumOperands(); i += Ratio) {
            uint64_t ShAmt = 0;
            for (unsigned j = 0; j != Ratio; ++j) {
                ConstantSDNode *C =
                    dyn_cast<ConstantSDNode>(Amt.getOperand(i + j));
                if (C == 0)
                    return SDValue();
                // 6 == Log2(64)
                ShAmt |= C->getZExtValue() << (j * (1 << (6 - RatioInLog2)));
            }
            if (ShAmt != ShiftAmt)
                return SDValue();
        }
        switch (Op.getOpcode()) {
            default:
                llvm_unreachable("Unknown shift opcode!");
            case ISD::SHL:
                return getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, VT, R, ShiftAmt,
                        DAG);
            case ISD::SRL:
                return getTargetVShiftByConstNode(Cse523ISD::VSRLI, dl, VT, R, ShiftAmt,
                        DAG);
            case ISD::SRA:
                return getTargetVShiftByConstNode(Cse523ISD::VSRAI, dl, VT, R, ShiftAmt,
                        DAG);
        }
    }

    return SDValue();
}

static SDValue LowerScalarVariableShift(SDValue Op, SelectionDAG &DAG,
        const Cse523Subtarget* Subtarget) {
    MVT VT = Op.getSimpleValueType();
    SDLoc dl(Op);
    SDValue R = Op.getOperand(0);
    SDValue Amt = Op.getOperand(1);

    if ((VT == MVT::v2i64 && Op.getOpcode() != ISD::SRA) ||
            VT == MVT::v4i32 || VT == MVT::v8i16 ||
            (Subtarget->hasInt256() &&
             ((VT == MVT::v4i64 && Op.getOpcode() != ISD::SRA) ||
              VT == MVT::v8i32 || VT == MVT::v16i16)) ||
            (Subtarget->hasAVX512() && (VT == MVT::v8i64 || VT == MVT::v16i32))) {
        SDValue BaseShAmt;
        EVT EltVT = VT.getVectorElementType();

        if (Amt.getOpcode() == ISD::BUILD_VECTOR) {
            unsigned NumElts = VT.getVectorNumElements();
            unsigned i, j;
            for (i = 0; i != NumElts; ++i) {
                if (Amt.getOperand(i).getOpcode() == ISD::UNDEF)
                    continue;
                break;
            }
            for (j = i; j != NumElts; ++j) {
                SDValue Arg = Amt.getOperand(j);
                if (Arg.getOpcode() == ISD::UNDEF) continue;
                if (Arg != Amt.getOperand(i))
                    break;
            }
            if (i != NumElts && j == NumElts)
                BaseShAmt = Amt.getOperand(i);
        } else {
            if (Amt.getOpcode() == ISD::EXTRACT_SUBVECTOR)
                Amt = Amt.getOperand(0);
            if (Amt.getOpcode() == ISD::VECTOR_SHUFFLE &&
                    cast<ShuffleVectorSDNode>(Amt)->isSplat()) {
                SDValue InVec = Amt.getOperand(0);
                if (InVec.getOpcode() == ISD::BUILD_VECTOR) {
                    unsigned NumElts = InVec.getValueType().getVectorNumElements();
                    unsigned i = 0;
                    for (; i != NumElts; ++i) {
                        SDValue Arg = InVec.getOperand(i);
                        if (Arg.getOpcode() == ISD::UNDEF) continue;
                        BaseShAmt = Arg;
                        break;
                    }
                } else if (InVec.getOpcode() == ISD::INSERT_VECTOR_ELT) {
                    if (ConstantSDNode *C =
                            dyn_cast<ConstantSDNode>(InVec.getOperand(2))) {
                        unsigned SplatIdx =
                            cast<ShuffleVectorSDNode>(Amt)->getSplatIndex();
                        if (C->getZExtValue() == SplatIdx)
                            BaseShAmt = InVec.getOperand(1);
                    }
                }
                if (BaseShAmt.getNode() == 0)
                    BaseShAmt = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, EltVT, Amt,
                            DAG.getIntPtrConstant(0));
            }
        }

        if (BaseShAmt.getNode()) {
            if (EltVT.bitsGT(MVT::i32))
                BaseShAmt = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, BaseShAmt);
            else if (EltVT.bitsLT(MVT::i32))
                BaseShAmt = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::i32, BaseShAmt);

            switch (Op.getOpcode()) {
                default:
                    llvm_unreachable("Unknown shift opcode!");
                case ISD::SHL:
                    switch (VT.SimpleTy) {
                        default: return SDValue();
                        case MVT::v2i64:
                        case MVT::v4i32:
                        case MVT::v8i16:
                        case MVT::v4i64:
                        case MVT::v8i32:
                        case MVT::v16i16:
                        case MVT::v16i32:
                        case MVT::v8i64:
                                 return getTargetVShiftNode(Cse523ISD::VSHLI, dl, VT, R, BaseShAmt, DAG);
                    }
                case ISD::SRA:
                    switch (VT.SimpleTy) {
                        default: return SDValue();
                        case MVT::v4i32:
                        case MVT::v8i16:
                        case MVT::v8i32:
                        case MVT::v16i16:
                        case MVT::v16i32:
                        case MVT::v8i64:
                                 return getTargetVShiftNode(Cse523ISD::VSRAI, dl, VT, R, BaseShAmt, DAG);
                    }
                case ISD::SRL:
                    switch (VT.SimpleTy) {
                        default: return SDValue();
                        case MVT::v2i64:
                        case MVT::v4i32:
                        case MVT::v8i16:
                        case MVT::v4i64:
                        case MVT::v8i32:
                        case MVT::v16i16:
                        case MVT::v16i32:
                        case MVT::v8i64:
                                 return getTargetVShiftNode(Cse523ISD::VSRLI, dl, VT, R, BaseShAmt, DAG);
                    }
            }
        }
    }

    // Special case in 32-bit mode, where i64 is expanded into high and low parts.
    if (!Subtarget->is64Bit() &&
            (VT == MVT::v2i64 || (Subtarget->hasInt256() && VT == MVT::v4i64) ||
             (Subtarget->hasAVX512() && VT == MVT::v8i64)) &&
            Amt.getOpcode() == ISD::BITCAST &&
            Amt.getOperand(0).getOpcode() == ISD::BUILD_VECTOR) {
        Amt = Amt.getOperand(0);
        unsigned Ratio = Amt.getSimpleValueType().getVectorNumElements() /
            VT.getVectorNumElements();
        std::vector<SDValue> Vals(Ratio);
        for (unsigned i = 0; i != Ratio; ++i)
            Vals[i] = Amt.getOperand(i);
        for (unsigned i = Ratio; i != Amt.getNumOperands(); i += Ratio) {
            for (unsigned j = 0; j != Ratio; ++j)
                if (Vals[j] != Amt.getOperand(i + j))
                    return SDValue();
        }
        switch (Op.getOpcode()) {
            default:
                llvm_unreachable("Unknown shift opcode!");
            case ISD::SHL:
                return DAG.getNode(Cse523ISD::VSHL, dl, VT, R, Op.getOperand(1));
            case ISD::SRL:
                return DAG.getNode(Cse523ISD::VSRL, dl, VT, R, Op.getOperand(1));
            case ISD::SRA:
                return DAG.getNode(Cse523ISD::VSRA, dl, VT, R, Op.getOperand(1));
        }
    }

    return SDValue();
}

static SDValue LowerShift(SDValue Op, const Cse523Subtarget* Subtarget,
        SelectionDAG &DAG) {

    MVT VT = Op.getSimpleValueType();
    SDLoc dl(Op);
    SDValue R = Op.getOperand(0);
    SDValue Amt = Op.getOperand(1);
    SDValue V;

    if (!Subtarget->hasSSE2())
        return SDValue();

    V = LowerScalarImmediateShift(Op, DAG, Subtarget);
    if (V.getNode())
        return V;

    V = LowerScalarVariableShift(Op, DAG, Subtarget);
    if (V.getNode())
        return V;

    if (Subtarget->hasAVX512() && (VT == MVT::v16i32 || VT == MVT::v8i64))
        return Op;
    // AVX2 has VPSLLV/VPSRAV/VPSRLV.
    if (Subtarget->hasInt256()) {
        if (Op.getOpcode() == ISD::SRL &&
                (VT == MVT::v2i64 || VT == MVT::v4i32 ||
                 VT == MVT::v4i64 || VT == MVT::v8i32))
            return Op;
        if (Op.getOpcode() == ISD::SHL &&
                (VT == MVT::v2i64 || VT == MVT::v4i32 ||
                 VT == MVT::v4i64 || VT == MVT::v8i32))
            return Op;
        if (Op.getOpcode() == ISD::SRA && (VT == MVT::v4i32 || VT == MVT::v8i32))
            return Op;
    }

    // If possible, lower this packed shift into a vector multiply instead of
    // expanding it into a sequence of scalar shifts.
    // Do this only if the vector shift count is a constant build_vector.
    if (Op.getOpcode() == ISD::SHL && 
            (VT == MVT::v8i16 || VT == MVT::v4i32 ||
             (Subtarget->hasInt256() && VT == MVT::v16i16)) &&
            ISD::isBuildVectorOfConstantSDNodes(Amt.getNode())) {
        SmallVector<SDValue, 8> Elts;
        EVT SVT = VT.getScalarType();
        unsigned SVTBits = SVT.getSizeInBits();
        const APInt &One = APInt(SVTBits, 1);
        unsigned NumElems = VT.getVectorNumElements();

        for (unsigned i=0; i !=NumElems; ++i) {
            SDValue Op = Amt->getOperand(i);
            if (Op->getOpcode() == ISD::UNDEF) {
                Elts.push_back(Op);
                continue;
            }

            ConstantSDNode *ND = cast<ConstantSDNode>(Op);
            const APInt &C = APInt(SVTBits, ND->getAPIntValue().getZExtValue());
            uint64_t ShAmt = C.getZExtValue();
            if (ShAmt >= SVTBits) {
                Elts.push_back(DAG.getUNDEF(SVT));
                continue;
            }
            Elts.push_back(DAG.getConstant(One.shl(ShAmt), SVT));
        }
        SDValue BV = DAG.getNode(ISD::BUILD_VECTOR, dl, VT, &Elts[0], NumElems);
        return DAG.getNode(ISD::MUL, dl, VT, R, BV);
    }

    // Lower SHL with variable shift amount.
    if (VT == MVT::v4i32 && Op->getOpcode() == ISD::SHL) {
        Op = DAG.getNode(ISD::SHL, dl, VT, Amt, DAG.getConstant(23, VT));

        Op = DAG.getNode(ISD::ADD, dl, VT, Op, DAG.getConstant(0x3f800000U, VT));
        Op = DAG.getNode(ISD::BITCAST, dl, MVT::v4f32, Op);
        Op = DAG.getNode(ISD::FP_TO_SINT, dl, VT, Op);
        return DAG.getNode(ISD::MUL, dl, VT, Op, R);
    }

    if (VT == MVT::v16i8 && Op->getOpcode() == ISD::SHL) {
        assert(Subtarget->hasSSE2() && "Need SSE2 for pslli/pcmpeq.");

        // a = a << 5;
        Op = DAG.getNode(ISD::SHL, dl, VT, Amt, DAG.getConstant(5, VT));
        Op = DAG.getNode(ISD::BITCAST, dl, VT, Op);

        // Turn 'a' into a mask suitable for VSELECT
        SDValue VSelM = DAG.getConstant(0x80, VT);
        SDValue OpVSel = DAG.getNode(ISD::AND, dl, VT, VSelM, Op);
        OpVSel = DAG.getNode(Cse523ISD::PCMPEQ, dl, VT, OpVSel, VSelM);

        SDValue CM1 = DAG.getConstant(0x0f, VT);
        SDValue CM2 = DAG.getConstant(0x3f, VT);

        // r = VSELECT(r, psllw(r & (char16)15, 4), a);
        SDValue M = DAG.getNode(ISD::AND, dl, VT, R, CM1);
        M = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, MVT::v8i16, M, 4, DAG);
        M = DAG.getNode(ISD::BITCAST, dl, VT, M);
        R = DAG.getNode(ISD::VSELECT, dl, VT, OpVSel, M, R);

        // a += a
        Op = DAG.getNode(ISD::ADD, dl, VT, Op, Op);
        OpVSel = DAG.getNode(ISD::AND, dl, VT, VSelM, Op);
        OpVSel = DAG.getNode(Cse523ISD::PCMPEQ, dl, VT, OpVSel, VSelM);

        // r = VSELECT(r, psllw(r & (char16)63, 2), a);
        M = DAG.getNode(ISD::AND, dl, VT, R, CM2);
        M = getTargetVShiftByConstNode(Cse523ISD::VSHLI, dl, MVT::v8i16, M, 2, DAG);
        M = DAG.getNode(ISD::BITCAST, dl, VT, M);
        R = DAG.getNode(ISD::VSELECT, dl, VT, OpVSel, M, R);

        // a += a
        Op = DAG.getNode(ISD::ADD, dl, VT, Op, Op);
        OpVSel = DAG.getNode(ISD::AND, dl, VT, VSelM, Op);
        OpVSel = DAG.getNode(Cse523ISD::PCMPEQ, dl, VT, OpVSel, VSelM);

        // return VSELECT(r, r+r, a);
        R = DAG.getNode(ISD::VSELECT, dl, VT, OpVSel,
                DAG.getNode(ISD::ADD, dl, VT, R, R), R);
        return R;
    }

    // It's worth extending once and using the v8i32 shifts for 16-bit types, but
    // the extra overheads to get from v16i8 to v8i32 make the existing SSE
    // solution better.
    if (Subtarget->hasInt256() && VT == MVT::v8i16) {
        MVT NewVT = VT == MVT::v8i16 ? MVT::v8i32 : MVT::v16i16;
        unsigned ExtOpc =
            Op.getOpcode() == ISD::SRA ? ISD::SIGN_EXTEND : ISD::ZERO_EXTEND;
        R = DAG.getNode(ExtOpc, dl, NewVT, R);
        Amt = DAG.getNode(ISD::ANY_EXTEND, dl, NewVT, Amt);
        return DAG.getNode(ISD::TRUNCATE, dl, VT,
                DAG.getNode(Op.getOpcode(), dl, NewVT, R, Amt));
    }

    // Decompose 256-bit shifts into smaller 128-bit shifts.
    if (VT.is256BitVector()) {
        unsigned NumElems = VT.getVectorNumElements();
        MVT EltVT = VT.getVectorElementType();
        EVT NewVT = MVT::getVectorVT(EltVT, NumElems/2);

        // Extract the two vectors
        SDValue V1 = Extract128BitVector(R, 0, DAG, dl);
        SDValue V2 = Extract128BitVector(R, NumElems/2, DAG, dl);

        // Recreate the shift amount vectors
        SDValue Amt1, Amt2;
        if (Amt.getOpcode() == ISD::BUILD_VECTOR) {
            // Constant shift amount
            SmallVector<SDValue, 4> Amt1Csts;
            SmallVector<SDValue, 4> Amt2Csts;
            for (unsigned i = 0; i != NumElems/2; ++i)
                Amt1Csts.push_back(Amt->getOperand(i));
            for (unsigned i = NumElems/2; i != NumElems; ++i)
                Amt2Csts.push_back(Amt->getOperand(i));

            Amt1 = DAG.getNode(ISD::BUILD_VECTOR, dl, NewVT,
                    &Amt1Csts[0], NumElems/2);
            Amt2 = DAG.getNode(ISD::BUILD_VECTOR, dl, NewVT,
                    &Amt2Csts[0], NumElems/2);
        } else {
            // Variable shift amount
            Amt1 = Extract128BitVector(Amt, 0, DAG, dl);
            Amt2 = Extract128BitVector(Amt, NumElems/2, DAG, dl);
        }

        // Issue new vector shifts for the smaller types
        V1 = DAG.getNode(Op.getOpcode(), dl, NewVT, V1, Amt1);
        V2 = DAG.getNode(Op.getOpcode(), dl, NewVT, V2, Amt2);

        // Concatenate the result back
        return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT, V1, V2);
    }

    return SDValue();
}

static SDValue LowerXALUO(SDValue Op, SelectionDAG &DAG) {
    // Lower the "add/sub/mul with overflow" instruction into a regular ins plus
    // a "setcc" instruction that checks the overflow flag. The "brcond" lowering
    // looks for this combo and may remove the "setcc" instruction if the "setcc"
    // has only one use.
    SDNode *N = Op.getNode();
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);
    unsigned BaseOp = 0;
    unsigned Cond = 0;
    SDLoc DL(Op);
    switch (Op.getOpcode()) {
        default: llvm_unreachable("Unknown ovf instruction!");
        case ISD::SADDO:
                 // A subtract of one will be selected as a INC. Note that INC doesn't
                 // set CF, so we can't do this for UADDO.
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(RHS))
                     if (C->isOne()) {
                         BaseOp = Cse523ISD::ADD;
                         Cond = Cse523::COND_O;
                         break;
                     }
                 BaseOp = Cse523ISD::ADD;
                 Cond = Cse523::COND_O;
                 break;
        case ISD::UADDO:
                 BaseOp = Cse523ISD::ADD;
                 Cond = Cse523::COND_B;
                 break;
        case ISD::SSUBO:
                 // A subtract of one will be selected as a DEC. Note that DEC doesn't
                 // set CF, so we can't do this for USUBO.
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(RHS))
                     if (C->isOne()) {
                         BaseOp = Cse523ISD::SUB;
                         Cond = Cse523::COND_O;
                         break;
                     }
                 BaseOp = Cse523ISD::SUB;
                 Cond = Cse523::COND_O;
                 break;
        case ISD::USUBO:
                 BaseOp = Cse523ISD::SUB;
                 Cond = Cse523::COND_B;
                 break;
        case ISD::SMULO:
                 BaseOp = Cse523ISD::SMUL;
                 Cond = Cse523::COND_O;
                 break;
        case ISD::UMULO: { // i64, i8 = umulo lhs, rhs --> i64, i64, i32 umul lhs,rhs
                             SDVTList VTs = DAG.getVTList(N->getValueType(0), N->getValueType(0),
                                     MVT::i64);
                             SDValue Sum = DAG.getNode(Cse523ISD::UMUL, DL, VTs, LHS, RHS);

                             SDValue SetCC =
                                 DAG.getNode(Cse523ISD::SETCC, DL, MVT::i64,
                                         DAG.getConstant(Cse523::COND_O, MVT::i64),
                                         SDValue(Sum.getNode(), 2));

                             return DAG.getNode(ISD::MERGE_VALUES, DL, N->getVTList(), Sum, SetCC);
                         }
    }

    // Also sets EFLAGS.
    SDVTList VTs = DAG.getVTList(N->getValueType(0), MVT::i64);
    SDValue Sum = DAG.getNode(BaseOp, DL, VTs, LHS, RHS);

    SDValue SetCC =
        DAG.getNode(Cse523ISD::SETCC, DL, N->getValueType(1),
                DAG.getConstant(Cond, MVT::i64),
                SDValue(Sum.getNode(), 1));

    return DAG.getNode(ISD::MERGE_VALUES, DL, N->getVTList(), Sum, SetCC);
}

SDValue Cse523TargetLowering::LowerSIGN_EXTEND_INREG(SDValue Op,
        SelectionDAG &DAG) const {
    SDLoc dl(Op);
    EVT ExtraVT = cast<VTSDNode>(Op.getOperand(1))->getVT();
    MVT VT = Op.getSimpleValueType();

    if (!Subtarget->hasSSE2() || !VT.isVector())
        return SDValue();

    unsigned BitsDiff = VT.getScalarType().getSizeInBits() -
        ExtraVT.getScalarType().getSizeInBits();

    switch (VT.SimpleTy) {
        default: return SDValue();
        case MVT::v8i32:
        case MVT::v16i16:
                 if (!Subtarget->hasFp256())
                     return SDValue();
                 if (!Subtarget->hasInt256()) {
                     // needs to be split
                     unsigned NumElems = VT.getVectorNumElements();

                     // Extract the LHS vectors
                     SDValue LHS = Op.getOperand(0);
                     SDValue LHS1 = Extract128BitVector(LHS, 0, DAG, dl);
                     SDValue LHS2 = Extract128BitVector(LHS, NumElems/2, DAG, dl);

                     MVT EltVT = VT.getVectorElementType();
                     EVT NewVT = MVT::getVectorVT(EltVT, NumElems/2);

                     EVT ExtraEltVT = ExtraVT.getVectorElementType();
                     unsigned ExtraNumElems = ExtraVT.getVectorNumElements();
                     ExtraVT = EVT::getVectorVT(*DAG.getContext(), ExtraEltVT,
                             ExtraNumElems/2);
                     SDValue Extra = DAG.getValueType(ExtraVT);

                     LHS1 = DAG.getNode(Op.getOpcode(), dl, NewVT, LHS1, Extra);
                     LHS2 = DAG.getNode(Op.getOpcode(), dl, NewVT, LHS2, Extra);

                     return DAG.getNode(ISD::CONCAT_VECTORS, dl, VT, LHS1, LHS2);
                 }
                 // fall through
        case MVT::v4i32:
        case MVT::v8i16: 
                {
                    llvm_unreachable("Unsupported Vector Instructions");
                }
    }
}

static SDValue LowerATOMIC_FENCE(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    SDLoc dl(Op);
    AtomicOrdering FenceOrdering = static_cast<AtomicOrdering>(
            cast<ConstantSDNode>(Op.getOperand(1))->getZExtValue());
    SynchronizationScope FenceScope = static_cast<SynchronizationScope>(
            cast<ConstantSDNode>(Op.getOperand(2))->getZExtValue());

    // The only fence that needs an instruction is a sequentially-consistent
    // cross-thread fence.
    if (FenceOrdering == SequentiallyConsistent && FenceScope == CrossThread) {
        // Use mfence if we have SSE2 or we're on cse523-64 (even if we asked for
        // no-sse2). There isn't any reason to disable it if the target processor
        // supports it.
        return DAG.getNode(Cse523ISD::MFENCE, dl, MVT::Other, Op.getOperand(0));
    }

    // MEMBARRIER is a compiler barrier; it codegens to a no-op.
    return DAG.getNode(Cse523ISD::MEMBARRIER, dl, MVT::Other, Op.getOperand(0));
}

static SDValue LowerCMP_SWAP(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    MVT T = Op.getSimpleValueType();
    SDLoc DL(Op);
    unsigned Reg = 0;
    unsigned size = 0;
    switch(T.SimpleTy) {
        default: llvm_unreachable("Invalid value type!");
        case MVT::i64:
                       assert(Subtarget->is64Bit() && "Node not type legal!");
                       Reg = Cse523::RAX; size = 8;
                       break;
    }
    SDValue cpIn = DAG.getCopyToReg(Op.getOperand(0), DL, Reg,
            Op.getOperand(2), SDValue());
    SDValue Ops[] = { cpIn.getValue(0),
        Op.getOperand(1),
        Op.getOperand(3),
        DAG.getTargetConstant(size, MVT::i64),
        cpIn.getValue(1) };
    SDVTList Tys = DAG.getVTList(MVT::Other, MVT::Glue);
    MachineMemOperand *MMO = cast<AtomicSDNode>(Op)->getMemOperand();
    SDValue Result = DAG.getMemIntrinsicNode(Cse523ISD::LCMPXCHG_DAG, DL, Tys,
            Ops, array_lengthof(Ops), T, MMO);
    SDValue cpOut =
        DAG.getCopyFromReg(Result.getValue(0), DL, Reg, T, Result.getValue(1));
    return cpOut;
}

static SDValue LowerREADCYCLECOUNTER(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    assert(Subtarget->is64Bit() && "Result not type legalized?");
    SDVTList Tys = DAG.getVTList(MVT::Other, MVT::Glue);
    SDValue TheChain = Op.getOperand(0);
    SDLoc dl(Op);
    SDValue rd = DAG.getNode(Cse523ISD::RDTSC_DAG, dl, Tys, &TheChain, 1);
    SDValue rax = DAG.getCopyFromReg(rd, dl, Cse523::RAX, MVT::i64, rd.getValue(1));
    SDValue rdx = DAG.getCopyFromReg(rax.getValue(1), dl, Cse523::RDX, MVT::i64,
            rax.getValue(2));
    SDValue Tmp = DAG.getNode(ISD::SHL, dl, MVT::i64, rdx,
            DAG.getConstant(32, MVT::i64));
    SDValue Ops[] = {
        DAG.getNode(ISD::OR, dl, MVT::i64, rax, Tmp),
        rdx.getValue(1)
    };
    return DAG.getMergeValues(Ops, array_lengthof(Ops), dl);
}

static SDValue LowerBITCAST(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    MVT SrcVT = Op.getOperand(0).getSimpleValueType();
    MVT DstVT = Op.getSimpleValueType();
    assert(Subtarget->is64Bit() && !Subtarget->hasSSE2() &&
            Subtarget->hasMMX() && "Unexpected custom BITCAST");
    assert((DstVT == MVT::i64 ||
                (DstVT.isVector() && DstVT.getSizeInBits()==64)) &&
            "Unexpected custom BITCAST");
    // i64 <=> MMX conversions are Legal.
    if (SrcVT==MVT::i64 && DstVT.isVector())
        return Op;
    if (DstVT==MVT::i64 && SrcVT.isVector())
        return Op;
    // MMX <=> MMX conversions are Legal.
    if (SrcVT.isVector() && DstVT.isVector())
        return Op;
    // All other conversions need to be expanded.
    return SDValue();
}

static SDValue LowerLOAD_SUB(SDValue Op, SelectionDAG &DAG) {
    SDNode *Node = Op.getNode();
    SDLoc dl(Node);
    EVT T = Node->getValueType(0);
    SDValue negOp = DAG.getNode(ISD::SUB, dl, T,
            DAG.getConstant(0, T), Node->getOperand(2));
    return DAG.getAtomic(ISD::ATOMIC_LOAD_ADD, dl,
            cast<AtomicSDNode>(Node)->getMemoryVT(),
            Node->getOperand(0),
            Node->getOperand(1), negOp,
            cast<AtomicSDNode>(Node)->getSrcValue(),
            cast<AtomicSDNode>(Node)->getAlignment(),
            cast<AtomicSDNode>(Node)->getOrdering(),
            cast<AtomicSDNode>(Node)->getSynchScope());
}

static SDValue LowerATOMIC_STORE(SDValue Op, SelectionDAG &DAG) {
    SDNode *Node = Op.getNode();
    SDLoc dl(Node);
    EVT VT = cast<AtomicSDNode>(Node)->getMemoryVT();

    // Convert seq_cst store -> xchg
    // Convert wide store -> swap (-> cmpxchg8b/cmpxchg16b)
    // FIXME: On 32-bit, store -> fist or movq would be more efficient
    //        (The only way to get a 16-byte store is cmpxchg16b)
    // FIXME: 16-byte ATOMIC_SWAP isn't actually hooked up at the moment.
    if (cast<AtomicSDNode>(Node)->getOrdering() == SequentiallyConsistent ||
            !DAG.getTargetLoweringInfo().isTypeLegal(VT)) {
        SDValue Swap = DAG.getAtomic(ISD::ATOMIC_SWAP, dl,
                cast<AtomicSDNode>(Node)->getMemoryVT(),
                Node->getOperand(0),
                Node->getOperand(1), Node->getOperand(2),
                cast<AtomicSDNode>(Node)->getMemOperand(),
                cast<AtomicSDNode>(Node)->getOrdering(),
                cast<AtomicSDNode>(Node)->getSynchScope());
        return Swap.getValue(1);
    }
    // Other atomic stores have a simple pattern.
    return Op;
}

static SDValue LowerADDC_ADDE_SUBC_SUBE(SDValue Op, SelectionDAG &DAG) {
    EVT VT = Op.getNode()->getSimpleValueType(0);

    // Let legalize expand this if it isn't a legal type yet.
    if (!DAG.getTargetLoweringInfo().isTypeLegal(VT))
        return SDValue();

    SDVTList VTs = DAG.getVTList(VT, MVT::i64);

    unsigned Opc;
    bool ExtraOp = false;
    switch (Op.getOpcode()) {
        default: llvm_unreachable("Invalid code");
        case ISD::ADDC: Opc = Cse523ISD::ADD; break;
        case ISD::ADDE: Opc = Cse523ISD::ADC; ExtraOp = true; break;
        case ISD::SUBC: Opc = Cse523ISD::SUB; break;
        case ISD::SUBE: Opc = Cse523ISD::SUB; ExtraOp = true; break;
    }

    if (!ExtraOp)
        return DAG.getNode(Opc, SDLoc(Op), VTs, Op.getOperand(0),
                Op.getOperand(1));
    return DAG.getNode(Opc, SDLoc(Op), VTs, Op.getOperand(0),
            Op.getOperand(1), Op.getOperand(2));
}

static SDValue LowerFSINCOS(SDValue Op, const Cse523Subtarget *Subtarget,
        SelectionDAG &DAG) {
    assert(Subtarget->isTargetDarwin() && Subtarget->is64Bit());

    // For MacOSX, we want to call an alternative entry point: __sincos_stret,
    // which returns the values as { float, float } (in XMM0) or
    // { double, double } (which is returned in XMM0, XMM1).
    SDLoc dl(Op);
    SDValue Arg = Op.getOperand(0);
    EVT ArgVT = Arg.getValueType();
    Type *ArgTy = ArgVT.getTypeForEVT(*DAG.getContext());

    TargetLowering::ArgListTy Args;
    TargetLowering::ArgListEntry Entry;

    Entry.Node = Arg;
    Entry.Ty = ArgTy;
    Entry.isSExt = false;
    Entry.isZExt = false;
    Args.push_back(Entry);

    bool isF64 = ArgVT == MVT::f64;
    // Only optimize cse523_64 for now. i386 is a bit messy. For f32,
    // the small struct {f32, f32} is returned in (eax, edx). For f64,
    // the results are returned via SRet in memory.
    const char *LibcallName =  isF64 ? "__sincos_stret" : "__sincosf_stret";
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    SDValue Callee = DAG.getExternalSymbol(LibcallName, TLI.getPointerTy());

    Type *RetTy = isF64
        ? (Type*)StructType::get(ArgTy, ArgTy, NULL)
        : (Type*)VectorType::get(ArgTy, 4);
    TargetLowering::
        CallLoweringInfo CLI(DAG.getEntryNode(), RetTy,
                false, false, false, false, 0,
                CallingConv::C, /*isTaillCall=*/false,
                /*doesNotRet=*/false, /*isReturnValueUsed*/true,
                Callee, Args, DAG, dl);
    std::pair<SDValue, SDValue> CallResult = TLI.LowerCallTo(CLI);

    if (isF64)
        // Returned in xmm0 and xmm1.
        return CallResult.first;

    // Returned in bits 0:31 and 32:64 xmm0.
    SDValue SinVal = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, ArgVT,
            CallResult.first, DAG.getIntPtrConstant(0));
    SDValue CosVal = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, ArgVT,
            CallResult.first, DAG.getIntPtrConstant(1));
    SDVTList Tys = DAG.getVTList(ArgVT, ArgVT);
    return DAG.getNode(ISD::MERGE_VALUES, dl, Tys, SinVal, CosVal);
}

static SDValue LowerCONCAT_VECTORS(SDValue Op, SelectionDAG &DAG) {
    llvm_unreachable("Unexpected Vector type");
    return SDValue();
}

/// LowerOperation - Provide custom lowering hooks for some operations.
///
SDValue Cse523TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
    switch (Op.getOpcode()) {
        default: llvm_unreachable("Should not custom lower this!");
        case ISD::SIGN_EXTEND_INREG:  return LowerSIGN_EXTEND_INREG(Op,DAG);
        case ISD::ATOMIC_FENCE:       return LowerATOMIC_FENCE(Op, Subtarget, DAG);
        case ISD::ATOMIC_CMP_SWAP:    return LowerCMP_SWAP(Op, Subtarget, DAG);
        case ISD::ATOMIC_LOAD_SUB:    return LowerLOAD_SUB(Op,DAG);
        case ISD::ATOMIC_STORE:       return LowerATOMIC_STORE(Op,DAG);
        case ISD::BUILD_VECTOR:       return LowerBUILD_VECTOR(Op, DAG);
        case ISD::CONCAT_VECTORS:     return LowerCONCAT_VECTORS(Op, DAG);
        case ISD::VECTOR_SHUFFLE:     return LowerVECTOR_SHUFFLE(Op, DAG);
        case ISD::EXTRACT_VECTOR_ELT: return LowerEXTRACT_VECTOR_ELT(Op, DAG);
        case ISD::INSERT_VECTOR_ELT:  return LowerINSERT_VECTOR_ELT(Op, DAG);
        case ISD::EXTRACT_SUBVECTOR:  return LowerEXTRACT_SUBVECTOR(Op,Subtarget,DAG);
        case ISD::INSERT_SUBVECTOR:   return LowerINSERT_SUBVECTOR(Op, Subtarget,DAG);
        case ISD::SCALAR_TO_VECTOR:   return LowerSCALAR_TO_VECTOR(Op, DAG);
        case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
        case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
        case ISD::GlobalTLSAddress:   return LowerGlobalTLSAddress(Op, DAG);
        case ISD::ExternalSymbol:     return LowerExternalSymbol(Op, DAG);
        case ISD::BlockAddress:       return LowerBlockAddress(Op, DAG);
        case ISD::SHL_PARTS:
        case ISD::SRA_PARTS:
        case ISD::SRL_PARTS:          return LowerShiftParts(Op, DAG);
        case ISD::SINT_TO_FP:         return LowerSINT_TO_FP(Op, DAG);
        case ISD::UINT_TO_FP:         return LowerUINT_TO_FP(Op, DAG);
        case ISD::TRUNCATE:           return LowerTRUNCATE(Op, DAG);
        case ISD::ZERO_EXTEND:        return LowerZERO_EXTEND(Op, Subtarget, DAG);
        case ISD::SIGN_EXTEND:        return LowerSIGN_EXTEND(Op, Subtarget, DAG);
        case ISD::ANY_EXTEND:         return LowerANY_EXTEND(Op, Subtarget, DAG);
        case ISD::FP_TO_SINT:         return LowerFP_TO_SINT(Op, DAG);
        case ISD::FP_TO_UINT:         return LowerFP_TO_UINT(Op, DAG);
        case ISD::FP_EXTEND:          return LowerFP_EXTEND(Op, DAG);
        case ISD::FABS:               return LowerFABS(Op, DAG);
        case ISD::FNEG:               return LowerFNEG(Op, DAG);
        case ISD::FCOPYSIGN:          return LowerFCOPYSIGN(Op, DAG);
        case ISD::FGETSIGN:           return LowerFGETSIGN(Op, DAG);
        case ISD::SETCC:              return LowerSETCC(Op, DAG);
        case ISD::SELECT:             return LowerSELECT(Op, DAG);
        case ISD::BRCOND:             return LowerBRCOND(Op, DAG);
        case ISD::JumpTable:          return LowerJumpTable(Op, DAG);
        case ISD::VASTART:            return LowerVASTART(Op, DAG);
        case ISD::VAARG:              return LowerVAARG(Op, DAG);
        case ISD::VACOPY:             return LowerVACOPY(Op, Subtarget, DAG);
        case ISD::INTRINSIC_WO_CHAIN: return LowerINTRINSIC_WO_CHAIN(Op, DAG);
        case ISD::INTRINSIC_VOID:
        case ISD::INTRINSIC_W_CHAIN:  return LowerINTRINSIC_W_CHAIN(Op, Subtarget, DAG);
        case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG);
        case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG);
        case ISD::FRAME_TO_ARGS_OFFSET:
                                      return LowerFRAME_TO_ARGS_OFFSET(Op, DAG);
        case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG);
        case ISD::EH_RETURN:          return LowerEH_RETURN(Op, DAG);
        case ISD::EH_SJLJ_SETJMP:     return lowerEH_SJLJ_SETJMP(Op, DAG);
        case ISD::EH_SJLJ_LONGJMP:    return lowerEH_SJLJ_LONGJMP(Op, DAG);
        case ISD::INIT_TRAMPOLINE:    return LowerINIT_TRAMPOLINE(Op, DAG);
        case ISD::ADJUST_TRAMPOLINE:  return LowerADJUST_TRAMPOLINE(Op, DAG);
        case ISD::FLT_ROUNDS_:        return LowerFLT_ROUNDS_(Op, DAG);
        case ISD::CTLZ:               return LowerCTLZ(Op, DAG);
        case ISD::CTLZ_ZERO_UNDEF:    return LowerCTLZ_ZERO_UNDEF(Op, DAG);
        case ISD::CTTZ:               return LowerCTTZ(Op, DAG);
        case ISD::MUL:                return LowerMUL(Op, Subtarget, DAG);
        case ISD::SRA:
        case ISD::SRL:
        case ISD::SHL:                return LowerShift(Op, Subtarget, DAG);
        case ISD::SADDO:
        case ISD::UADDO:
        case ISD::SSUBO:
        case ISD::USUBO:
        case ISD::SMULO:
        case ISD::UMULO:              return LowerXALUO(Op, DAG);
        case ISD::READCYCLECOUNTER:   return LowerREADCYCLECOUNTER(Op, Subtarget,DAG);
        case ISD::BITCAST:            return LowerBITCAST(Op, Subtarget, DAG);
        case ISD::ADDC:
        case ISD::ADDE:
        case ISD::SUBC:
        case ISD::SUBE:               return LowerADDC_ADDE_SUBC_SUBE(Op, DAG);
        case ISD::ADD:                return LowerADD(Op, DAG);
        case ISD::SUB:                return LowerSUB(Op, DAG);
        case ISD::SDIV:               return LowerSDIV(Op, DAG);
        case ISD::FSINCOS:            return LowerFSINCOS(Op, Subtarget, DAG);
    }
}

static void ReplaceATOMIC_LOAD(SDNode *Node,
        SmallVectorImpl<SDValue> &Results,
        SelectionDAG &DAG) {
    SDLoc dl(Node);
    EVT VT = cast<AtomicSDNode>(Node)->getMemoryVT();

    // Convert wide load -> cmpxchg8b/cmpxchg16b
    // FIXME: On 32-bit, load -> fild or movq would be more efficient
    //        (The only way to get a 16-byte load is cmpxchg16b)
    // FIXME: 16-byte ATOMIC_CMP_SWAP isn't actually hooked up at the moment.
    SDValue Zero = DAG.getConstant(0, VT);
    SDValue Swap = DAG.getAtomic(ISD::ATOMIC_CMP_SWAP, dl, VT,
            Node->getOperand(0),
            Node->getOperand(1), Zero, Zero,
            cast<AtomicSDNode>(Node)->getMemOperand(),
            cast<AtomicSDNode>(Node)->getOrdering(),
            cast<AtomicSDNode>(Node)->getSynchScope());
    Results.push_back(Swap.getValue(0));
    Results.push_back(Swap.getValue(1));
}

static void
ReplaceATOMIC_BINARY_64(SDNode *Node, SmallVectorImpl<SDValue>&Results,
        SelectionDAG &DAG, unsigned NewOp) {
    SDLoc dl(Node);
    assert (Node->getValueType(0) == MVT::i64 &&
            "Only know how to expand i64 atomics");

    SDValue Chain = Node->getOperand(0);
    SDValue In1 = Node->getOperand(1);
    SDValue In2L = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, MVT::i32,
            Node->getOperand(2), DAG.getIntPtrConstant(0));
    SDValue In2H = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, MVT::i32,
            Node->getOperand(2), DAG.getIntPtrConstant(1));
    SDValue Ops[] = { Chain, In1, In2L, In2H };
    SDVTList Tys = DAG.getVTList(MVT::i32, MVT::i32, MVT::Other);
    SDValue Result =
        DAG.getMemIntrinsicNode(NewOp, dl, Tys, Ops, array_lengthof(Ops), MVT::i64,
                cast<MemSDNode>(Node)->getMemOperand());
    SDValue OpsF[] = { Result.getValue(0), Result.getValue(1)};
    Results.push_back(DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, OpsF, 2));
    Results.push_back(Result.getValue(2));
}

/// ReplaceNodeResults - Replace a node with an illegal result type
/// with a new node built out of custom code.
void Cse523TargetLowering::ReplaceNodeResults(SDNode *N,
        SmallVectorImpl<SDValue>&Results,
        SelectionDAG &DAG) const {
    SDLoc dl(N);
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    switch (N->getOpcode()) {
        default:
            llvm_unreachable("Do not know how to custom type legalize this operation!");
        case ISD::SIGN_EXTEND_INREG:
        case ISD::ADDC:
        case ISD::ADDE:
        case ISD::SUBC:
        case ISD::SUBE:
        case ISD::SADDO:
        case ISD::UADDO:
        case ISD::SSUBO:
        case ISD::USUBO:
        case ISD::SMULO:
        case ISD::UMULO:
            // We don't want to expand or promote these.
            return;
        case ISD::FP_TO_SINT:
        case ISD::FP_TO_UINT: {
                                  bool IsSigned = N->getOpcode() == ISD::FP_TO_SINT;

                                  if (!IsSigned && !isIntegerTypeFTOL(SDValue(N, 0).getValueType()))
                                      return;

                                  std::pair<SDValue,SDValue> Vals =
                                      FP_TO_INTHelper(SDValue(N, 0), DAG, IsSigned, /*IsReplace=*/ true);
                                  SDValue FIST = Vals.first, StackSlot = Vals.second;
                                  if (FIST.getNode() != 0) {
                                      EVT VT = N->getValueType(0);
                                      // Return a load from the stack slot.
                                      if (StackSlot.getNode() != 0)
                                          Results.push_back(DAG.getLoad(VT, dl, FIST, StackSlot,
                                                      MachinePointerInfo(),
                                                      false, false, false, 0));
                                      else
                                          Results.push_back(FIST);
                                  }
                                  return;
                              }
        case ISD::UINT_TO_FP: {
                                  assert(Subtarget->hasSSE2() && "Requires at least SSE2!");
                                  if (N->getOperand(0).getValueType() != MVT::v2i32 ||
                                          N->getValueType(0) != MVT::v2f32)
                                      return;
                                  SDValue ZExtIn = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::v2i64,
                                          N->getOperand(0));
                                  SDValue Bias = DAG.getConstantFP(BitsToDouble(0x4330000000000000ULL),
                                          MVT::f64);
                                  SDValue VBias = DAG.getNode(ISD::BUILD_VECTOR, dl, MVT::v2f64, Bias, Bias);
                                  SDValue Or = DAG.getNode(ISD::OR, dl, MVT::v2i64, ZExtIn,
                                          DAG.getNode(ISD::BITCAST, dl, MVT::v2i64, VBias));
                                  Or = DAG.getNode(ISD::BITCAST, dl, MVT::v2f64, Or);
                                  SDValue Sub = DAG.getNode(ISD::FSUB, dl, MVT::v2f64, Or, VBias);
                                  Results.push_back(DAG.getNode(Cse523ISD::VFPROUND, dl, MVT::v4f32, Sub));
                                  return;
                              }
        case ISD::FP_ROUND: {
                                if (!TLI.isTypeLegal(N->getOperand(0).getValueType()))
                                    return;
                                SDValue V = DAG.getNode(Cse523ISD::VFPROUND, dl, MVT::v4f32, N->getOperand(0));
                                Results.push_back(V);
                                return;
                            }
        case ISD::READCYCLECOUNTER: {
                                        SDVTList Tys = DAG.getVTList(MVT::Other, MVT::Glue);
                                        SDValue TheChain = N->getOperand(0);
                                        SDValue rd = DAG.getNode(Cse523ISD::RDTSC_DAG, dl, Tys, &TheChain, 1);
                                        // TODO: Replaced
                                        assert(0);
                                        SDValue eax = DAG.getCopyFromReg(rd, dl, Cse523::RAX, MVT::i32,
                                                rd.getValue(1));
                                        SDValue edx = DAG.getCopyFromReg(eax.getValue(1), dl, Cse523::RDX, MVT::i32,
                                                eax.getValue(2));
                                        // Use a buildpair to merge the two 32-bit values into a 64-bit one.
                                        SDValue Ops[] = { eax, edx };
                                        Results.push_back(DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, Ops,
                                                    array_lengthof(Ops)));
                                        Results.push_back(edx.getValue(1));
                                        return;
                                    }
        case ISD::ATOMIC_CMP_SWAP: {
                                       EVT T = N->getValueType(0);
                                       assert((T == MVT::i64 || T == MVT::i128) && "can only expand cmpxchg pair");
                                       bool Regs64bit = T == MVT::i128;
                                       EVT HalfT = Regs64bit ? MVT::i64 : MVT::i32;
                                       SDValue cpInL, cpInH;
                                       cpInL = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, HalfT, N->getOperand(2),
                                               DAG.getConstant(0, HalfT));
                                       cpInH = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, HalfT, N->getOperand(2),
                                               DAG.getConstant(1, HalfT));
                                       cpInL = DAG.getCopyToReg(N->getOperand(0), dl,
                                               Cse523::RAX,
                                               cpInL, SDValue());
                                       cpInH = DAG.getCopyToReg(cpInL.getValue(0), dl,
                                               Cse523::RDX,
                                               cpInH, cpInL.getValue(1));
                                       SDValue swapInL, swapInH;
                                       swapInL = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, HalfT, N->getOperand(3),
                                               DAG.getConstant(0, HalfT));
                                       swapInH = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, HalfT, N->getOperand(3),
                                               DAG.getConstant(1, HalfT));
                                       swapInL = DAG.getCopyToReg(cpInH.getValue(0), dl,
                                               Cse523::RBX,
                                               swapInL, cpInH.getValue(1));
                                       swapInH = DAG.getCopyToReg(swapInL.getValue(0), dl,
                                               Cse523::RCX,
                                               swapInH, swapInL.getValue(1));
                                       SDValue Ops[] = { swapInH.getValue(0),
                                           N->getOperand(1),
                                           swapInH.getValue(1) };
                                       SDVTList Tys = DAG.getVTList(MVT::Other, MVT::Glue);
                                       MachineMemOperand *MMO = cast<AtomicSDNode>(N)->getMemOperand();
                                       unsigned Opcode = Regs64bit ? Cse523ISD::LCMPXCHG16_DAG :
                                           Cse523ISD::LCMPXCHG8_DAG;
                                       SDValue Result = DAG.getMemIntrinsicNode(Opcode, dl, Tys,
                                               Ops, array_lengthof(Ops), T, MMO);
                                       SDValue cpOutL = DAG.getCopyFromReg(Result.getValue(0), dl,
                                               Cse523::RAX,
                                               HalfT, Result.getValue(1));
                                       SDValue cpOutH = DAG.getCopyFromReg(cpOutL.getValue(1), dl,
                                               Cse523::RDX,
                                               HalfT, cpOutL.getValue(2));
                                       SDValue OpsF[] = { cpOutL.getValue(0), cpOutH.getValue(0)};
                                       Results.push_back(DAG.getNode(ISD::BUILD_PAIR, dl, T, OpsF, 2));
                                       Results.push_back(cpOutH.getValue(1));
                                       return;
                                   }
        case ISD::ATOMIC_LOAD_ADD:
        case ISD::ATOMIC_LOAD_AND:
        case ISD::ATOMIC_LOAD_NAND:
        case ISD::ATOMIC_LOAD_OR:
        case ISD::ATOMIC_LOAD_SUB:
        case ISD::ATOMIC_LOAD_XOR:
        case ISD::ATOMIC_LOAD_MAX:
        case ISD::ATOMIC_LOAD_MIN:
        case ISD::ATOMIC_LOAD_UMAX:
        case ISD::ATOMIC_LOAD_UMIN:
        case ISD::ATOMIC_SWAP: {
                                   unsigned Opc;
                                   switch (N->getOpcode()) {
                                       default: llvm_unreachable("Unexpected opcode");
                                       case ISD::ATOMIC_LOAD_ADD:
                                                Opc = Cse523ISD::ATOMADD64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_AND:
                                                Opc = Cse523ISD::ATOMAND64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_NAND:
                                                Opc = Cse523ISD::ATOMNAND64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_OR:
                                                Opc = Cse523ISD::ATOMOR64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_SUB:
                                                Opc = Cse523ISD::ATOMSUB64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_XOR:
                                                Opc = Cse523ISD::ATOMXOR64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_MAX:
                                                Opc = Cse523ISD::ATOMMAX64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_MIN:
                                                Opc = Cse523ISD::ATOMMIN64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_UMAX:
                                                Opc = Cse523ISD::ATOMUMAX64_DAG;
                                                break;
                                       case ISD::ATOMIC_LOAD_UMIN:
                                                Opc = Cse523ISD::ATOMUMIN64_DAG;
                                                break;
                                       case ISD::ATOMIC_SWAP:
                                                Opc = Cse523ISD::ATOMSWAP64_DAG;
                                                break;
                                   }
                                   ReplaceATOMIC_BINARY_64(N, Results, DAG, Opc);
                                   return;
                               }
        case ISD::ATOMIC_LOAD:
                               ReplaceATOMIC_LOAD(N, Results, DAG);
    }
}

const char *Cse523TargetLowering::getTargetNodeName(unsigned Opcode) const {
    switch (Opcode) {
        default: return NULL;
        case Cse523ISD::BSF:                return "Cse523ISD::BSF";
        case Cse523ISD::BSR:                return "Cse523ISD::BSR";
        case Cse523ISD::SHLD:               return "Cse523ISD::SHLD";
        case Cse523ISD::SHRD:               return "Cse523ISD::SHRD";
        case Cse523ISD::FAND:               return "Cse523ISD::FAND";
        case Cse523ISD::FANDN:              return "Cse523ISD::FANDN";
        case Cse523ISD::FOR:                return "Cse523ISD::FOR";
        case Cse523ISD::FXOR:               return "Cse523ISD::FXOR";
        case Cse523ISD::FSRL:               return "Cse523ISD::FSRL";
        case Cse523ISD::FILD:               return "Cse523ISD::FILD";
        case Cse523ISD::FILD_FLAG:          return "Cse523ISD::FILD_FLAG";
        case Cse523ISD::FP_TO_INT16_IN_MEM: return "Cse523ISD::FP_TO_INT16_IN_MEM";
        case Cse523ISD::FP_TO_INT32_IN_MEM: return "Cse523ISD::FP_TO_INT32_IN_MEM";
        case Cse523ISD::FP_TO_INT64_IN_MEM: return "Cse523ISD::FP_TO_INT64_IN_MEM";
        case Cse523ISD::FLD:                return "Cse523ISD::FLD";
        case Cse523ISD::FST:                return "Cse523ISD::FST";
        case Cse523ISD::CALL:               return "Cse523ISD::CALL";
        case Cse523ISD::RDTSC_DAG:          return "Cse523ISD::RDTSC_DAG";
        case Cse523ISD::BT:                 return "Cse523ISD::BT";
        case Cse523ISD::CMP:                return "Cse523ISD::CMP";
        case Cse523ISD::COMI:               return "Cse523ISD::COMI";
        case Cse523ISD::UCOMI:              return "Cse523ISD::UCOMI";
        case Cse523ISD::CMPM:               return "Cse523ISD::CMPM";
        case Cse523ISD::CMPMU:              return "Cse523ISD::CMPMU";
        case Cse523ISD::SETCC:              return "Cse523ISD::SETCC";
        case Cse523ISD::SETCC_CARRY:        return "Cse523ISD::SETCC_CARRY";
        case Cse523ISD::FSETCC:             return "Cse523ISD::FSETCC";
        case Cse523ISD::CMOV:               return "Cse523ISD::CMOV";
        case Cse523ISD::BRCOND:             return "Cse523ISD::BRCOND";
        case Cse523ISD::RET_FLAG:           return "Cse523ISD::RET_FLAG";
        case Cse523ISD::REP_STOS:           return "Cse523ISD::REP_STOS";
        case Cse523ISD::REP_MOVS:           return "Cse523ISD::REP_MOVS";
        case Cse523ISD::GlobalBaseReg:      return "Cse523ISD::GlobalBaseReg";
        case Cse523ISD::Wrapper:            return "Cse523ISD::Wrapper";
        case Cse523ISD::WrapperRIP:         return "Cse523ISD::WrapperRIP";
        case Cse523ISD::PEXTRB:             return "Cse523ISD::PEXTRB";
        case Cse523ISD::PEXTRW:             return "Cse523ISD::PEXTRW";
        case Cse523ISD::INSERTPS:           return "Cse523ISD::INSERTPS";
        case Cse523ISD::PINSRB:             return "Cse523ISD::PINSRB";
        case Cse523ISD::PINSRW:             return "Cse523ISD::PINSRW";
        case Cse523ISD::PSHUFB:             return "Cse523ISD::PSHUFB";
        case Cse523ISD::ANDNP:              return "Cse523ISD::ANDNP";
        case Cse523ISD::PSIGN:              return "Cse523ISD::PSIGN";
        case Cse523ISD::BLENDV:             return "Cse523ISD::BLENDV";
        case Cse523ISD::BLENDI:             return "Cse523ISD::BLENDI";
        case Cse523ISD::SUBUS:              return "Cse523ISD::SUBUS";
        case Cse523ISD::HADD:               return "Cse523ISD::HADD";
        case Cse523ISD::HSUB:               return "Cse523ISD::HSUB";
        case Cse523ISD::FHADD:              return "Cse523ISD::FHADD";
        case Cse523ISD::FHSUB:              return "Cse523ISD::FHSUB";
        case Cse523ISD::UMAX:               return "Cse523ISD::UMAX";
        case Cse523ISD::UMIN:               return "Cse523ISD::UMIN";
        case Cse523ISD::SMAX:               return "Cse523ISD::SMAX";
        case Cse523ISD::SMIN:               return "Cse523ISD::SMIN";
        case Cse523ISD::FMAX:               return "Cse523ISD::FMAX";
        case Cse523ISD::FMIN:               return "Cse523ISD::FMIN";
        case Cse523ISD::FMAXC:              return "Cse523ISD::FMAXC";
        case Cse523ISD::FMINC:              return "Cse523ISD::FMINC";
        case Cse523ISD::FRSQRT:             return "Cse523ISD::FRSQRT";
        case Cse523ISD::FRCP:               return "Cse523ISD::FRCP";
        case Cse523ISD::TLSADDR:            return "Cse523ISD::TLSADDR";
        case Cse523ISD::TLSBASEADDR:        return "Cse523ISD::TLSBASEADDR";
        case Cse523ISD::TLSCALL:            return "Cse523ISD::TLSCALL";
        case Cse523ISD::EH_SJLJ_SETJMP:     return "Cse523ISD::EH_SJLJ_SETJMP";
        case Cse523ISD::EH_SJLJ_LONGJMP:    return "Cse523ISD::EH_SJLJ_LONGJMP";
        case Cse523ISD::EH_RETURN:          return "Cse523ISD::EH_RETURN";
        case Cse523ISD::TC_RETURN:          return "Cse523ISD::TC_RETURN";
        case Cse523ISD::FNSTCW16m:          return "Cse523ISD::FNSTCW16m";
        case Cse523ISD::FNSTSW16r:          return "Cse523ISD::FNSTSW16r";
        case Cse523ISD::LCMPXCHG_DAG:       return "Cse523ISD::LCMPXCHG_DAG";
        case Cse523ISD::LCMPXCHG8_DAG:      return "Cse523ISD::LCMPXCHG8_DAG";
        case Cse523ISD::ATOMADD64_DAG:      return "Cse523ISD::ATOMADD64_DAG";
        case Cse523ISD::ATOMSUB64_DAG:      return "Cse523ISD::ATOMSUB64_DAG";
        case Cse523ISD::ATOMOR64_DAG:       return "Cse523ISD::ATOMOR64_DAG";
        case Cse523ISD::ATOMXOR64_DAG:      return "Cse523ISD::ATOMXOR64_DAG";
        case Cse523ISD::ATOMAND64_DAG:      return "Cse523ISD::ATOMAND64_DAG";
        case Cse523ISD::ATOMNAND64_DAG:     return "Cse523ISD::ATOMNAND64_DAG";
        case Cse523ISD::VZEXT_MOVL:         return "Cse523ISD::VZEXT_MOVL";
        case Cse523ISD::VZEXT_LOAD:         return "Cse523ISD::VZEXT_LOAD";
        case Cse523ISD::VZEXT:              return "Cse523ISD::VZEXT";
        case Cse523ISD::VSEXT:              return "Cse523ISD::VSEXT";
        case Cse523ISD::VTRUNC:             return "Cse523ISD::VTRUNC";
        case Cse523ISD::VTRUNCM:            return "Cse523ISD::VTRUNCM";
        case Cse523ISD::VINSERT:            return "Cse523ISD::VINSERT";
        case Cse523ISD::VFPEXT:             return "Cse523ISD::VFPEXT";
        case Cse523ISD::VFPROUND:           return "Cse523ISD::VFPROUND";
        case Cse523ISD::VSHLDQ:             return "Cse523ISD::VSHLDQ";
        case Cse523ISD::VSRLDQ:             return "Cse523ISD::VSRLDQ";
        case Cse523ISD::VSHL:               return "Cse523ISD::VSHL";
        case Cse523ISD::VSRL:               return "Cse523ISD::VSRL";
        case Cse523ISD::VSRA:               return "Cse523ISD::VSRA";
        case Cse523ISD::VSHLI:              return "Cse523ISD::VSHLI";
        case Cse523ISD::VSRLI:              return "Cse523ISD::VSRLI";
        case Cse523ISD::VSRAI:              return "Cse523ISD::VSRAI";
        case Cse523ISD::CMPP:               return "Cse523ISD::CMPP";
        case Cse523ISD::PCMPEQ:             return "Cse523ISD::PCMPEQ";
        case Cse523ISD::PCMPGT:             return "Cse523ISD::PCMPGT";
        case Cse523ISD::PCMPEQM:            return "Cse523ISD::PCMPEQM";
        case Cse523ISD::PCMPGTM:            return "Cse523ISD::PCMPGTM";
        case Cse523ISD::ADD:                return "Cse523ISD::ADD";
        case Cse523ISD::SUB:                return "Cse523ISD::SUB";
        case Cse523ISD::ADC:                return "Cse523ISD::ADC";
//        case Cse523ISD::SBB:                return "Cse523ISD::SBB";
        case Cse523ISD::SMUL:               return "Cse523ISD::SMUL";
        case Cse523ISD::UMUL:               return "Cse523ISD::UMUL";
//        case Cse523ISD::INC:                return "Cse523ISD::INC";
//        case Cse523ISD::DEC:                return "Cse523ISD::DEC";
        case Cse523ISD::OR:                 return "Cse523ISD::OR";
        case Cse523ISD::XOR:                return "Cse523ISD::XOR";
        case Cse523ISD::AND:                return "Cse523ISD::AND";
        case Cse523ISD::BZHI:               return "Cse523ISD::BZHI";
        case Cse523ISD::BEXTR:              return "Cse523ISD::BEXTR";
        case Cse523ISD::MUL_IMM:            return "Cse523ISD::MUL_IMM";
        case Cse523ISD::PTEST:              return "Cse523ISD::PTEST";
        case Cse523ISD::TESTP:              return "Cse523ISD::TESTP";
        case Cse523ISD::TESTM:              return "Cse523ISD::TESTM";
        case Cse523ISD::TESTNM:             return "Cse523ISD::TESTNM";
        case Cse523ISD::KORTEST:            return "Cse523ISD::KORTEST";
        case Cse523ISD::PALIGNR:            return "Cse523ISD::PALIGNR";
        case Cse523ISD::PSHUFD:             return "Cse523ISD::PSHUFD";
        case Cse523ISD::PSHUFHW:            return "Cse523ISD::PSHUFHW";
        case Cse523ISD::PSHUFLW:            return "Cse523ISD::PSHUFLW";
        case Cse523ISD::SHUFP:              return "Cse523ISD::SHUFP";
        case Cse523ISD::MOVLHPS:            return "Cse523ISD::MOVLHPS";
        case Cse523ISD::MOVLHPD:            return "Cse523ISD::MOVLHPD";
        case Cse523ISD::MOVHLPS:            return "Cse523ISD::MOVHLPS";
        case Cse523ISD::MOVLPS:             return "Cse523ISD::MOVLPS";
        case Cse523ISD::MOVLPD:             return "Cse523ISD::MOVLPD";
        case Cse523ISD::MOVDDUP:            return "Cse523ISD::MOVDDUP";
        case Cse523ISD::MOVSHDUP:           return "Cse523ISD::MOVSHDUP";
        case Cse523ISD::MOVSLDUP:           return "Cse523ISD::MOVSLDUP";
        case Cse523ISD::MOVSD:              return "Cse523ISD::MOVSD";
        case Cse523ISD::MOVSS:              return "Cse523ISD::MOVSS";
        case Cse523ISD::UNPCKL:             return "Cse523ISD::UNPCKL";
        case Cse523ISD::UNPCKH:             return "Cse523ISD::UNPCKH";
        case Cse523ISD::VBROADCAST:         return "Cse523ISD::VBROADCAST";
        case Cse523ISD::VBROADCASTM:        return "Cse523ISD::VBROADCASTM";
        case Cse523ISD::VPERMILP:           return "Cse523ISD::VPERMILP";
        case Cse523ISD::VPERM2X128:         return "Cse523ISD::VPERM2X128";
        case Cse523ISD::VPERMV:             return "Cse523ISD::VPERMV";
        case Cse523ISD::VPERMV3:            return "Cse523ISD::VPERMV3";
        case Cse523ISD::VPERMIV3:           return "Cse523ISD::VPERMIV3";
        case Cse523ISD::VPERMI:             return "Cse523ISD::VPERMI";
        case Cse523ISD::PMULUDQ:            return "Cse523ISD::PMULUDQ";
        case Cse523ISD::VASTART_SAVE_XMM_REGS: return "Cse523ISD::VASTART_SAVE_XMM_REGS";
        case Cse523ISD::VAARG_64:           return "Cse523ISD::VAARG_64";
        case Cse523ISD::WIN_ALLOCA:         return "Cse523ISD::WIN_ALLOCA";
        case Cse523ISD::MEMBARRIER:         return "Cse523ISD::MEMBARRIER";
        case Cse523ISD::SEG_ALLOCA:         return "Cse523ISD::SEG_ALLOCA";
        case Cse523ISD::WIN_FTOL:           return "Cse523ISD::WIN_FTOL";
        case Cse523ISD::SAHF:               return "Cse523ISD::SAHF";
        case Cse523ISD::RDRAND:             return "Cse523ISD::RDRAND";
        case Cse523ISD::RDSEED:             return "Cse523ISD::RDSEED";
        case Cse523ISD::FMADD:              return "Cse523ISD::FMADD";
        case Cse523ISD::FMSUB:              return "Cse523ISD::FMSUB";
        case Cse523ISD::FNMADD:             return "Cse523ISD::FNMADD";
        case Cse523ISD::FNMSUB:             return "Cse523ISD::FNMSUB";
        case Cse523ISD::FMADDSUB:           return "Cse523ISD::FMADDSUB";
        case Cse523ISD::FMSUBADD:           return "Cse523ISD::FMSUBADD";
        case Cse523ISD::PCMPESTRI:          return "Cse523ISD::PCMPESTRI";
        case Cse523ISD::PCMPISTRI:          return "Cse523ISD::PCMPISTRI";
        case Cse523ISD::XTEST:              return "Cse523ISD::XTEST";
    }
}

// isLegalAddressingMode - Return true if the addressing mode represented
// by AM is legal for this target, for a load/store of the specified type.
bool Cse523TargetLowering::isLegalAddressingMode(const AddrMode &AM,
        Type *Ty) const {
    // Cse523 supports extremely general addressing modes.
    CodeModel::Model M = getTargetMachine().getCodeModel();
    Reloc::Model R = getTargetMachine().getRelocationModel();

    // Cse523 allows a sign-extended 32-bit immediate field as a displacement.
    if (!Cse523::isOffsetSuitableForCodeModel(AM.BaseOffs, M, AM.BaseGV != NULL))
        return false;

    if (AM.BaseGV) {
        unsigned GVFlags =
            Subtarget->ClassifyGlobalReference(AM.BaseGV, getTargetMachine());

        // If a reference to this global requires an extra load, we can't fold it.
        if (isGlobalStubReference(GVFlags))
            return false;

        // If BaseGV requires a register for the PIC base, we cannot also have a
        // BaseReg specified.
        if (AM.HasBaseReg && isGlobalRelativeToPICBase(GVFlags))
            return false;

        // If lower 4G is not available, then we must use rip-relative addressing.
        if ((M != CodeModel::Small || R != Reloc::Static) &&
                Subtarget->is64Bit() && (AM.BaseOffs || AM.Scale > 1))
            return false;
    }

    switch (AM.Scale) {
        case 0:
        case 1:
        case 2:
        case 4:
        case 8:
            // These scales always work.
            break;
        case 3:
        case 5:
        case 9:
            // These scales are formed with basereg+scalereg.  Only accept if there is
            // no basereg yet.
            if (AM.HasBaseReg)
                return false;
            break;
        default:  // Other stuff never works.
            return false;
    }

    return true;
}

bool Cse523TargetLowering::isVectorShiftByScalarCheap(Type *Ty) const {
    unsigned Bits = Ty->getScalarSizeInBits();

    // 8-bit shifts are always expensive, but versions with a scalar amount aren't
    // particularly cheaper than those without.
    if (Bits == 8)
        return false;

    // On AVX2 there are new vpsllv[dq] instructions (and other shifts), that make
    // variable shifts just as cheap as scalar ones.
    if (Subtarget->hasInt256() && (Bits == 32 || Bits == 64))
        return false;

    // Otherwise, it's significantly cheaper to shift by a scalar amount than by a
    // fully general vector.
    return true;
}

bool Cse523TargetLowering::isTruncateFree(Type *Ty1, Type *Ty2) const {
    return false;
}

bool Cse523TargetLowering::allowTruncateForTailCall(Type *Ty1, Type *Ty2) const {
    if (!Ty1->isIntegerTy() || !Ty2->isIntegerTy())
        return false;

    if (!isTypeLegal(EVT::getEVT(Ty1)))
        return false;

    assert(Ty1->getPrimitiveSizeInBits() <= 64 && "i128 is probably not a noop");

    // Assuming the caller doesn't have a zeroext or signext return parameter,
    // truncation all the way down to i1 is valid.
    return true;
}

bool Cse523TargetLowering::isLegalICmpImmediate(int64_t Imm) const {
    return isInt<32>(Imm);
}

bool Cse523TargetLowering::isLegalAddImmediate(int64_t Imm) const {
    // Can also use sub to handle negated immediates.
    return isInt<32>(Imm);
}

bool Cse523TargetLowering::isTruncateFree(EVT VT1, EVT VT2) const {
    return false;
}

bool Cse523TargetLowering::isZExtFree(Type *Ty1, Type *Ty2) const {
    return false;
}

bool Cse523TargetLowering::isZExtFree(EVT VT1, EVT VT2) const {
    return false;
}

bool Cse523TargetLowering::isZExtFree(SDValue Val, EVT VT2) const {
    return false;
}

bool
Cse523TargetLowering::isFMAFasterThanFMulAndFAdd(EVT VT) const {
    if (!(Subtarget->hasFMA() || Subtarget->hasFMA4()))
        return false;

    VT = VT.getScalarType();

    if (!VT.isSimple())
        return false;

    switch (VT.getSimpleVT().SimpleTy) {
        case MVT::f32:
        case MVT::f64:
            return true;
        default:
            break;
    }

    return false;
}

bool Cse523TargetLowering::isNarrowingProfitable(EVT VT1, EVT VT2) const {
    // i16 instructions are longer (0x66 prefix) and potentially slower.
    return !(VT1 == MVT::i32 && VT2 == MVT::i16);
}

/// isShuffleMaskLegal - Targets can use this to indicate that they only
/// support *some* VECTOR_SHUFFLE operations, those with specific masks.
/// By default, if a target supports the VECTOR_SHUFFLE node, all mask values
/// are assumed to be legal.
bool
Cse523TargetLowering::isShuffleMaskLegal(const SmallVectorImpl<int> &M,
        EVT VT) const {
    if (!VT.isSimple())
        return false;

    MVT SVT = VT.getSimpleVT();

    // Very little shuffling can be done for 64-bit vectors right now.
    if (VT.getSizeInBits() == 64)
        return false;

    // FIXME: pshufb, blends, shifts.
    return (SVT.getVectorNumElements() == 2 ||
            ShuffleVectorSDNode::isSplatMask(&M[0], VT) ||
            isMOVLMask(M, SVT) ||
            isSHUFPMask(M, SVT) ||
            isPSHUFDMask(M, SVT) ||
            isPSHUFHWMask(M, SVT, Subtarget->hasInt256()) ||
            isPSHUFLWMask(M, SVT, Subtarget->hasInt256()) ||
            isPALIGNRMask(M, SVT, Subtarget) ||
            isUNPCKLMask(M, SVT, Subtarget->hasInt256()) ||
            isUNPCKHMask(M, SVT, Subtarget->hasInt256()) ||
            isUNPCKL_v_undef_Mask(M, SVT, Subtarget->hasInt256()) ||
            isUNPCKH_v_undef_Mask(M, SVT, Subtarget->hasInt256()));
}

bool
Cse523TargetLowering::isVectorClearMaskLegal(const SmallVectorImpl<int> &Mask,
        EVT VT) const {
    if (!VT.isSimple())
        return false;

    MVT SVT = VT.getSimpleVT();
    unsigned NumElts = SVT.getVectorNumElements();
    // FIXME: This collection of masks seems suspect.
    if (NumElts == 2)
        return true;
    if (NumElts == 4 && SVT.is128BitVector()) {
        return (isMOVLMask(Mask, SVT)  ||
                isCommutedMOVLMask(Mask, SVT, true) ||
                isSHUFPMask(Mask, SVT) ||
                isSHUFPMask(Mask, SVT, /* Commuted */ true));
    }
    return false;
}

//===----------------------------------------------------------------------===//
//                           Cse523 Scheduler Hooks
//===----------------------------------------------------------------------===//

/// Utility function to emit xbegin specifying the start of an RTM region.
static MachineBasicBlock *EmitXBegin(MachineInstr *MI, MachineBasicBlock *MBB,
        const TargetInstrInfo *TII) {
    DebugLoc DL = MI->getDebugLoc();

    const BasicBlock *BB = MBB->getBasicBlock();
    MachineFunction::iterator I = MBB;
    ++I;

    // For the v = xbegin(), we generate
    //
    // thisMBB:
    //  xbegin sinkMBB
    //
    // mainMBB:
    //  eax = -1
    //
    // sinkMBB:
    //  v = eax

    MachineBasicBlock *thisMBB = MBB;
    MachineFunction *MF = MBB->getParent();
    MachineBasicBlock *mainMBB = MF->CreateMachineBasicBlock(BB);
    MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(BB);
    MF->insert(I, mainMBB);
    MF->insert(I, sinkMBB);

    // Transfer the remainder of BB and its successor edges to sinkMBB.
    sinkMBB->splice(sinkMBB->begin(), MBB,
            llvm::next(MachineBasicBlock::iterator(MI)), MBB->end());
    sinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

    assert(0);

    // thisMBB:
    //  xbegin sinkMBB
    //  # fallthrough to mainMBB
    //  # abortion to sinkMBB
//    BuildMI(thisMBB, DL, TII->get(Cse523::XBEGIN_4)).addMBB(sinkMBB);
//    thisMBB->addSuccessor(mainMBB);
//    thisMBB->addSuccessor(sinkMBB);
//
//    // mainMBB:
//    //  EAX = -1 TODO: Replaced
//    BuildMI(mainMBB, DL, TII->get(Cse523::MOV32ri), Cse523::RAX).addImm(-1);
//    mainMBB->addSuccessor(sinkMBB);

    // sinkMBB:
    // EAX is live into the sinkMBB
    sinkMBB->addLiveIn(Cse523::RAX);
    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
            TII->get(TargetOpcode::COPY), MI->getOperand(0).getReg())
        .addReg(Cse523::RAX);

    MI->eraseFromParent();
    return sinkMBB;
}

// Get CMPXCHG opcode for the specified data type.
static unsigned getCmpXChgOpcode(EVT VT) {
    switch (VT.getSimpleVT().SimpleTy) {
//        case MVT::i8:  return Cse523::LCMPXCHG8;
//        case MVT::i16: return Cse523::LCMPXCHG16;
//        case MVT::i32: return Cse523::LCMPXCHG32;
//        case MVT::i64: return Cse523::LCMPXCHG64;
        default:
                       break;
    }
    llvm_unreachable("Invalid operand size!");
}

// Get LOAD opcode for the specified data type.
static unsigned getLoadOpcode(EVT VT) {
    switch (VT.getSimpleVT().SimpleTy) {
        case MVT::i64: return Cse523::MOV64rm;
        default:
                       break;
    }
    llvm_unreachable("Invalid operand size!");
}

// Get opcode of the non-atomic one from the specified atomic instruction.
static unsigned getNonAtomicOpcode(unsigned Opc) {
//    switch (Opc) {
//        case Cse523::ATOMAND8:  return Cse523::AND8rr;
//        case Cse523::ATOMAND16: return Cse523::AND16rr;
//        case Cse523::ATOMAND32: return Cse523::AND32rr;
//        case Cse523::ATOMAND64: return Cse523::AND64rr;
//        case Cse523::ATOMOR8:   return Cse523::OR8rr;
//        case Cse523::ATOMOR16:  return Cse523::OR16rr;
//        case Cse523::ATOMOR32:  return Cse523::OR32rr;
//        case Cse523::ATOMOR64:  return Cse523::OR64rr;
//        case Cse523::ATOMXOR8:  return Cse523::XOR8rr;
//        case Cse523::ATOMXOR16: return Cse523::XOR16rr;
//        case Cse523::ATOMXOR32: return Cse523::XOR32rr;
//        case Cse523::ATOMXOR64: return Cse523::XOR64rr;
//    }
    llvm_unreachable("Unhandled atomic-load-op opcode!");
}

// Get opcode of the non-atomic one from the specified atomic instruction with
// extra opcode.
static unsigned getNonAtomicOpcodeWithExtraOpc(unsigned Opc,
        unsigned &ExtraOpc) {
//    switch (Opc) {
//        case Cse523::ATOMNAND8:  ExtraOpc = Cse523::NOT8r;   return Cse523::AND8rr;
//        case Cse523::ATOMNAND16: ExtraOpc = Cse523::NOT16r;  return Cse523::AND16rr;
//        case Cse523::ATOMNAND32: ExtraOpc = Cse523::NOT32r;  return Cse523::AND32rr;
//        case Cse523::ATOMNAND64: ExtraOpc = Cse523::NOT64r;  return Cse523::AND64rr;
//        case Cse523::ATOMMAX8:   ExtraOpc = Cse523::CMP8rr;  return Cse523::CMOVL32rr;
//        case Cse523::ATOMMAX16:  ExtraOpc = Cse523::CMP16rr; return Cse523::CMOVL16rr;
//        case Cse523::ATOMMAX32:  ExtraOpc = Cse523::CMP32rr; return Cse523::CMOVL32rr;
//        case Cse523::ATOMMAX64:  ExtraOpc = Cse523::CMP64rr; return Cse523::CMOVL64rr;
//        case Cse523::ATOMMIN8:   ExtraOpc = Cse523::CMP8rr;  return Cse523::CMOVG32rr;
//        case Cse523::ATOMMIN16:  ExtraOpc = Cse523::CMP16rr; return Cse523::CMOVG16rr;
//        case Cse523::ATOMMIN32:  ExtraOpc = Cse523::CMP32rr; return Cse523::CMOVG32rr;
//        case Cse523::ATOMMIN64:  ExtraOpc = Cse523::CMP64rr; return Cse523::CMOVG64rr;
//        case Cse523::ATOMUMAX8:  ExtraOpc = Cse523::CMP8rr;  return Cse523::CMOVB32rr;
//        case Cse523::ATOMUMAX16: ExtraOpc = Cse523::CMP16rr; return Cse523::CMOVB16rr;
//        case Cse523::ATOMUMAX32: ExtraOpc = Cse523::CMP32rr; return Cse523::CMOVB32rr;
//        case Cse523::ATOMUMAX64: ExtraOpc = Cse523::CMP64rr; return Cse523::CMOVB64rr;
//        case Cse523::ATOMUMIN8:  ExtraOpc = Cse523::CMP8rr;  return Cse523::CMOVA32rr;
//        case Cse523::ATOMUMIN16: ExtraOpc = Cse523::CMP16rr; return Cse523::CMOVA16rr;
//        case Cse523::ATOMUMIN32: ExtraOpc = Cse523::CMP32rr; return Cse523::CMOVA32rr;
//        case Cse523::ATOMUMIN64: ExtraOpc = Cse523::CMP64rr; return Cse523::CMOVA64rr;
//    }
    llvm_unreachable("Unhandled atomic-load-op opcode!");
}

// Get opcode of the non-atomic one from the specified atomic instruction for
// 64-bit data type on 32-bit target.
static unsigned getNonAtomic6432Opcode(unsigned Opc, unsigned &HiOpc) {
//    switch (Opc) {
//        case Cse523::ATOMAND6432:  HiOpc = Cse523::AND32rr; return Cse523::AND32rr;
//        case Cse523::ATOMOR6432:   HiOpc = Cse523::OR32rr;  return Cse523::OR32rr;
//        case Cse523::ATOMXOR6432:  HiOpc = Cse523::XOR32rr; return Cse523::XOR32rr;
//        case Cse523::ATOMADD6432:  HiOpc = Cse523::ADC32rr; return Cse523::ADD32rr;
//        case Cse523::ATOMSUB6432:  HiOpc = Cse523::SBB32rr; return Cse523::SUB32rr;
//        case Cse523::ATOMSWAP6432: HiOpc = Cse523::MOV32rr; return Cse523::MOV32rr;
//        case Cse523::ATOMMAX6432:  HiOpc = Cse523::SETLr;   return Cse523::SETLr;
//        case Cse523::ATOMMIN6432:  HiOpc = Cse523::SETGr;   return Cse523::SETGr;
//        case Cse523::ATOMUMAX6432: HiOpc = Cse523::SETBr;   return Cse523::SETBr;
//        case Cse523::ATOMUMIN6432: HiOpc = Cse523::SETAr;   return Cse523::SETAr;
//    }
    llvm_unreachable("Unhandled atomic-load-op opcode!");
}

// Get opcode of the non-atomic one from the specified atomic instruction for
// 64-bit data type on 32-bit target with extra opcode.
static unsigned getNonAtomic6432OpcodeWithExtraOpc(unsigned Opc,
        unsigned &HiOpc,
        unsigned &ExtraOpc) {
//    switch (Opc) {
//        case Cse523::ATOMNAND6432:
//            ExtraOpc = Cse523::NOT32r;
//            HiOpc = Cse523::AND32rr;
//            return Cse523::AND32rr;
//    }
    llvm_unreachable("Unhandled atomic-load-op opcode!");
}

// Get pseudo CMOV opcode from the specified data type.
static unsigned getPseudoCMOVOpc(EVT VT) {
    if (VT.getSimpleVT().SimpleTy != MVT::i64)
        llvm_unreachable("Unknown CMOV opcode!");

    return Cse523::CMOV_GR64;
}

// EmitAtomicLoadArith - emit the code sequence for pseudo atomic instructions.
// They will be translated into a spin-loop or compare-exchange loop from
//
//    ...
//    dst = atomic-fetch-op MI.addr, MI.val
//    ...
//
// to
//
//    ...
//    t1 = LOAD MI.addr
// loop:
//    t4 = phi(t1, t3 / loop)
//    t2 = OP MI.val, t4
//    EAX = t4
//    LCMPXCHG [MI.addr], t2, [EAX is implicitly used & defined]
//    t3 = EAX
//    JNE loop
// sink:
//    dst = t3
//    ...
MachineBasicBlock *
Cse523TargetLowering::EmitAtomicLoadArith(MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    MachineFunction *MF = MBB->getParent();
    MachineRegisterInfo &MRI = MF->getRegInfo();

    const BasicBlock *BB = MBB->getBasicBlock();
    MachineFunction::iterator I = MBB;
    ++I;

    assert(MI->getNumOperands() <= Cse523::AddrNumOperands + 4 &&
            "Unexpected number of operands");

    assert(MI->hasOneMemOperand() &&
            "Expected atomic-load-op to have one memoperand");

    // Memory Reference
    MachineInstr::mmo_iterator MMOBegin = MI->memoperands_begin();
    MachineInstr::mmo_iterator MMOEnd = MI->memoperands_end();

    unsigned DstReg, SrcReg;
    unsigned MemOpndSlot;

    unsigned CurOp = 0;

    DstReg = MI->getOperand(CurOp++).getReg();
    MemOpndSlot = CurOp;
    CurOp += Cse523::AddrNumOperands;
    SrcReg = MI->getOperand(CurOp++).getReg();

    const TargetRegisterClass *RC = MRI.getRegClass(DstReg);
    MVT::SimpleValueType VT = *RC->vt_begin();
    unsigned t1 = MRI.createVirtualRegister(RC);
    unsigned t2 = MRI.createVirtualRegister(RC);
    unsigned t3 = MRI.createVirtualRegister(RC);
    unsigned t4 = MRI.createVirtualRegister(RC);
    unsigned PhyReg = getCse523SubSuperRegister(Cse523::RAX, VT); //TODO: Replaced

    unsigned LCMPXCHGOpc = getCmpXChgOpcode(VT);
    unsigned LOADOpc = getLoadOpcode(VT);

    // For the atomic load-arith operator, we generate
    //
    //  thisMBB:
    //    t1 = LOAD [MI.addr]
    //  mainMBB:
    //    t4 = phi(t1 / thisMBB, t3 / mainMBB)
    //    t1 = OP MI.val, EAX
    //    EAX = t4
    //    LCMPXCHG [MI.addr], t1, [EAX is implicitly used & defined]
    //    t3 = EAX
    //    JNE mainMBB
    //  sinkMBB:
    //    dst = t3

    MachineBasicBlock *thisMBB = MBB;
    MachineBasicBlock *mainMBB = MF->CreateMachineBasicBlock(BB);
    MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(BB);
    MF->insert(I, mainMBB);
    MF->insert(I, sinkMBB);

    MachineInstrBuilder MIB;

    // Transfer the remainder of BB and its successor edges to sinkMBB.
    sinkMBB->splice(sinkMBB->begin(), MBB,
            llvm::next(MachineBasicBlock::iterator(MI)), MBB->end());
    sinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

    // thisMBB:
    MIB = BuildMI(thisMBB, DL, TII->get(LOADOpc), t1);
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
        MachineOperand NewMO = MI->getOperand(MemOpndSlot + i);
        if (NewMO.isReg())
            NewMO.setIsKill(false);
        MIB.addOperand(NewMO);
    }
    for (MachineInstr::mmo_iterator MMOI = MMOBegin; MMOI != MMOEnd; ++MMOI) {
        unsigned flags = (*MMOI)->getFlags();
        flags = (flags & ~MachineMemOperand::MOStore) | MachineMemOperand::MOLoad;
        MachineMemOperand *MMO =
            MF->getMachineMemOperand((*MMOI)->getPointerInfo(), flags,
                    (*MMOI)->getSize(),
                    (*MMOI)->getBaseAlignment(),
                    (*MMOI)->getTBAAInfo(),
                    (*MMOI)->getRanges());
        MIB.addMemOperand(MMO);
    }

    thisMBB->addSuccessor(mainMBB);

    // mainMBB:
    MachineBasicBlock *origMainMBB = mainMBB;

    // Add a PHI.
    MachineInstr *Phi = BuildMI(mainMBB, DL, TII->get(Cse523::PHI), t4)
        .addReg(t1).addMBB(thisMBB).addReg(t3).addMBB(mainMBB);

    unsigned Opc = MI->getOpcode();
    switch (Opc) {
        default:
            llvm_unreachable("Unhandled atomic-load-op opcode!");
//        case Cse523::ATOMAND8:
//        case Cse523::ATOMAND16:
//        case Cse523::ATOMAND32:
//        case Cse523::ATOMAND64:
//        case Cse523::ATOMOR8:
//        case Cse523::ATOMOR16:
//        case Cse523::ATOMOR32:
//        case Cse523::ATOMOR64:
//        case Cse523::ATOMXOR8:
//        case Cse523::ATOMXOR16:
//        case Cse523::ATOMXOR32:
//        case Cse523::ATOMXOR64: {
//                                    unsigned ARITHOpc = getNonAtomicOpcode(Opc);
//                                    BuildMI(mainMBB, DL, TII->get(ARITHOpc), t2).addReg(SrcReg)
//                                        .addReg(t4);
//                                    break;
//                                }
//        case Cse523::ATOMNAND8:
//        case Cse523::ATOMNAND16:
//        case Cse523::ATOMNAND32:
//        case Cse523::ATOMNAND64: {
//                                     unsigned Tmp = MRI.createVirtualRegister(RC);
//                                     unsigned NOTOpc;
//                                     unsigned ANDOpc = getNonAtomicOpcodeWithExtraOpc(Opc, NOTOpc);
//                                     BuildMI(mainMBB, DL, TII->get(ANDOpc), Tmp).addReg(SrcReg)
//                                         .addReg(t4);
//                                     BuildMI(mainMBB, DL, TII->get(NOTOpc), t2).addReg(Tmp);
//                                     break;
//                                 }
//        case Cse523::ATOMMAX8:
//        case Cse523::ATOMMAX16:
//        case Cse523::ATOMMAX32:
//        case Cse523::ATOMMAX64:
//        case Cse523::ATOMMIN8:
//        case Cse523::ATOMMIN16:
//        case Cse523::ATOMMIN32:
//        case Cse523::ATOMMIN64:
//        case Cse523::ATOMUMAX8:
//        case Cse523::ATOMUMAX16:
//        case Cse523::ATOMUMAX32:
//        case Cse523::ATOMUMAX64:
//        case Cse523::ATOMUMIN8:
//        case Cse523::ATOMUMIN16:
//        case Cse523::ATOMUMIN32:
//        case Cse523::ATOMUMIN64: {
//                                     unsigned CMPOpc;
//                                     unsigned CMOVOpc = getNonAtomicOpcodeWithExtraOpc(Opc, CMPOpc);
//
//                                     BuildMI(mainMBB, DL, TII->get(CMPOpc))
//                                         .addReg(SrcReg)
//                                         .addReg(t4);
//
//                                     if (Subtarget->hasCMov()) {
//                                         if (VT != MVT::i8) {
//                                             // Native support
//                                             BuildMI(mainMBB, DL, TII->get(CMOVOpc), t2)
//                                                 .addReg(SrcReg)
//                                                 .addReg(t4);
//                                         } else {
//                                             // Promote i8 to i32 to use CMOV32
//                                             const TargetRegisterInfo* TRI = getTargetMachine().getRegisterInfo();
//                                             const TargetRegisterClass *RC32 =
//                                                 TRI->getSubClassWithSubReg(getRegClassFor(MVT::i32), Cse523::sub_8bit);
//                                             unsigned SrcReg32 = MRI.createVirtualRegister(RC32);
//                                             unsigned AccReg32 = MRI.createVirtualRegister(RC32);
//                                             unsigned Tmp = MRI.createVirtualRegister(RC32);
//
//                                             unsigned Undef = MRI.createVirtualRegister(RC32);
//                                             BuildMI(mainMBB, DL, TII->get(TargetOpcode::IMPLICIT_DEF), Undef);
//
//                                             BuildMI(mainMBB, DL, TII->get(TargetOpcode::INSERT_SUBREG), SrcReg32)
//                                                 .addReg(Undef)
//                                                 .addReg(SrcReg)
//                                                 .addImm(Cse523::sub_8bit);
//                                             BuildMI(mainMBB, DL, TII->get(TargetOpcode::INSERT_SUBREG), AccReg32)
//                                                 .addReg(Undef)
//                                                 .addReg(t4)
//                                                 .addImm(Cse523::sub_8bit);
//
//                                             BuildMI(mainMBB, DL, TII->get(CMOVOpc), Tmp)
//                                                 .addReg(SrcReg32)
//                                                 .addReg(AccReg32);
//
//                                             BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), t2)
//                                                 .addReg(Tmp, 0, Cse523::sub_8bit);
//                                         }
//                                     } else {
//                                         // Use pseudo select and lower them.
//                                         assert((VT == MVT::i8 || VT == MVT::i16 || VT == MVT::i32) &&
//                                                 "Invalid atomic-load-op transformation!");
//                                         unsigned SelOpc = getPseudoCMOVOpc(VT);
//                                         Cse523::CondCode CC = Cse523::getCondFromCMovOpc(CMOVOpc);
//                                         assert(CC != Cse523::COND_INVALID && "Invalid atomic-load-op transformation!");
//                                         MIB = BuildMI(mainMBB, DL, TII->get(SelOpc), t2)
//                                             .addReg(SrcReg).addReg(t4)
//                                             .addImm(CC);
//                                         mainMBB = EmitLoweredSelect(MIB, mainMBB);
//                                         // Replace the original PHI node as mainMBB is changed after CMOV
//                                         // lowering.
//                                         BuildMI(*origMainMBB, Phi, DL, TII->get(Cse523::PHI), t4)
//                                             .addReg(t1).addMBB(thisMBB).addReg(t3).addMBB(mainMBB);
//                                         Phi->eraseFromParent();
//                                     }
//                                     break;
//                                 }
    }

    // Copy PhyReg back from virtual register.
    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), PhyReg)
        .addReg(t4);

    MIB = BuildMI(mainMBB, DL, TII->get(LCMPXCHGOpc));
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
        MachineOperand NewMO = MI->getOperand(MemOpndSlot + i);
        if (NewMO.isReg())
            NewMO.setIsKill(false);
        MIB.addOperand(NewMO);
    }
    MIB.addReg(t2);
    MIB.setMemRefs(MMOBegin, MMOEnd);

    // Copy PhyReg back to virtual register.
    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), t3)
        .addReg(PhyReg);

    BuildMI(mainMBB, DL, TII->get(Cse523::JNE_4)).addMBB(origMainMBB);

    mainMBB->addSuccessor(origMainMBB);
    mainMBB->addSuccessor(sinkMBB);

    // sinkMBB:
    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
            TII->get(TargetOpcode::COPY), DstReg)
        .addReg(t3);

    MI->eraseFromParent();
    return sinkMBB;
}

// EmitAtomicLoadArith6432 - emit the code sequence for pseudo atomic
// instructions. They will be translated into a spin-loop or compare-exchange
// loop from
//
//    ...
//    dst = atomic-fetch-op MI.addr, MI.val
//    ...
//
// to
//
//    ...
//    t1L = LOAD [MI.addr + 0]
//    t1H = LOAD [MI.addr + 4]
// loop:
//    t4L = phi(t1L, t3L / loop)
//    t4H = phi(t1H, t3H / loop)
//    t2L = OP MI.val.lo, t4L
//    t2H = OP MI.val.hi, t4H
//    EAX = t4L
//    EDX = t4H
//    EBX = t2L
//    ECX = t2H
//    LCMPXCHG8B [MI.addr], [ECX:EBX & EDX:EAX are implicitly used and EDX:EAX is implicitly defined]
//    t3L = EAX
//    t3H = EDX
//    JNE loop
// sink:
//    dstL = t3L
//    dstH = t3H
//    ...
MachineBasicBlock *
Cse523TargetLowering::EmitAtomicLoadArith6432(MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    MachineFunction *MF = MBB->getParent();
    MachineRegisterInfo &MRI = MF->getRegInfo();

    const BasicBlock *BB = MBB->getBasicBlock();
    MachineFunction::iterator I = MBB;
    ++I;

    assert(MI->getNumOperands() <= Cse523::AddrNumOperands + 7 &&
            "Unexpected number of operands");

    assert(MI->hasOneMemOperand() &&
            "Expected atomic-load-op32 to have one memoperand");

    // Memory Reference
    MachineInstr::mmo_iterator MMOBegin = MI->memoperands_begin();
    MachineInstr::mmo_iterator MMOEnd = MI->memoperands_end();

    unsigned DstLoReg, DstHiReg;
    unsigned SrcLoReg, SrcHiReg;
    unsigned MemOpndSlot;

    unsigned CurOp = 0;

    DstLoReg = MI->getOperand(CurOp++).getReg();
    DstHiReg = MI->getOperand(CurOp++).getReg();
    MemOpndSlot = CurOp;
    CurOp += Cse523::AddrNumOperands;
    SrcLoReg = MI->getOperand(CurOp++).getReg();
    SrcHiReg = MI->getOperand(CurOp++).getReg();

    assert(0);

//    const TargetRegisterClass *RC = &Cse523::GR32RegClass;
//    const TargetRegisterClass *RC8 = &Cse523::GR8RegClass;
//
//    unsigned t1L = MRI.createVirtualRegister(RC);
//    unsigned t1H = MRI.createVirtualRegister(RC);
//    unsigned t2L = MRI.createVirtualRegister(RC);
//    unsigned t2H = MRI.createVirtualRegister(RC);
//    unsigned t3L = MRI.createVirtualRegister(RC);
//    unsigned t3H = MRI.createVirtualRegister(RC);
//    unsigned t4L = MRI.createVirtualRegister(RC);
//    unsigned t4H = MRI.createVirtualRegister(RC);
//
//    unsigned LCMPXCHGOpc = Cse523::LCMPXCHG8B;
//    unsigned LOADOpc = Cse523::MOV32rm;
//
//    // For the atomic load-arith operator, we generate
//    //
//    //  thisMBB:
//    //    t1L = LOAD [MI.addr + 0]
//    //    t1H = LOAD [MI.addr + 4]
//    //  mainMBB:
//    //    t4L = phi(t1L / thisMBB, t3L / mainMBB)
//    //    t4H = phi(t1H / thisMBB, t3H / mainMBB)
//    //    t2L = OP MI.val.lo, t4L
//    //    t2H = OP MI.val.hi, t4H
//    //    EBX = t2L
//    //    ECX = t2H
//    //    LCMPXCHG8B [MI.addr], [ECX:EBX & EDX:EAX are implicitly used and EDX:EAX is implicitly defined]
//    //    t3L = EAX
//    //    t3H = EDX
//    //    JNE loop
//    //  sinkMBB:
//    //    dstL = t3L
//    //    dstH = t3H
//
//    MachineBasicBlock *thisMBB = MBB;
//    MachineBasicBlock *mainMBB = MF->CreateMachineBasicBlock(BB);
    MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(BB);
//    MF->insert(I, mainMBB);
//    MF->insert(I, sinkMBB);
//
//    MachineInstrBuilder MIB;
//
//    // Transfer the remainder of BB and its successor edges to sinkMBB.
//    sinkMBB->splice(sinkMBB->begin(), MBB,
//            llvm::next(MachineBasicBlock::iterator(MI)), MBB->end());
//    sinkMBB->transferSuccessorsAndUpdatePHIs(MBB);
//
//    // thisMBB:
//    // Lo
//    MIB = BuildMI(thisMBB, DL, TII->get(LOADOpc), t1L);
//    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
//        MachineOperand NewMO = MI->getOperand(MemOpndSlot + i);
//        if (NewMO.isReg())
//            NewMO.setIsKill(false);
//        MIB.addOperand(NewMO);
//    }
//    for (MachineInstr::mmo_iterator MMOI = MMOBegin; MMOI != MMOEnd; ++MMOI) {
//        unsigned flags = (*MMOI)->getFlags();
//        flags = (flags & ~MachineMemOperand::MOStore) | MachineMemOperand::MOLoad;
//        MachineMemOperand *MMO =
//            MF->getMachineMemOperand((*MMOI)->getPointerInfo(), flags,
//                    (*MMOI)->getSize(),
//                    (*MMOI)->getBaseAlignment(),
//                    (*MMOI)->getTBAAInfo(),
//                    (*MMOI)->getRanges());
//        MIB.addMemOperand(MMO);
//    };
//    MachineInstr *LowMI = MIB;
//
//    // Hi
//    MIB = BuildMI(thisMBB, DL, TII->get(LOADOpc), t1H);
//    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
//        if (i == Cse523::AddrDisp) {
//            MIB.addDisp(MI->getOperand(MemOpndSlot + i), 4); // 4 == sizeof(i32)
//        } else {
//            MachineOperand NewMO = MI->getOperand(MemOpndSlot + i);
//            if (NewMO.isReg())
//                NewMO.setIsKill(false);
//            MIB.addOperand(NewMO);
//        }
//    }
//    MIB.setMemRefs(LowMI->memoperands_begin(), LowMI->memoperands_end());
//
//    thisMBB->addSuccessor(mainMBB);
//
//    // mainMBB:
//    MachineBasicBlock *origMainMBB = mainMBB;
//
//    // Add PHIs.
//    MachineInstr *PhiL = BuildMI(mainMBB, DL, TII->get(Cse523::PHI), t4L)
//        .addReg(t1L).addMBB(thisMBB).addReg(t3L).addMBB(mainMBB);
//    MachineInstr *PhiH = BuildMI(mainMBB, DL, TII->get(Cse523::PHI), t4H)
//        .addReg(t1H).addMBB(thisMBB).addReg(t3H).addMBB(mainMBB);
//
//    unsigned Opc = MI->getOpcode();
//    switch (Opc) {
//        default:
//            llvm_unreachable("Unhandled atomic-load-op6432 opcode!");
//        case Cse523::ATOMAND6432:
//        case Cse523::ATOMOR6432:
//        case Cse523::ATOMXOR6432:
//        case Cse523::ATOMADD6432:
//        case Cse523::ATOMSUB6432: {
//                                      unsigned HiOpc;
//                                      unsigned LoOpc = getNonAtomic6432Opcode(Opc, HiOpc);
//                                      BuildMI(mainMBB, DL, TII->get(LoOpc), t2L).addReg(t4L)
//                                          .addReg(SrcLoReg);
//                                      BuildMI(mainMBB, DL, TII->get(HiOpc), t2H).addReg(t4H)
//                                          .addReg(SrcHiReg);
//                                      break;
//                                  }
//        case Cse523::ATOMNAND6432: {
//                                       unsigned HiOpc, NOTOpc;
//                                       unsigned LoOpc = getNonAtomic6432OpcodeWithExtraOpc(Opc, HiOpc, NOTOpc);
//                                       unsigned TmpL = MRI.createVirtualRegister(RC);
//                                       unsigned TmpH = MRI.createVirtualRegister(RC);
//                                       BuildMI(mainMBB, DL, TII->get(LoOpc), TmpL).addReg(SrcLoReg)
//                                           .addReg(t4L);
//                                       BuildMI(mainMBB, DL, TII->get(HiOpc), TmpH).addReg(SrcHiReg)
//                                           .addReg(t4H);
//                                       BuildMI(mainMBB, DL, TII->get(NOTOpc), t2L).addReg(TmpL);
//                                       BuildMI(mainMBB, DL, TII->get(NOTOpc), t2H).addReg(TmpH);
//                                       break;
//                                   }
//        case Cse523::ATOMMAX6432:
//        case Cse523::ATOMMIN6432:
//        case Cse523::ATOMUMAX6432:
//        case Cse523::ATOMUMIN6432: {
//                                       unsigned HiOpc;
//                                       unsigned LoOpc = getNonAtomic6432Opcode(Opc, HiOpc);
//                                       unsigned cL = MRI.createVirtualRegister(RC8);
//                                       unsigned cH = MRI.createVirtualRegister(RC8);
//                                       unsigned cL32 = MRI.createVirtualRegister(RC);
//                                       unsigned cH32 = MRI.createVirtualRegister(RC);
//                                       unsigned cc = MRI.createVirtualRegister(RC);
//                                       // cl := cmp src_lo, lo
//                                       BuildMI(mainMBB, DL, TII->get(Cse523::CMP32rr))
//                                           .addReg(SrcLoReg).addReg(t4L);
//                                       BuildMI(mainMBB, DL, TII->get(LoOpc), cL);
//                                       BuildMI(mainMBB, DL, TII->get(Cse523::MOVZX32rr8), cL32).addReg(cL);
//                                       // ch := cmp src_hi, hi
//                                       BuildMI(mainMBB, DL, TII->get(Cse523::CMP32rr))
//                                           .addReg(SrcHiReg).addReg(t4H);
//                                       BuildMI(mainMBB, DL, TII->get(HiOpc), cH);
//                                       BuildMI(mainMBB, DL, TII->get(Cse523::MOVZX32rr8), cH32).addReg(cH);
//                                       // cc := if (src_hi == hi) ? cl : ch;
//                                       if (Subtarget->hasCMov()) {
//                                           BuildMI(mainMBB, DL, TII->get(Cse523::CMOVE32rr), cc)
//                                               .addReg(cH32).addReg(cL32);
//                                       } else {
//                                           MIB = BuildMI(mainMBB, DL, TII->get(Cse523::CMOV_GR32), cc)
//                                               .addReg(cH32).addReg(cL32)
//                                               .addImm(Cse523::COND_E);
//                                           mainMBB = EmitLoweredSelect(MIB, mainMBB);
//                                       }
//                                       BuildMI(mainMBB, DL, TII->get(Cse523::TEST32rr)).addReg(cc).addReg(cc);
//                                       if (Subtarget->hasCMov()) {
//                                           BuildMI(mainMBB, DL, TII->get(Cse523::CMOVNE32rr), t2L)
//                                               .addReg(SrcLoReg).addReg(t4L);
//                                           BuildMI(mainMBB, DL, TII->get(Cse523::CMOVNE32rr), t2H)
//                                               .addReg(SrcHiReg).addReg(t4H);
//                                       } else {
//                                           MIB = BuildMI(mainMBB, DL, TII->get(Cse523::CMOV_GR32), t2L)
//                                               .addReg(SrcLoReg).addReg(t4L)
//                                               .addImm(Cse523::COND_NE);
//                                           mainMBB = EmitLoweredSelect(MIB, mainMBB);
//                                           // As the lowered CMOV won't clobber EFLAGS, we could reuse it for the
//                                           // 2nd CMOV lowering.
//                                           mainMBB->addLiveIn(Cse523::EFLAGS);
//                                           MIB = BuildMI(mainMBB, DL, TII->get(Cse523::CMOV_GR32), t2H)
//                                               .addReg(SrcHiReg).addReg(t4H)
//                                               .addImm(Cse523::COND_NE);
//                                           mainMBB = EmitLoweredSelect(MIB, mainMBB);
//                                           // Replace the original PHI node as mainMBB is changed after CMOV
//                                           // lowering.
//                                           BuildMI(*origMainMBB, PhiL, DL, TII->get(Cse523::PHI), t4L)
//                                               .addReg(t1L).addMBB(thisMBB).addReg(t3L).addMBB(mainMBB);
//                                           BuildMI(*origMainMBB, PhiH, DL, TII->get(Cse523::PHI), t4H)
//                                               .addReg(t1H).addMBB(thisMBB).addReg(t3H).addMBB(mainMBB);
//                                           PhiL->eraseFromParent();
//                                           PhiH->eraseFromParent();
//                                       }
//                                       break;
//                                   }
//        case Cse523::ATOMSWAP6432: {
//                                       unsigned HiOpc;
//                                       unsigned LoOpc = getNonAtomic6432Opcode(Opc, HiOpc);
//                                       BuildMI(mainMBB, DL, TII->get(LoOpc), t2L).addReg(SrcLoReg);
//                                       BuildMI(mainMBB, DL, TII->get(HiOpc), t2H).addReg(SrcHiReg);
//                                       break;
//                                   }
//    }
//
//    // Copy EDX:EAX back from HiReg:LoReg
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), Cse523::EAX).addReg(t4L);
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), Cse523::EDX).addReg(t4H);
//    // Copy ECX:EBX from t1H:t1L
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), Cse523::EBX).addReg(t2L);
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), Cse523::ECX).addReg(t2H);
//
//    MIB = BuildMI(mainMBB, DL, TII->get(LCMPXCHGOpc));
//    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
//        MachineOperand NewMO = MI->getOperand(MemOpndSlot + i);
//        if (NewMO.isReg())
//            NewMO.setIsKill(false);
//        MIB.addOperand(NewMO);
//    }
//    MIB.setMemRefs(MMOBegin, MMOEnd);
//
//    // Copy EDX:EAX back to t3H:t3L
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), t3L).addReg(Cse523::EAX);
//    BuildMI(mainMBB, DL, TII->get(TargetOpcode::COPY), t3H).addReg(Cse523::EDX);
//
//    BuildMI(mainMBB, DL, TII->get(Cse523::JNE_4)).addMBB(origMainMBB);
//
//    mainMBB->addSuccessor(origMainMBB);
//    mainMBB->addSuccessor(sinkMBB);
//
//    // sinkMBB:
//    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
//            TII->get(TargetOpcode::COPY), DstLoReg)
//        .addReg(t3L);
//    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
//            TII->get(TargetOpcode::COPY), DstHiReg)
//        .addReg(t3H);
//
//    MI->eraseFromParent();
    return sinkMBB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitVAARG64WithCustomInserter(
        MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    assert(0);
    return NULL;
//    // Emit va_arg instruction on Cse523-64.
//
//    // Operands to this pseudo-instruction:
//    // 0  ) Output        : destination address (reg)
//    // 1-5) Input         : va_list address (addr, i64mem)
//    // 6  ) ArgSize       : Size (in bytes) of vararg type
//    // 7  ) ArgMode       : 0=overflow only, 1=use gp_offset, 2=use fp_offset
//    // 8  ) Align         : Alignment of type
//    // 9  ) EFLAGS (implicit-def)
//
//    assert(MI->getNumOperands() == 10 && "VAARG_64 should have 10 operands!");
//    assert(Cse523::AddrNumOperands == 5 && "VAARG_64 assumes 5 address operands");
//
//    unsigned DestReg = MI->getOperand(0).getReg();
//    machineOperand &Base = MI->getOperand(1);
//    machineOperand &Scale = MI->getOperand(2);
//    machineOperand &Index = MI->getOperand(3);
//    machineOperand &Disp = MI->getOperand(4);
//    machineOperand &Segment = MI->getOperand(5);
//    unsigned ArgSize = MI->getOperand(6).getImm();
//    unsigned ArgMode = MI->getOperand(7).getImm();
//    unsigned Align = MI->getOperand(8).getImm();
//
//    // Memory Reference
//    assert(MI->hasOneMemOperand() && "Expected VAARG_64 to have one memoperand");
//    machineInstr::mmo_iterator MMOBegin = MI->memoperands_begin();
//    machineInstr::mmo_iterator MMOEnd = MI->memoperands_end();
//
//    // Machine Information
//    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
//    machineRegisterInfo &MRI = MBB->getParent()->getRegInfo();
//    const TargetRegisterClass *AddrRegClass = getRegClassFor(MVT::i64);
//    const TargetRegisterClass *OffsetRegClass = getRegClassFor(MVT::i32);
//    debugLoc DL = MI->getDebugLoc();
//
//    // struct va_list {
//    //   i32   gp_offset
//    //   i32   fp_offset
//    //   i64   overflow_area (address)
//    //   i64   reg_save_area (address)
//    // }
//    // sizeof(va_list) = 24
//    // alignment(va_list) = 8
//
//    unsigned TotalNumIntRegs = 6;
//    unsigned TotalNumXMMRegs = 8;
//    bool UseGPOffset = (ArgMode == 1);
//    bool UseFPOffset = (ArgMode == 2);
//    unsigned MaxOffset = TotalNumIntRegs * 8 +
//        (UseFPOffset ? TotalNumXMMRegs * 16 : 0);
//
//    /* Align ArgSize to a multiple of 8 */
//    unsigned ArgSizeA8 = (ArgSize + 7) & ~7;
//    bool NeedsAlign = (Align > 8);
//
//    machineBasicBlock *thisMBB = MBB;
//    machineBasicBlock *overflowMBB;
//    machineBasicBlock *offsetMBB;
//    machineBasicBlock *endMBB;
//
//    unsigned OffsetDestReg = 0;    // Argument address computed by offsetMBB
//    unsigned OverflowDestReg = 0;  // Argument address computed by overflowMBB
//    unsigned OffsetReg = 0;
//
//    if (!UseGPOffset && !UseFPOffset) {
//        // If we only pull from the overflow region, we don't create a branch.
//        // We don't need to alter control flow.
//        OffsetDestReg = 0; // unused
//        OverflowDestReg = DestReg;
//
//        offsetMBB = NULL;
//        overflowMBB = thisMBB;
//        endMBB = thisMBB;
//    } else {
//        // First emit code to check if gp_offset (or fp_offset) is below the bound.
//        // If so, pull the argument from reg_save_area. (branch to offsetMBB)
//        // If not, pull from overflow_area. (branch to overflowMBB)
//        //
//        //       thisMBB
//        //         |     .
//        //         |        .
//        //     offsetMBB   overflowMBB
//        //         |        .
//        //         |     .
//        //        endMBB
//
//        // Registers for the PHI in endMBB
//        OffsetDestReg = MRI.createVirtualRegister(AddrRegClass);
//        OverflowDestReg = MRI.createVirtualRegister(AddrRegClass);
//
//        const BasicBlock *LLVM_BB = MBB->getBasicBlock();
//        MachineFunction *MF = MBB->getParent();
//        overflowMBB = MF->CreateMachineBasicBlock(LLVM_BB);
//        offsetMBB = MF->CreateMachineBasicBlock(LLVM_BB);
//        endMBB = MF->CreateMachineBasicBlock(LLVM_BB);
//
//        MachineFunction::iterator MBBIter = MBB;
//        ++MBBIter;
//
//        // Insert the new basic blocks
//        MF->insert(MBBIter, offsetMBB);
//        MF->insert(MBBIter, overflowMBB);
//        MF->insert(MBBIter, endMBB);
//
//        // Transfer the remainder of MBB and its successor edges to endMBB.
//        endMBB->splice(endMBB->begin(), thisMBB,
//                llvm::next(MachineBasicBlock::iterator(MI)),
//                thisMBB->end());
//        endMBB->transferSuccessorsAndUpdatePHIs(thisMBB);
//
//        // Make offsetMBB and overflowMBB successors of thisMBB
//        thisMBB->addSuccessor(offsetMBB);
//        thisMBB->addSuccessor(overflowMBB);
//
//        // endMBB is a successor of both offsetMBB and overflowMBB
//        offsetMBB->addSuccessor(endMBB);
//        overflowMBB->addSuccessor(endMBB);
//
//        // Load the offset value into a register
//        OffsetReg = MRI.createVirtualRegister(OffsetRegClass);
//        BuildMI(thisMBB, DL, TII->get(Cse523::MOV32rm), OffsetReg)
//            .addOperand(Base)
//            .addOperand(Scale)
//            .addOperand(Index)
//            .addDisp(Disp, UseFPOffset ? 4 : 0)
//            .addOperand(Segment)
//            .setMemRefs(MMOBegin, MMOEnd);
//
//        // Check if there is enough room left to pull this argument.
//        BuildMI(thisMBB, DL, TII->get(Cse523::CMP32ri))
//            .addReg(OffsetReg)
//            .addImm(MaxOffset + 8 - ArgSizeA8);
//
//        // Branch to "overflowMBB" if offset >= max
//        // Fall through to "offsetMBB" otherwise
//        BuildMI(thisMBB, DL, TII->get(Cse523::GetCondBranchFromCond(Cse523::COND_AE)))
//            .addMBB(overflowMBB);
//    }
//
//    // In offsetMBB, emit code to use the reg_save_area.
//    if (offsetMBB) {
//        assert(OffsetReg != 0);
//
//        // Read the reg_save_area address.
//        unsigned RegSaveReg = MRI.createVirtualRegister(AddrRegClass);
//        BuildMI(offsetMBB, DL, TII->get(Cse523::MOV64rm), RegSaveReg)
//            .addOperand(Base)
//            .addOperand(Scale)
//            .addOperand(Index)
//            .addDisp(Disp, 16)
//            .addOperand(Segment)
//            .setMemRefs(MMOBegin, MMOEnd);
//
//        // Zero-extend the offset
//        unsigned OffsetReg64 = MRI.createVirtualRegister(AddrRegClass);
//        BuildMI(offsetMBB, DL, TII->get(Cse523::SUBREG_TO_REG), OffsetReg64)
//            .addImm(0)
//            .addReg(OffsetReg)
//            .addImm(Cse523::sub_32bit);
//
//        // Add the offset to the reg_save_area to get the final address.
//        BuildMI(offsetMBB, DL, TII->get(Cse523::ADD64rr), OffsetDestReg)
//            .addReg(OffsetReg64)
//            .addReg(RegSaveReg);
//
//        // Compute the offset for the next argument
//        unsigned NextOffsetReg = MRI.createVirtualRegister(OffsetRegClass);
//        BuildMI(offsetMBB, DL, TII->get(Cse523::ADD32ri), NextOffsetReg)
//            .addReg(OffsetReg)
//            .addImm(UseFPOffset ? 16 : 8);
//
//        // Store it back into the va_list.
//        BuildMI(offsetMBB, DL, TII->get(Cse523::MOV32mr))
//            .addOperand(Base)
//            .addOperand(Scale)
//            .addOperand(Index)
//            .addDisp(Disp, UseFPOffset ? 4 : 0)
//            .addOperand(Segment)
//            .addReg(NextOffsetReg)
//            .setMemRefs(MMOBegin, MMOEnd);
//
//        // Jump to endMBB
//        BuildMI(offsetMBB, DL, TII->get(Cse523::JMP_4))
//            .addMBB(endMBB);
//    }
//
//    //
//    // Emit code to use overflow area
//    //
//
//    // Load the overflow_area address into a register.
//    unsigned OverflowAddrReg = MRI.createVirtualRegister(AddrRegClass);
//    buildMI(overflowMBB, DL, TII->get(Cse523::MOV64rm), OverflowAddrReg)
//        .addOperand(Base)
//        .addOperand(Scale)
//        .addOperand(Index)
//        .addDisp(Disp, 8)
//        .addOperand(Segment)
//        .setMemRefs(MMOBegin, MMOEnd);
//
//    // If we need to align it, do so. Otherwise, just copy the address
//    // to OverflowDestReg.
//    if (NeedsAlign) {
//        // Align the overflow address
//        assert((Align & (Align-1)) == 0 && "Alignment must be a power of 2");
//        unsigned TmpReg = MRI.createVirtualRegister(AddrRegClass);
//
//        // aligned_addr = (addr + (align-1)) & ~(align-1)
//        BuildMI(overflowMBB, DL, TII->get(Cse523::ADD64ri32), TmpReg)
//            .addReg(OverflowAddrReg)
//            .addImm(Align-1);
//
//        BuildMI(overflowMBB, DL, TII->get(Cse523::AND64ri32), OverflowDestReg)
//            .addReg(TmpReg)
//            .addImm(~(uint64_t)(Align-1));
//    } else {
//        BuildMI(overflowMBB, DL, TII->get(TargetOpcode::COPY), OverflowDestReg)
//            .addReg(OverflowAddrReg);
//    }
//
//    // Compute the next overflow address after this argument.
//    // (the overflow address should be kept 8-byte aligned)
//    unsigned NextAddrReg = MRI.createVirtualRegister(AddrRegClass);
//    buildMI(overflowMBB, DL, TII->get(Cse523::ADD64ri32), NextAddrReg)
//        .addReg(OverflowDestReg)
//        .addImm(ArgSizeA8);
//
//    // Store the new overflow address.
//    buildMI(overflowMBB, DL, TII->get(Cse523::MOV64mr))
//        .addOperand(Base)
//        .addOperand(Scale)
//        .addOperand(Index)
//        .addDisp(Disp, 8)
//        .addOperand(Segment)
//        .addReg(NextAddrReg)
//        .setMemRefs(MMOBegin, MMOEnd);
//
//    // If we branched, emit the PHI to the front of endMBB.
//    if (offsetMBB) {
//        BuildMI(*endMBB, endMBB->begin(), DL,
//                TII->get(Cse523::PHI), DestReg)
//            .addReg(OffsetDestReg).addMBB(offsetMBB)
//            .addReg(OverflowDestReg).addMBB(overflowMBB);
//    }
//
//    // Erase the pseudo instruction
//    mI->eraseFromParent();
//
//    return endMBB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitVAStartSaveXMMRegsWithCustomInserter(
        MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    // Emit code to save XMM registers to the stack. The ABI says that the
    // number of registers to save is given in %al, so it's theoretically
    // possible to do an indirect jump trick to avoid saving all of them,
    // however this code takes a simpler approach and just executes all
    // of the stores if %al is non-zero. It's less code, and it's probably
    // easier on the hardware branch predictor, and stores aren't all that
    // expensive anyway.

    // Create the new basic blocks. One block contains all the XMM stores,
    // and one block is the final destination regardless of whether any
    // stores were performed.
    const BasicBlock *LLVM_BB = MBB->getBasicBlock();
    MachineFunction *F = MBB->getParent();
    MachineFunction::iterator MBBIter = MBB;
    ++MBBIter;
    MachineBasicBlock *XMMSaveMBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *EndMBB = F->CreateMachineBasicBlock(LLVM_BB);
    F->insert(MBBIter, XMMSaveMBB);
    F->insert(MBBIter, EndMBB);

    // Transfer the remainder of MBB and its successor edges to EndMBB.
    EndMBB->splice(EndMBB->begin(), MBB,
            llvm::next(MachineBasicBlock::iterator(MI)),
            MBB->end());
    EndMBB->transferSuccessorsAndUpdatePHIs(MBB);

    // The original block will now fall through to the XMM save block.
    MBB->addSuccessor(XMMSaveMBB);
    // The XMMSaveMBB will fall through to the end block.
    XMMSaveMBB->addSuccessor(EndMBB);

    // Now add the instructions.
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    unsigned CountReg = MI->getOperand(0).getReg();
    int64_t RegSaveFrameIndex = MI->getOperand(1).getImm();
    int64_t VarArgsFPOffset = MI->getOperand(2).getImm();

    if (!Subtarget->isTargetWin64()) {
        assert(0);
        // If %al is 0, branch around the XMM save block.
        //BuildMI(MBB, DL, TII->get(Cse523::TEST8rr)).addReg(CountReg).addReg(CountReg);
        //BuildMI(MBB, DL, TII->get(Cse523::JE_4)).addMBB(EndMBB);
        MBB->addSuccessor(EndMBB);
    }

    // Make sure the last operand is EFLAGS, which gets clobbered by the branch
    // that was just emitted, but clearly shouldn't be "saved".
    assert((MI->getNumOperands() <= 3 ||
                !MI->getOperand(MI->getNumOperands() - 1).isReg() ||
                MI->getOperand(MI->getNumOperands() - 1).getReg() == Cse523::EFLAGS)
            && "Expected last argument to be EFLAGS");
    unsigned MOVOpc = 10;//Cse523::MOVAPSmr;
    // In the XMM save block, save all the XMM argument registers.
    for (int i = 3, e = MI->getNumOperands() - 1; i != e; ++i) {
        int64_t Offset = (i - 3) * 16 + VarArgsFPOffset;
        MachineMemOperand *MMO =
            F->getMachineMemOperand(
                    MachinePointerInfo::getFixedStack(RegSaveFrameIndex, Offset),
                    MachineMemOperand::MOStore,
                    /*Size=*/16, /*Align=*/16);
        BuildMI(XMMSaveMBB, DL, TII->get(MOVOpc))
            .addFrameIndex(RegSaveFrameIndex)
            .addImm(/*Scale=*/1)
            .addReg(/*IndexReg=*/0)
            .addImm(/*Disp=*/Offset)
            .addReg(/*Segment=*/0)
            .addReg(MI->getOperand(i).getReg())
            .addMemOperand(MMO);
    }

    MI->eraseFromParent();   // The pseudo instruction is gone now.

    return EndMBB;
}

// The EFLAGS operand of SelectItr might be missing a kill marker
// because there were multiple uses of EFLAGS, and ISel didn't know
// which to mark. Figure out whether SelectItr should have had a
// kill marker, and set it if it should. Returns the correct kill
// marker value.
static bool checkAndUpdateEFLAGSKill(MachineBasicBlock::iterator SelectItr,
        MachineBasicBlock* BB,
        const TargetRegisterInfo* TRI) {
    // Scan forward through BB for a use/def of EFLAGS.
    MachineBasicBlock::iterator miI(llvm::next(SelectItr));
    for (MachineBasicBlock::iterator miE = BB->end(); miI != miE; ++miI) {
        const MachineInstr& mi = *miI;
        if (mi.readsRegister(Cse523::EFLAGS))
            return false;
        if (mi.definesRegister(Cse523::EFLAGS))
            break; // Should have kill-flag - update below.
    }

    // If we hit the end of the block, check whether EFLAGS is live into a
    // successor.
    if (miI == BB->end()) {
        for (MachineBasicBlock::succ_iterator sItr = BB->succ_begin(),
                sEnd = BB->succ_end();
                sItr != sEnd; ++sItr) {
            MachineBasicBlock* succ = *sItr;
            if (succ->isLiveIn(Cse523::EFLAGS))
                return false;
        }
    }

    // We found a def, or hit the end of the basic block and EFLAGS wasn't live
    // out. SelectMI should have a kill flag on EFLAGS.
    SelectItr->addRegisterKilled(Cse523::EFLAGS, TRI);
    return true;
}

MachineBasicBlock *
Cse523TargetLowering::EmitLoweredSetCC(MachineInstr *MI,
        MachineBasicBlock *BB) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    // To "insert" a SELECT_CC instruction, we actually have to insert the
    // diamond control-flow pattern.  The incoming instruction knows the
    // destination vreg to set, the condition code register to branch on, the
    // true/false values to select between, and a branch opcode to use.
    const BasicBlock *LLVM_BB = BB->getBasicBlock();
    MachineFunction::iterator It = BB;
    ++It;

    /*
     * Create below basic blocks to convert SETCC to JCC+MOVs
     *
     *               thisMBB
     *            (MOV64 V0, 0) 
     *            (MOV64 V1, 1)
     *                (JCC)
     *  (On False)______|_____(On true)
     *            |           |
     *            |           |
     *        copy0MBB     copy1MBB
     *            |           |
     *            |___________|
     *                  |
     *               sinkMBB
     *  (Dst = PHI (V0:Copy0MBB, V1:Copy1MBB))
     */
    MachineBasicBlock *thisMBB = BB;
    MachineFunction *F = BB->getParent();
    MachineBasicBlock *copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *copy1MBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *sinkMBB = F->CreateMachineBasicBlock(LLVM_BB);

    MachineRegisterInfo &MRI = thisMBB->getParent()->getRegInfo();
    const TargetRegisterClass *RC = getRegClassFor(MVT::i64);
    unsigned copy0DstReg = MRI.createVirtualRegister(RC);
    unsigned copy1DstReg = MRI.createVirtualRegister(RC);

    F->insert(It, copy0MBB);
    F->insert(It, copy1MBB);
    F->insert(It, sinkMBB);

    // If the EFLAGS register isn't dead in the terminator, then claim that it's
    // live into the sink and copy blocks.
    const TargetRegisterInfo* TRI = getTargetMachine().getRegisterInfo();
    if (!MI->killsRegister(Cse523::EFLAGS) &&
            !checkAndUpdateEFLAGSKill(MI, BB, TRI)) {
        copy0MBB->addLiveIn(Cse523::EFLAGS);
        copy1MBB->addLiveIn(Cse523::EFLAGS);
        sinkMBB->addLiveIn(Cse523::EFLAGS);
    }

    // Transfer the remainder of BB and its successor edges to sinkMBB.
    sinkMBB->splice(sinkMBB->begin(), BB,
            llvm::next(MachineBasicBlock::iterator(MI)),
            BB->end());
    sinkMBB->transferSuccessorsAndUpdatePHIs(BB);

    // Add the true and fallthrough blocks as its successors.
    BB->addSuccessor(copy0MBB);
    BB->addSuccessor(copy1MBB);

    // Create MOVs analogous to SETCC
    BuildMI(BB, DL, TII->get(Cse523::MOV64ri), copy0DstReg).addImm(0);
    BuildMI(BB, DL, TII->get(Cse523::MOV64ri), copy1DstReg).addImm(1);

    // Create the conditional branch instruction.
    unsigned Opc = Cse523::GetCondBranchFromCond((Cse523::CondCode)MI->getOperand(1).getImm());
    BuildMI(BB, DL, TII->get(Opc)).addMBB(copy1MBB);

    copy0MBB->addSuccessor(sinkMBB);
    copy1MBB->addSuccessor(sinkMBB);

    //  sinkMBB:
    //   %Result = phi [copy0DstReg, copy0MBB], [copy1DstReg, copy1MBB]
    //  ...
    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
            TII->get(Cse523::PHI), MI->getOperand(0).getReg())
        .addReg(copy0DstReg).addMBB(copy0MBB)
        .addReg(copy1DstReg).addMBB(copy1MBB);

    MI->eraseFromParent();   // The pseudo instruction is gone now.
    return sinkMBB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitLoweredSelect(MachineInstr *MI,
        MachineBasicBlock *BB) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    // To "insert" a SELECT_CC instruction, we actually have to insert the
    // diamond control-flow pattern.  The incoming instruction knows the
    // destination vreg to set, the condition code register to branch on, the
    // true/false values to select between, and a branch opcode to use.
    const BasicBlock *LLVM_BB = BB->getBasicBlock();
    MachineFunction::iterator It = BB;
    ++It;

    //  thisMBB:
    //  ...
    //   TrueVal = ...
    //   cmpTY ccX, r1, r2
    //   bCC copy1MBB
    //   fallthrough --> copy0MBB
    MachineBasicBlock *thisMBB = BB;
    MachineFunction *F = BB->getParent();
    MachineBasicBlock *copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *sinkMBB = F->CreateMachineBasicBlock(LLVM_BB);
    F->insert(It, copy0MBB);
    F->insert(It, sinkMBB);

    // If the EFLAGS register isn't dead in the terminator, then claim that it's
    // live into the sink and copy blocks.
    const TargetRegisterInfo* TRI = getTargetMachine().getRegisterInfo();
    if (!MI->killsRegister(Cse523::EFLAGS) &&
            !checkAndUpdateEFLAGSKill(MI, BB, TRI)) {
        copy0MBB->addLiveIn(Cse523::EFLAGS);
        sinkMBB->addLiveIn(Cse523::EFLAGS);
    }

    // Transfer the remainder of BB and its successor edges to sinkMBB.
    sinkMBB->splice(sinkMBB->begin(), BB,
            llvm::next(MachineBasicBlock::iterator(MI)),
            BB->end());
    sinkMBB->transferSuccessorsAndUpdatePHIs(BB);

    // Add the true and fallthrough blocks as its successors.
    BB->addSuccessor(copy0MBB);
    BB->addSuccessor(sinkMBB);

    // Create the conditional branch instruction.
    unsigned Opc =
        Cse523::GetCondBranchFromCond((Cse523::CondCode)MI->getOperand(3).getImm());
    BuildMI(BB, DL, TII->get(Opc)).addMBB(sinkMBB);

    //  copy0MBB:
    //   %FalseValue = ...
    //   # fallthrough to sinkMBB
    copy0MBB->addSuccessor(sinkMBB);

    //  sinkMBB:
    //   %Result = phi [ %FalseValue, copy0MBB ], [ %TrueValue, thisMBB ]
    //  ...
    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
            TII->get(Cse523::PHI), MI->getOperand(0).getReg())
        .addReg(MI->getOperand(1).getReg()).addMBB(copy0MBB)
        .addReg(MI->getOperand(2).getReg()).addMBB(thisMBB);

    MI->eraseFromParent();   // The pseudo instruction is gone now.
    return sinkMBB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitLoweredSegAlloca(MachineInstr *MI, MachineBasicBlock *BB,
        bool Is64Bit) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();
    MachineFunction *MF = BB->getParent();
    const BasicBlock *LLVM_BB = BB->getBasicBlock();

    assert(getTargetMachine().Options.EnableSegmentedStacks);

    unsigned TlsReg = Cse523::FS;
    unsigned TlsOffset = Is64Bit ? 0x70 : 0x30;

    // BB:
    //  ... [Till the alloca]
    // If stacklet is not large enough, jump to mallocMBB
    //
    // bumpMBB:
    //  Allocate by subtracting from RSP
    //  Jump to continueMBB
    //
    // mallocMBB:
    //  Allocate by call to runtime
    //
    // continueMBB:
    //  ...
    //  [rest of original BB]
    //

    MachineBasicBlock *mallocMBB = MF->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *bumpMBB = MF->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *continueMBB = MF->CreateMachineBasicBlock(LLVM_BB);

    MachineRegisterInfo &MRI = MF->getRegInfo();
    const TargetRegisterClass *AddrRegClass =
        getRegClassFor(Is64Bit ? MVT::i64:MVT::i32);

    unsigned mallocPtrVReg = MRI.createVirtualRegister(AddrRegClass),
             bumpSPPtrVReg = MRI.createVirtualRegister(AddrRegClass),
             tmpSPVReg = MRI.createVirtualRegister(AddrRegClass),
             SPLimitVReg = MRI.createVirtualRegister(AddrRegClass),
             sizeVReg = MI->getOperand(1).getReg(),
             physSPReg = Cse523::RSP;

    MachineFunction::iterator MBBIter = BB;
    ++MBBIter;

    MF->insert(MBBIter, bumpMBB);
    MF->insert(MBBIter, mallocMBB);
    MF->insert(MBBIter, continueMBB);

    continueMBB->splice(continueMBB->begin(), BB, llvm::next
            (MachineBasicBlock::iterator(MI)), BB->end());
    continueMBB->transferSuccessorsAndUpdatePHIs(BB);

    // Add code to the main basic block to check if the stack limit has been hit,
    // and if so, jump to mallocMBB otherwise to bumpMBB.
    BuildMI(BB, DL, TII->get(TargetOpcode::COPY), tmpSPVReg).addReg(physSPReg);
    BuildMI(BB, DL, TII->get(Cse523::SUB64rr), SPLimitVReg)
        .addReg(tmpSPVReg).addReg(sizeVReg);
    BuildMI(BB, DL, TII->get(Cse523::CMP64mr))
        .addReg(0).addImm(1).addReg(0).addImm(TlsOffset).addReg(TlsReg)
        .addReg(SPLimitVReg);
    BuildMI(BB, DL, TII->get(Cse523::JG_4)).addMBB(mallocMBB);

    // bumpMBB simply decreases the stack pointer, since we know the current
    // stacklet has enough space.
    BuildMI(bumpMBB, DL, TII->get(TargetOpcode::COPY), physSPReg)
        .addReg(SPLimitVReg);
    BuildMI(bumpMBB, DL, TII->get(TargetOpcode::COPY), bumpSPPtrVReg)
        .addReg(SPLimitVReg);
    BuildMI(bumpMBB, DL, TII->get(Cse523::JMP_4)).addMBB(continueMBB);

    // Calls into a routine in libgcc to allocate more space from the heap.
    const uint32_t *RegMask =
        getTargetMachine().getRegisterInfo()->getCallPreservedMask(CallingConv::C);

    //BuildMI(mallocMBB, DL, TII->get(Cse523::MOV64rr), Cse523::RDI)
    //    .addReg(sizeVReg);
    //BuildMI(mallocMBB, DL, TII->get(Cse523::CALL64pcrel32))
    //    .addExternalSymbol("__morestack_allocate_stack_space")
    //    .addRegMask(RegMask)
    //    .addReg(Cse523::RDI, RegState::Implicit)
    //    .addReg(Cse523::RAX, RegState::ImplicitDefine);


    //BuildMI(mallocMBB, DL, TII->get(TargetOpcode::COPY), mallocPtrVReg)
    //    .addReg(Cse523::RAX);
    //BuildMI(mallocMBB, DL, TII->get(Cse523::JMP_4)).addMBB(continueMBB);

    // Set up the CFG correctly.
    BB->addSuccessor(bumpMBB);
    BB->addSuccessor(mallocMBB);
    mallocMBB->addSuccessor(continueMBB);
    bumpMBB->addSuccessor(continueMBB);

    // Take care of the PHI nodes.
    //BuildMI(*continueMBB, continueMBB->begin(), DL, TII->get(Cse523::PHI),
    //        MI->getOperand(0).getReg())
    //    .addReg(mallocPtrVReg).addMBB(mallocMBB)
    //    .addReg(bumpSPPtrVReg).addMBB(bumpMBB);

    // Delete the original pseudo instruction.
    MI->eraseFromParent();

    // And we're done.
    return continueMBB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitLoweredWinAlloca(MachineInstr *MI,
        MachineBasicBlock *BB) const {
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
    DebugLoc DL = MI->getDebugLoc();

    assert(!Subtarget->isTargetMacho());

    // The lowering is pretty easy: we're just emitting the call to _alloca.  The
    // non-trivial part is impdef of ESP.

    if (Subtarget->isTargetCygMing()) {
        // ___chkstk(Mingw64):
        // Clobbers R10, R11, RAX and EFLAGS.
        // Updates RSP.
        BuildMI(*BB, MI, DL, TII->get(Cse523::W64ALLOCA))
            .addExternalSymbol("___chkstk")
            .addReg(Cse523::RAX, RegState::Implicit)
            .addReg(Cse523::RSP, RegState::Implicit)
            .addReg(Cse523::RAX, RegState::Define | RegState::Implicit)
            .addReg(Cse523::RSP, RegState::Define | RegState::Implicit)
            .addReg(Cse523::EFLAGS, RegState::Define | RegState::Implicit);
    } else {
        // __chkstk(MSVCRT): does not update stack pointer.
        // Clobbers R10, R11 and EFLAGS.
        BuildMI(*BB, MI, DL, TII->get(Cse523::W64ALLOCA))
            .addExternalSymbol("__chkstk")
            .addReg(Cse523::RAX, RegState::Implicit)
            .addReg(Cse523::EFLAGS, RegState::Define | RegState::Implicit);
        // RAX has the offset to be subtracted from RSP.
        BuildMI(*BB, MI, DL, TII->get(Cse523::SUB64rr), Cse523::RSP)
            .addReg(Cse523::RSP)
            .addReg(Cse523::RAX);
    }
    
    MI->eraseFromParent();   // The pseudo instruction is gone now.
    return BB;
}

MachineBasicBlock *
Cse523TargetLowering::EmitLoweredTLSCall(MachineInstr *MI,
        MachineBasicBlock *BB) const {
    // This is pretty easy.  We're taking the value that we received from
    // our load from the relocation, sticking it in either RDI (cse523-64)
    // or EAX and doing an indirect call.  The return value will then
    // be in the normal return register.
    const Cse523InstrInfo *TII
        = static_cast<const Cse523InstrInfo*>(getTargetMachine().getInstrInfo());
    DebugLoc DL = MI->getDebugLoc();
    MachineFunction *F = BB->getParent();

    assert(Subtarget->isTargetDarwin() && "Darwin only instr emitted?");
    assert(MI->getOperand(3).isGlobal() && "This should be a global");

    // Get a register mask for the lowered call.
    // FIXME: The 32-bit calls have non-standard calling conventions. Use a
    // proper register mask.
    const uint32_t *RegMask =
        getTargetMachine().getRegisterInfo()->getCallPreservedMask(CallingConv::C);
    MachineInstrBuilder MIB = BuildMI(*BB, MI, DL,
            TII->get(Cse523::MOV64rm), Cse523::RDI)
        .addReg(Cse523::RIP)
        .addImm(0).addReg(0)
        .addGlobalAddress(MI->getOperand(3).getGlobal(), 0,
                MI->getOperand(3).getTargetFlags())
        .addReg(0);
    MIB = BuildMI(*BB, MI, DL, TII->get(Cse523::CALL64m));
    addDirectMem(MIB, Cse523::RDI);
    MIB.addReg(Cse523::RAX, RegState::ImplicitDefine).addRegMask(RegMask);

    MI->eraseFromParent(); // The pseudo instruction is gone now.
    return BB;
}

MachineBasicBlock *
Cse523TargetLowering::emitEHSjLjSetJmp(MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    DebugLoc DL = MI->getDebugLoc();
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();

    MachineFunction *MF = MBB->getParent();
    MachineRegisterInfo &MRI = MF->getRegInfo();

    const BasicBlock *BB = MBB->getBasicBlock();
    MachineFunction::iterator I = MBB;
    ++I;

    // Memory Reference
    MachineInstr::mmo_iterator MMOBegin = MI->memoperands_begin();
    MachineInstr::mmo_iterator MMOEnd = MI->memoperands_end();

    unsigned DstReg;
    unsigned MemOpndSlot = 0;

    unsigned CurOp = 0;

    DstReg = MI->getOperand(CurOp++).getReg();
    const TargetRegisterClass *RC = MRI.getRegClass(DstReg);
    assert(RC->hasType(MVT::i32) && "Invalid destination!");
    unsigned mainDstReg = MRI.createVirtualRegister(RC);
    unsigned restoreDstReg = MRI.createVirtualRegister(RC);

    MemOpndSlot = CurOp;

    MVT PVT = getPointerTy();
    assert((PVT == MVT::i64 || PVT == MVT::i32) &&
            "Invalid Pointer Size!");

    // For v = setjmp(buf), we generate
    //
    // thisMBB:
    //  buf[LabelOffset] = restoreMBB
    //  SjLjSetup restoreMBB
    //
    // mainMBB:
    //  v_main = 0
    //
    // sinkMBB:
    //  v = phi(main, restore)
    //
    // restoreMBB:
    //  v_restore = 1

    MachineBasicBlock *thisMBB = MBB;
    MachineBasicBlock *mainMBB = MF->CreateMachineBasicBlock(BB);
    MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(BB);
    MachineBasicBlock *restoreMBB = MF->CreateMachineBasicBlock(BB);
    MF->insert(I, mainMBB);
    MF->insert(I, sinkMBB);
    MF->push_back(restoreMBB);

    MachineInstrBuilder MIB;

    // Transfer the remainder of BB and its successor edges to sinkMBB.
    sinkMBB->splice(sinkMBB->begin(), MBB,
            llvm::next(MachineBasicBlock::iterator(MI)), MBB->end());
    sinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

    // thisMBB:
    unsigned PtrStoreOpc = 0;
    unsigned LabelReg = 0;
    const int64_t LabelOffset = 1 * PVT.getStoreSize();
    Reloc::Model RM = getTargetMachine().getRelocationModel();
    bool UseImmLabel = (getTargetMachine().getCodeModel() == CodeModel::Small) &&
        (RM == Reloc::Static || RM == Reloc::DynamicNoPIC);

    // Prepare IP either in reg or imm.
    if (!UseImmLabel) {
        PtrStoreOpc = Cse523::MOV64mr;
        const TargetRegisterClass *PtrRC = getRegClassFor(PVT);
        LabelReg = MRI.createVirtualRegister(PtrRC);
        MIB = BuildMI(*thisMBB, MI, DL, TII->get(Cse523::LEA64r), LabelReg)
            .addReg(Cse523::RIP)
            .addImm(0)
            .addReg(0)
            .addMBB(restoreMBB)
            .addReg(0);
    } else
        PtrStoreOpc = Cse523::MOV64mi32;
    // Store IP
    MIB = BuildMI(*thisMBB, MI, DL, TII->get(PtrStoreOpc));
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
        if (i == Cse523::AddrDisp)
            MIB.addDisp(MI->getOperand(MemOpndSlot + i), LabelOffset);
        else
            MIB.addOperand(MI->getOperand(MemOpndSlot + i));
    }
    if (!UseImmLabel)
        MIB.addReg(LabelReg);
    else
        MIB.addMBB(restoreMBB);
    MIB.setMemRefs(MMOBegin, MMOEnd);
    // Setup
    MIB = BuildMI(*thisMBB, MI, DL, TII->get(Cse523::EH_SjLj_Setup))
        .addMBB(restoreMBB);

    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    MIB.addRegMask(RegInfo->getNoPreservedMask());
    thisMBB->addSuccessor(mainMBB);
    thisMBB->addSuccessor(restoreMBB);

    // mainMBB:
    //  EAX = 0
    BuildMI(mainMBB, DL, TII->get(Cse523::MOV64r0), mainDstReg);
    mainMBB->addSuccessor(sinkMBB);

    // sinkMBB:
    BuildMI(*sinkMBB, sinkMBB->begin(), DL,
            TII->get(Cse523::PHI), DstReg)
        .addReg(mainDstReg).addMBB(mainMBB)
        .addReg(restoreDstReg).addMBB(restoreMBB);

    // restoreMBB:
    BuildMI(restoreMBB, DL, TII->get(Cse523::MOV64ri), restoreDstReg).addImm(1);
    BuildMI(restoreMBB, DL, TII->get(Cse523::JMP_4)).addMBB(sinkMBB);
    restoreMBB->addSuccessor(sinkMBB);

    MI->eraseFromParent();
    return sinkMBB;
}

MachineBasicBlock *
Cse523TargetLowering::emitEHSjLjLongJmp(MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    DebugLoc DL = MI->getDebugLoc();
    const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();

    MachineFunction *MF = MBB->getParent();
    MachineRegisterInfo &MRI = MF->getRegInfo();

    // Memory Reference
    MachineInstr::mmo_iterator MMOBegin = MI->memoperands_begin();
    MachineInstr::mmo_iterator MMOEnd = MI->memoperands_end();

    MVT PVT = getPointerTy();
    assert((PVT == MVT::i64 || PVT == MVT::i32) &&
            "Invalid Pointer Size!");

    const TargetRegisterClass *RC = &Cse523::GR64RegClass;
    unsigned Tmp = MRI.createVirtualRegister(RC);
    // Since FP is only updated here but NOT referenced, it's treated as GPR.
    const Cse523RegisterInfo *RegInfo =
        static_cast<const Cse523RegisterInfo*>(getTargetMachine().getRegisterInfo());
    unsigned FP = Cse523::RBP;
    unsigned SP = RegInfo->getStackRegister();

    MachineInstrBuilder MIB;

    const int64_t LabelOffset = 1 * PVT.getStoreSize();
    const int64_t SPOffset = 2 * PVT.getStoreSize();

    unsigned PtrLoadOpc = Cse523::MOV64rm;
    unsigned IJmpOpc = Cse523::JMP64r;

    // Reload FP
    MIB = BuildMI(*MBB, MI, DL, TII->get(PtrLoadOpc), FP);
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i)
        MIB.addOperand(MI->getOperand(i));
    MIB.setMemRefs(MMOBegin, MMOEnd);
    // Reload IP
    MIB = BuildMI(*MBB, MI, DL, TII->get(PtrLoadOpc), Tmp);
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
        if (i == Cse523::AddrDisp)
            MIB.addDisp(MI->getOperand(i), LabelOffset);
        else
            MIB.addOperand(MI->getOperand(i));
    }
    MIB.setMemRefs(MMOBegin, MMOEnd);
    // Reload SP
    MIB = BuildMI(*MBB, MI, DL, TII->get(PtrLoadOpc), SP);
    for (unsigned i = 0; i < Cse523::AddrNumOperands; ++i) {
        if (i == Cse523::AddrDisp)
            MIB.addDisp(MI->getOperand(i), SPOffset);
        else
            MIB.addOperand(MI->getOperand(i));
    }
    MIB.setMemRefs(MMOBegin, MMOEnd);
    // Jump
    BuildMI(*MBB, MI, DL, TII->get(IJmpOpc)).addReg(Tmp);

    MI->eraseFromParent();
    return MBB;
}

// Replace 213-type (isel default) FMA3 instructions with 231-type for
// accumulator loops. Writing back to the accumulator allows the coalescer
// to remove extra copies in the loop.   
MachineBasicBlock *
Cse523TargetLowering::emitFMA3Instr(MachineInstr *MI,
        MachineBasicBlock *MBB) const {
    assert(0);
    return NULL;
}

MachineBasicBlock *
Cse523TargetLowering::EmitInstrWithCustomInserter(MachineInstr *MI,
        MachineBasicBlock *BB) const {
    switch (MI->getOpcode()) {
        default: llvm_unreachable("Unexpected instr type to insert");
        case Cse523::TAILJMPd64:
        case Cse523::TAILJMPr64:
        case Cse523::TAILJMPm64:
                 llvm_unreachable("TAILJMP64 would not be touched here.");
        case Cse523::TCRETURNdi64:
        case Cse523::TCRETURNri64:
        case Cse523::TCRETURNmi64:
                 return BB;
        case Cse523::SEG_ALLOCA_64:
                 return EmitLoweredSegAlloca(MI, BB, true);
        case Cse523::TLSCall_64:
                 return EmitLoweredTLSCall(MI, BB);
        case Cse523::SETCC_GR64:        
                 return EmitLoweredSetCC(MI, BB);
        case Cse523::CMOV_GR64:        
                 return EmitLoweredSelect(MI, BB);
    }
}

//===----------------------------------------------------------------------===//
//                           Cse523 Optimization Hooks
//===----------------------------------------------------------------------===//

void Cse523TargetLowering::computeMaskedBitsForTargetNode(const SDValue Op,
        APInt &KnownZero,
        APInt &KnownOne,
        const SelectionDAG &DAG,
        unsigned Depth) const {
    unsigned BitWidth = KnownZero.getBitWidth();
    unsigned Opc = Op.getOpcode();
    assert((Opc >= ISD::BUILTIN_OP_END ||
                Opc == ISD::INTRINSIC_WO_CHAIN ||
                Opc == ISD::INTRINSIC_W_CHAIN ||
                Opc == ISD::INTRINSIC_VOID) &&
            "Should use MaskedValueIsZero if you don't know whether Op"
            " is a target node!");

    KnownZero = KnownOne = APInt(BitWidth, 0);   // Don't know anything.
    switch (Opc) {
        default: break;
        case Cse523ISD::ADD:
        case Cse523ISD::SUB:
        case Cse523ISD::ADC:
//        case Cse523ISD::SBB:
        case Cse523ISD::SMUL:
        case Cse523ISD::UMUL:
//        case Cse523ISD::INC:
//        case Cse523ISD::DEC:
        case Cse523ISD::OR:
        case Cse523ISD::XOR:
        case Cse523ISD::AND:
                 // These nodes' second result is a boolean.
                 if (Op.getResNo() == 0)
                     break;
                 // Fallthrough
        case Cse523ISD::SETCC:
                 KnownZero |= APInt::getHighBitsSet(BitWidth, BitWidth - 1);
                 break;
        case ISD::INTRINSIC_WO_CHAIN:
                assert(0);
    }
}

unsigned Cse523TargetLowering::ComputeNumSignBitsForTargetNode(SDValue Op,
        unsigned Depth) const {
    // SETCC_CARRY sets the dest to ~0 for true or 0 for false.
    if (Op.getOpcode() == Cse523ISD::SETCC_CARRY)
        return Op.getValueType().getScalarType().getSizeInBits();

    // Fallback case.
    return 1;
}

/// isGAPlusOffset - Returns true (and the GlobalValue and the offset) if the
/// node is a GlobalAddress + offset.
bool Cse523TargetLowering::isGAPlusOffset(SDNode *N,
        const GlobalValue* &GA,
        int64_t &Offset) const {
    if (N->getOpcode() == Cse523ISD::Wrapper) { 
        if (isa<GlobalAddressSDNode>(N->getOperand(0))) {
            GA = cast<GlobalAddressSDNode>(N->getOperand(0))->getGlobal();
            Offset = cast<GlobalAddressSDNode>(N->getOperand(0))->getOffset();
            return true;
        }
    }
    return TargetLowering::isGAPlusOffset(N, GA, Offset);
}

/// isShuffleHigh128VectorInsertLow - Checks whether the shuffle node is the
/// same as extracting the high 128-bit part of 256-bit vector and then
/// inserting the result into the low part of a new 256-bit vector
static bool isShuffleHigh128VectorInsertLow(ShuffleVectorSDNode *SVOp) {
    EVT VT = SVOp->getValueType(0);
    unsigned NumElems = VT.getVectorNumElements();

    // vector_shuffle <4, 5, 6, 7, u, u, u, u> or <2, 3, u, u>
    for (unsigned i = 0, j = NumElems/2; i != NumElems/2; ++i, ++j)
        if (!isUndefOrEqual(SVOp->getMaskElt(i), j) ||
                SVOp->getMaskElt(j) >= 0)
            return false;

    return true;
}

/// isShuffleLow128VectorInsertHigh - Checks whether the shuffle node is the
/// same as extracting the low 128-bit part of 256-bit vector and then
/// inserting the result into the high part of a new 256-bit vector
static bool isShuffleLow128VectorInsertHigh(ShuffleVectorSDNode *SVOp) {
    EVT VT = SVOp->getValueType(0);
    unsigned NumElems = VT.getVectorNumElements();

    // vector_shuffle <u, u, u, u, 0, 1, 2, 3> or <u, u, 0, 1>
    for (unsigned i = NumElems/2, j = 0; i != NumElems; ++i, ++j)
        if (!isUndefOrEqual(SVOp->getMaskElt(i), j) ||
                SVOp->getMaskElt(j) >= 0)
            return false;

    return true;
}

/// PerformShuffleCombine256 - Performs shuffle combines for 256-bit vectors.
static SDValue PerformShuffleCombine256(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget* Subtarget) {
    SDLoc dl(N);
    ShuffleVectorSDNode *SVOp = cast<ShuffleVectorSDNode>(N);
    SDValue V1 = SVOp->getOperand(0);
    SDValue V2 = SVOp->getOperand(1);
    EVT VT = SVOp->getValueType(0);
    unsigned NumElems = VT.getVectorNumElements();

    if (V1.getOpcode() == ISD::CONCAT_VECTORS &&
            V2.getOpcode() == ISD::CONCAT_VECTORS) {
        //
        //                   0,0,0,...
        //                      |
        //    V      UNDEF    BUILD_VECTOR    UNDEF
        //     \      /           \           /
        //  CONCAT_VECTOR         CONCAT_VECTOR
        //         \                  /
        //          \                /
        //          RESULT: V + zero extended
        //
        if (V2.getOperand(0).getOpcode() != ISD::BUILD_VECTOR ||
                V2.getOperand(1).getOpcode() != ISD::UNDEF ||
                V1.getOperand(1).getOpcode() != ISD::UNDEF)
            return SDValue();

        if (!ISD::isBuildVectorAllZeros(V2.getOperand(0).getNode()))
            return SDValue();

        // To match the shuffle mask, the first half of the mask should
        // be exactly the first vector, and all the rest a splat with the
        // first element of the second one.
        for (unsigned i = 0; i != NumElems/2; ++i)
            if (!isUndefOrEqual(SVOp->getMaskElt(i), i) ||
                    !isUndefOrEqual(SVOp->getMaskElt(i+NumElems/2), NumElems))
                return SDValue();

        // If V1 is coming from a vector load then just fold to a VZEXT_LOAD.
        if (LoadSDNode *Ld = dyn_cast<LoadSDNode>(V1.getOperand(0))) {
            if (Ld->hasNUsesOfValue(1, 0)) {
                SDVTList Tys = DAG.getVTList(MVT::v4i64, MVT::Other);
                SDValue Ops[] = { Ld->getChain(), Ld->getBasePtr() };
                SDValue ResNode =
                    DAG.getMemIntrinsicNode(Cse523ISD::VZEXT_LOAD, dl, Tys, Ops,
                            array_lengthof(Ops),
                            Ld->getMemoryVT(),
                            Ld->getPointerInfo(),
                            Ld->getAlignment(),
                            false/*isVolatile*/, true/*ReadMem*/,
                            false/*WriteMem*/);

                // Make sure the newly-created LOAD is in the same position as Ld in
                // terms of dependency. We create a TokenFactor for Ld and ResNode,
                // and update uses of Ld's output chain to use the TokenFactor.
                if (Ld->hasAnyUseOfValue(1)) {
                    SDValue NewChain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                            SDValue(Ld, 1), SDValue(ResNode.getNode(), 1));
                    DAG.ReplaceAllUsesOfValueWith(SDValue(Ld, 1), NewChain);
                    DAG.UpdateNodeOperands(NewChain.getNode(), SDValue(Ld, 1),
                            SDValue(ResNode.getNode(), 1));
                }

                return DAG.getNode(ISD::BITCAST, dl, VT, ResNode);
            }
        }

        // Emit a zeroed vector and insert the desired subvector on its
        // first half.
        SDValue Zeros = getZeroVector(VT, Subtarget, DAG, dl);
        SDValue InsV = Insert128BitVector(Zeros, V1.getOperand(0), 0, DAG, dl);
        return DCI.CombineTo(N, InsV);
    }

    //===--------------------------------------------------------------------===//
    // Combine some shuffles into subvector extracts and inserts:
    //

    // vector_shuffle <4, 5, 6, 7, u, u, u, u> or <2, 3, u, u>
    if (isShuffleHigh128VectorInsertLow(SVOp)) {
        SDValue V = Extract128BitVector(V1, NumElems/2, DAG, dl);
        SDValue InsV = Insert128BitVector(DAG.getUNDEF(VT), V, 0, DAG, dl);
        return DCI.CombineTo(N, InsV);
    }

    // vector_shuffle <u, u, u, u, 0, 1, 2, 3> or <u, u, 0, 1>
    if (isShuffleLow128VectorInsertHigh(SVOp)) {
        SDValue V = Extract128BitVector(V1, 0, DAG, dl);
        SDValue InsV = Insert128BitVector(DAG.getUNDEF(VT), V, NumElems/2, DAG, dl);
        return DCI.CombineTo(N, InsV);
    }

    return SDValue();
}

/// PerformShuffleCombine - Performs several different shuffle combines.
static SDValue PerformShuffleCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    SDLoc dl(N);
    EVT VT = N->getValueType(0);

    // Don't create instructions with illegal types after legalize types has run.
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    if (!DCI.isBeforeLegalize() && !TLI.isTypeLegal(VT.getVectorElementType()))
        return SDValue();

    // Combine 256-bit vector shuffles. This is only profitable when in AVX mode
    if (Subtarget->hasFp256() && VT.is256BitVector() &&
            N->getOpcode() == ISD::VECTOR_SHUFFLE)
        return PerformShuffleCombine256(N, DAG, DCI, Subtarget);

    // Only handle 128 wide vector from here on.
    if (!VT.is128BitVector())
        return SDValue();

    // Combine a vector_shuffle that is equal to build_vector load1, load2, load3,
    // load4, <0, 1, 2, 3> into a 128-bit load if the load addresses are
    // consecutive, non-overlapping, and in the right order.
    SmallVector<SDValue, 16> Elts;
    for (unsigned i = 0, e = VT.getVectorNumElements(); i != e; ++i)
        Elts.push_back(getShuffleScalarElt(N, i, DAG, 0));

    return EltsFromConsecutiveLoads(VT, Elts, dl, DAG, true);
}

/// PerformTruncateCombine - Converts truncate operation to
/// a sequence of vector shuffle operations.
/// It is possible when we truncate 256-bit vector to 128-bit vector
static SDValue PerformTruncateCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget)  {
    return SDValue();
}

/// PerformEXTRACT_VECTOR_ELTCombine - Detect vector gather/scatter index
/// generation and convert it from being a bunch of shuffles and extracts
/// to a simple store and scalar loads to extract the elements.
static SDValue PerformEXTRACT_VECTOR_ELTCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI) {
    //assert(0);
    // The replacement was made in place; don't return anything.
    return SDValue();
}

/// \brief Matches a VSELECT onto min/max or return 0 if the node doesn't match.
static std::pair<unsigned, bool>
matchIntegerMINMAX(SDValue Cond, EVT VT, SDValue LHS, SDValue RHS,
        SelectionDAG &DAG, const Cse523Subtarget *Subtarget) {
    if (!VT.isVector())
        return std::make_pair(0, false);
    llvm_unreachable("Unexpected Vector Instructions");
}

/// PerformSELECTCombine - Do target-specific dag combines on SELECT and VSELECT
/// nodes.
static SDValue PerformSELECTCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    SDLoc DL(N);
    SDValue Cond = N->getOperand(0);
    // Get the LHS/RHS of the select.
    SDValue LHS = N->getOperand(1);
    SDValue RHS = N->getOperand(2);
    EVT VT = LHS.getValueType();
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();

    // If we have SSE[12] support, try to form min/max nodes. SSE min/max
    // instructions match the semantics of the common C idiom x<y?x:y but not
    // x<=y?x:y, because of how they handle negative zero (which can be
    // ignored in unsafe-math mode).
    if (Cond.getOpcode() == ISD::SETCC && VT.isFloatingPoint() &&
            VT != MVT::f80 && TLI.isTypeLegal(VT) &&
            (Subtarget->hasSSE2() ||
             (Subtarget->hasSSE1() && VT.getScalarType() == MVT::f32))) {
        ISD::CondCode CC = cast<CondCodeSDNode>(Cond.getOperand(2))->get();

        unsigned Opcode = 0;
        // Check for x CC y ? x : y.
        if (DAG.isEqualTo(LHS, Cond.getOperand(0)) &&
                DAG.isEqualTo(RHS, Cond.getOperand(1))) {
            switch (CC) {
                default: break;
                case ISD::SETULT:
                         // Converting this to a min would handle NaNs incorrectly, and swapping
                         // the operands would cause it to handle comparisons between positive
                         // and negative zero incorrectly.
                         if (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS)) {
                             if (!DAG.getTarget().Options.UnsafeFPMath &&
                                     !(DAG.isKnownNeverZero(LHS) || DAG.isKnownNeverZero(RHS)))
                                 break;
                             std::swap(LHS, RHS);
                         }
                         Opcode = Cse523ISD::FMIN;
                         break;
                case ISD::SETOLE:
                         // Converting this to a min would handle comparisons between positive
                         // and negative zero incorrectly.
                         if (!DAG.getTarget().Options.UnsafeFPMath &&
                                 !DAG.isKnownNeverZero(LHS) && !DAG.isKnownNeverZero(RHS))
                             break;
                         Opcode = Cse523ISD::FMIN;
                         break;
                case ISD::SETULE:
                         // Converting this to a min would handle both negative zeros and NaNs
                         // incorrectly, but we can swap the operands to fix both.
                         std::swap(LHS, RHS);
                case ISD::SETOLT:
                case ISD::SETLT:
                case ISD::SETLE:
                         Opcode = Cse523ISD::FMIN;
                         break;

                case ISD::SETOGE:
                         // Converting this to a max would handle comparisons between positive
                         // and negative zero incorrectly.
                         if (!DAG.getTarget().Options.UnsafeFPMath &&
                                 !DAG.isKnownNeverZero(LHS) && !DAG.isKnownNeverZero(RHS))
                             break;
                         Opcode = Cse523ISD::FMAX;
                         break;
                case ISD::SETUGT:
                         // Converting this to a max would handle NaNs incorrectly, and swapping
                         // the operands would cause it to handle comparisons between positive
                         // and negative zero incorrectly.
                         if (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS)) {
                             if (!DAG.getTarget().Options.UnsafeFPMath &&
                                     !(DAG.isKnownNeverZero(LHS) || DAG.isKnownNeverZero(RHS)))
                                 break;
                             std::swap(LHS, RHS);
                         }
                         Opcode = Cse523ISD::FMAX;
                         break;
                case ISD::SETUGE:
                         // Converting this to a max would handle both negative zeros and NaNs
                         // incorrectly, but we can swap the operands to fix both.
                         std::swap(LHS, RHS);
                case ISD::SETOGT:
                case ISD::SETGT:
                case ISD::SETGE:
                         Opcode = Cse523ISD::FMAX;
                         break;
            }
            // Check for x CC y ? y : x -- a min/max with reversed arms.
        } else if (DAG.isEqualTo(LHS, Cond.getOperand(1)) &&
                DAG.isEqualTo(RHS, Cond.getOperand(0))) {
            switch (CC) {
                default: break;
                case ISD::SETOGE:
                         // Converting this to a min would handle comparisons between positive
                         // and negative zero incorrectly, and swapping the operands would
                         // cause it to handle NaNs incorrectly.
                         if (!DAG.getTarget().Options.UnsafeFPMath &&
                                 !(DAG.isKnownNeverZero(LHS) || DAG.isKnownNeverZero(RHS))) {
                             if (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS))
                                 break;
                             std::swap(LHS, RHS);
                         }
                         Opcode = Cse523ISD::FMIN;
                         break;
                case ISD::SETUGT:
                         // Converting this to a min would handle NaNs incorrectly.
                         if (!DAG.getTarget().Options.UnsafeFPMath &&
                                 (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS)))
                             break;
                         Opcode = Cse523ISD::FMIN;
                         break;
                case ISD::SETUGE:
                         // Converting this to a min would handle both negative zeros and NaNs
                         // incorrectly, but we can swap the operands to fix both.
                         std::swap(LHS, RHS);
                case ISD::SETOGT:
                case ISD::SETGT:
                case ISD::SETGE:
                         Opcode = Cse523ISD::FMIN;
                         break;

                case ISD::SETULT:
                         // Converting this to a max would handle NaNs incorrectly.
                         if (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS))
                             break;
                         Opcode = Cse523ISD::FMAX;
                         break;
                case ISD::SETOLE:
                         // Converting this to a max would handle comparisons between positive
                         // and negative zero incorrectly, and swapping the operands would
                         // cause it to handle NaNs incorrectly.
                         if (!DAG.getTarget().Options.UnsafeFPMath &&
                                 !DAG.isKnownNeverZero(LHS) && !DAG.isKnownNeverZero(RHS)) {
                             if (!DAG.isKnownNeverNaN(LHS) || !DAG.isKnownNeverNaN(RHS))
                                 break;
                             std::swap(LHS, RHS);
                         }
                         Opcode = Cse523ISD::FMAX;
                         break;
                case ISD::SETULE:
                         // Converting this to a max would handle both negative zeros and NaNs
                         // incorrectly, but we can swap the operands to fix both.
                         std::swap(LHS, RHS);
                case ISD::SETOLT:
                case ISD::SETLT:
                case ISD::SETLE:
                         Opcode = Cse523ISD::FMAX;
                         break;
            }
        }

        if (Opcode)
            return DAG.getNode(Opcode, DL, N->getValueType(0), LHS, RHS);
    }

    EVT CondVT = Cond.getValueType();
    if (Subtarget->hasAVX512() && VT.isVector() && CondVT.isVector() &&
            CondVT.getVectorElementType() == MVT::i1) {
        // v16i8 (select v16i1, v16i8, v16i8) does not have a proper
        // lowering on AVX-512. In this case we convert it to
        // v16i8 (select v16i8, v16i8, v16i8) and use AVX instruction.
        // The same situation for all 128 and 256-bit vectors of i8 and i16
        EVT OpVT = LHS.getValueType();
        if ((OpVT.is128BitVector() || OpVT.is256BitVector()) &&
                (OpVT.getVectorElementType() == MVT::i8 ||
                 OpVT.getVectorElementType() == MVT::i16)) {
            Cond = DAG.getNode(ISD::SIGN_EXTEND, DL, OpVT, Cond);
            DCI.AddToWorklist(Cond.getNode());
            return DAG.getNode(N->getOpcode(), DL, OpVT, Cond, LHS, RHS);
        }
    }
    // If this is a select between two integer constants, try to do some
    // optimizations.
    if (ConstantSDNode *TrueC = dyn_cast<ConstantSDNode>(LHS)) {
        if (ConstantSDNode *FalseC = dyn_cast<ConstantSDNode>(RHS))
            // Don't do this for crazy integer types.
            if (DAG.getTargetLoweringInfo().isTypeLegal(LHS.getValueType())) {
                // If this is efficiently invertible, canonicalize the LHSC/RHSC values
                // so that TrueC (the true value) is larger than FalseC.
                bool NeedsCondInvert = false;

                if (TrueC->getAPIntValue().ult(FalseC->getAPIntValue()) &&
                        // Efficiently invertible.
                        (Cond.getOpcode() == ISD::SETCC ||  // setcc -> invertible.
                         (Cond.getOpcode() == ISD::XOR &&   // xor(X, C) -> invertible.
                          isa<ConstantSDNode>(Cond.getOperand(1))))) {
                    NeedsCondInvert = true;
                    std::swap(TrueC, FalseC);
                }

                // Optimize C ? 8 : 0 -> zext(C) << 3.  Likewise for any pow2/0.
                if (FalseC->getAPIntValue() == 0 &&
                        TrueC->getAPIntValue().isPowerOf2()) {
                    if (NeedsCondInvert) // Invert the condition if needed.
                        Cond = DAG.getNode(ISD::XOR, DL, Cond.getValueType(), Cond,
                                DAG.getConstant(1, Cond.getValueType()));

                    // Zero extend the condition if needed.
                    Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, LHS.getValueType(), Cond);

                    unsigned ShAmt = TrueC->getAPIntValue().logBase2();
                    return DAG.getNode(ISD::SHL, DL, LHS.getValueType(), Cond,
                            DAG.getConstant(ShAmt, MVT::i64));
                }

                // Optimize Cond ? cst+1 : cst -> zext(setcc(C)+cst.
                if (FalseC->getAPIntValue()+1 == TrueC->getAPIntValue()) {
                    if (NeedsCondInvert) // Invert the condition if needed.
                        Cond = DAG.getNode(ISD::XOR, DL, Cond.getValueType(), Cond,
                                DAG.getConstant(1, Cond.getValueType()));

                    // Zero extend the condition if needed.
                    Cond = DAG.getNode(ISD::ZERO_EXTEND, DL,
                            FalseC->getValueType(0), Cond);
                    return DAG.getNode(ISD::ADD, DL, Cond.getValueType(), Cond,
                            SDValue(FalseC, 0));
                }

                // Optimize cases that will turn into an LEA instruction.  This requires
                // an i32 or i64 and an efficient multiplier (1, 2, 3, 4, 5, 8, 9).
                if (N->getValueType(0) == MVT::i32 || N->getValueType(0) == MVT::i64) {
                    uint64_t Diff = TrueC->getZExtValue()-FalseC->getZExtValue();
                    if (N->getValueType(0) == MVT::i32) Diff = (unsigned)Diff;

                    bool isFastMultiplier = false;
                    if (Diff < 10) {
                        switch ((unsigned char)Diff) {
                            default: break;
                            case 1:  // result = add base, cond
                            case 2:  // result = lea base(    , cond*2)
                            case 3:  // result = lea base(cond, cond*2)
                            case 4:  // result = lea base(    , cond*4)
                            case 5:  // result = lea base(cond, cond*4)
                            case 8:  // result = lea base(    , cond*8)
                            case 9:  // result = lea base(cond, cond*8)
                                     isFastMultiplier = true;
                                     break;
                        }
                    }

                    if (isFastMultiplier) {
                        APInt Diff = TrueC->getAPIntValue()-FalseC->getAPIntValue();
                        if (NeedsCondInvert) // Invert the condition if needed.
                            Cond = DAG.getNode(ISD::XOR, DL, Cond.getValueType(), Cond,
                                    DAG.getConstant(1, Cond.getValueType()));

                        // Zero extend the condition if needed.
                        Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, FalseC->getValueType(0),
                                Cond);
                        // Scale the condition by the difference.
                        if (Diff != 1)
                            Cond = DAG.getNode(ISD::MUL, DL, Cond.getValueType(), Cond,
                                    DAG.getConstant(Diff, Cond.getValueType()));

                        // Add the base if non-zero.
                        if (FalseC->getAPIntValue() != 0)
                            Cond = DAG.getNode(ISD::ADD, DL, Cond.getValueType(), Cond,
                                    SDValue(FalseC, 0));
                        return Cond;
                    }
                }
            }
    }

    // Canonicalize max and min:
    // (x > y) ? x : y -> (x >= y) ? x : y
    // (x < y) ? x : y -> (x <= y) ? x : y
    // This allows use of COND_S / COND_NS (see TranslateCse523CC) which eliminates
    // the need for an extra compare
    // against zero. e.g.
    // (x - y) > 0 : (x - y) ? 0 -> (x - y) >= 0 : (x - y) ? 0
    // subl   %esi, %edi
    // testl  %edi, %edi
    // movl   $0, %eax
    // cmovgl %edi, %eax
    // =>
    // xorl   %eax, %eax
    // subl   %esi, $edi
    // cmovsl %eax, %edi
    if (N->getOpcode() == ISD::SELECT && Cond.getOpcode() == ISD::SETCC &&
            DAG.isEqualTo(LHS, Cond.getOperand(0)) &&
            DAG.isEqualTo(RHS, Cond.getOperand(1))) {
        ISD::CondCode CC = cast<CondCodeSDNode>(Cond.getOperand(2))->get();
        switch (CC) {
            default: break;
            case ISD::SETLT:
            case ISD::SETGT: {
                                 ISD::CondCode NewCC = (CC == ISD::SETLT) ? ISD::SETLE : ISD::SETGE;
                                 Cond = DAG.getSetCC(SDLoc(Cond), Cond.getValueType(),
                                         Cond.getOperand(0), Cond.getOperand(1), NewCC);
                                 return DAG.getNode(ISD::SELECT, DL, VT, Cond, LHS, RHS);
                             }
        }
    }

    // Early exit check
    if (!TLI.isTypeLegal(VT))
        return SDValue();

    // Match VSELECTs into subs with unsigned saturation.
    if (N->getOpcode() == ISD::VSELECT && Cond.getOpcode() == ISD::SETCC &&
            // psubus is available in SSE2 and AVX2 for i8 and i16 vectors.
            ((Subtarget->hasSSE2() && (VT == MVT::v16i8 || VT == MVT::v8i16)) ||
             (Subtarget->hasAVX2() && (VT == MVT::v32i8 || VT == MVT::v16i16)))) {
        ISD::CondCode CC = cast<CondCodeSDNode>(Cond.getOperand(2))->get();

        // Check if one of the arms of the VSELECT is a zero vector. If it's on the
        // left side invert the predicate to simplify logic below.
        SDValue Other;
        if (ISD::isBuildVectorAllZeros(LHS.getNode())) {
            Other = RHS;
            CC = ISD::getSetCCInverse(CC, true);
        } else if (ISD::isBuildVectorAllZeros(RHS.getNode())) {
            Other = LHS;
        }

        if (Other.getNode() && Other->getNumOperands() == 2 &&
                DAG.isEqualTo(Other->getOperand(0), Cond.getOperand(0))) {
            SDValue OpLHS = Other->getOperand(0), OpRHS = Other->getOperand(1);
            SDValue CondRHS = Cond->getOperand(1);

            // Look for a general sub with unsigned saturation first.
            // x >= y ? x-y : 0 --> subus x, y
            // x >  y ? x-y : 0 --> subus x, y
            if ((CC == ISD::SETUGE || CC == ISD::SETUGT) &&
                    Other->getOpcode() == ISD::SUB && DAG.isEqualTo(OpRHS, CondRHS))
                return DAG.getNode(Cse523ISD::SUBUS, DL, VT, OpLHS, OpRHS);

            // If the RHS is a constant we have to reverse the const canonicalization.
            // x > C-1 ? x+-C : 0 --> subus x, C
            if (CC == ISD::SETUGT && Other->getOpcode() == ISD::ADD &&
                    isSplatVector(CondRHS.getNode()) && isSplatVector(OpRHS.getNode())) {
                APInt A = cast<ConstantSDNode>(OpRHS.getOperand(0))->getAPIntValue();
                if (CondRHS.getConstantOperandVal(0) == -A-1)
                    return DAG.getNode(Cse523ISD::SUBUS, DL, VT, OpLHS,
                            DAG.getConstant(-A, VT));
            }

            // Another special case: If C was a sign bit, the sub has been
            // canonicalized into a xor.
            // FIXME: Would it be better to use ComputeMaskedBits to determine whether
            //        it's safe to decanonicalize the xor?
            // x s< 0 ? x^C : 0 --> subus x, C
            if (CC == ISD::SETLT && Other->getOpcode() == ISD::XOR &&
                    ISD::isBuildVectorAllZeros(CondRHS.getNode()) &&
                    isSplatVector(OpRHS.getNode())) {
                APInt A = cast<ConstantSDNode>(OpRHS.getOperand(0))->getAPIntValue();
                if (A.isSignBit())
                    return DAG.getNode(Cse523ISD::SUBUS, DL, VT, OpLHS, OpRHS);
            }
        }
    }

    // Try to match a min/max vector operation.
    if (N->getOpcode() == ISD::VSELECT && Cond.getOpcode() == ISD::SETCC) {
        std::pair<unsigned, bool> ret = matchIntegerMINMAX(Cond, VT, LHS, RHS, DAG, Subtarget);
        unsigned Opc = ret.first;
        bool NeedSplit = ret.second;

        if (Opc && NeedSplit) {
            unsigned NumElems = VT.getVectorNumElements();
            // Extract the LHS vectors
            SDValue LHS1 = Extract128BitVector(LHS, 0, DAG, DL);
            SDValue LHS2 = Extract128BitVector(LHS, NumElems/2, DAG, DL);

            // Extract the RHS vectors
            SDValue RHS1 = Extract128BitVector(RHS, 0, DAG, DL);
            SDValue RHS2 = Extract128BitVector(RHS, NumElems/2, DAG, DL);

            // Create min/max for each subvector
            LHS = DAG.getNode(Opc, DL, LHS1.getValueType(), LHS1, RHS1);
            RHS = DAG.getNode(Opc, DL, LHS2.getValueType(), LHS2, RHS2);

            // Merge the result
            return DAG.getNode(ISD::CONCAT_VECTORS, DL, VT, LHS, RHS);
        } else if (Opc)
            return DAG.getNode(Opc, DL, VT, LHS, RHS);
    }

    // Simplify vector selection if the selector will be produced by CMPP*/PCMP*.
    if (N->getOpcode() == ISD::VSELECT && Cond.getOpcode() == ISD::SETCC &&
            // Check if SETCC has already been promoted
            TLI.getSetCCResultType(*DAG.getContext(), VT) == CondVT &&
            // Check that condition value type matches vselect operand type
            CondVT == VT) { 

        assert(Cond.getValueType().isVector() &&
                "vector select expects a vector selector!");

        bool TValIsAllOnes = ISD::isBuildVectorAllOnes(LHS.getNode());
        bool FValIsAllZeros = ISD::isBuildVectorAllZeros(RHS.getNode());

        if (!TValIsAllOnes && !FValIsAllZeros) {
            // Try invert the condition if true value is not all 1s and false value
            // is not all 0s.
            bool TValIsAllZeros = ISD::isBuildVectorAllZeros(LHS.getNode());
            bool FValIsAllOnes = ISD::isBuildVectorAllOnes(RHS.getNode());

            if (TValIsAllZeros || FValIsAllOnes) {
                SDValue CC = Cond.getOperand(2);
                ISD::CondCode NewCC =
                    ISD::getSetCCInverse(cast<CondCodeSDNode>(CC)->get(),
                            Cond.getOperand(0).getValueType().isInteger());
                Cond = DAG.getSetCC(DL, CondVT, Cond.getOperand(0), Cond.getOperand(1), NewCC);
                std::swap(LHS, RHS);
                TValIsAllOnes = FValIsAllOnes;
                FValIsAllZeros = TValIsAllZeros;
            }
        }

        if (TValIsAllOnes || FValIsAllZeros) {
            SDValue Ret;

            if (TValIsAllOnes && FValIsAllZeros)
                Ret = Cond;
            else if (TValIsAllOnes)
                Ret = DAG.getNode(ISD::OR, DL, CondVT, Cond,
                        DAG.getNode(ISD::BITCAST, DL, CondVT, RHS));
            else if (FValIsAllZeros)
                Ret = DAG.getNode(ISD::AND, DL, CondVT, Cond,
                        DAG.getNode(ISD::BITCAST, DL, CondVT, LHS));

            return DAG.getNode(ISD::BITCAST, DL, VT, Ret);
        }
    }

    // Try to fold this VSELECT into a MOVSS/MOVSD
    if (N->getOpcode() == ISD::VSELECT &&
            Cond.getOpcode() == ISD::BUILD_VECTOR && !DCI.isBeforeLegalize()) {
        if (VT == MVT::v4i32 || VT == MVT::v4f32 ||
                (Subtarget->hasSSE2() && (VT == MVT::v2i64 || VT == MVT::v2f64))) {
            bool CanFold = false;
            unsigned NumElems = Cond.getNumOperands();
            SDValue A = LHS;
            SDValue B = RHS;

            if (isZero(Cond.getOperand(0))) {
                CanFold = true;

                // fold (vselect <0,-1,-1,-1>, A, B) -> (movss A, B)
                // fold (vselect <0,-1> -> (movsd A, B)
                for (unsigned i = 1, e = NumElems; i != e && CanFold; ++i)
                    CanFold = isAllOnes(Cond.getOperand(i));
            } else if (isAllOnes(Cond.getOperand(0))) {
                CanFold = true;
                std::swap(A, B);

                // fold (vselect <-1,0,0,0>, A, B) -> (movss B, A)
                // fold (vselect <-1,0> -> (movsd B, A)
                for (unsigned i = 1, e = NumElems; i != e && CanFold; ++i)
                    CanFold = isZero(Cond.getOperand(i));
            }

            if (CanFold) {
                if (VT == MVT::v4i32 || VT == MVT::v4f32)
                    return getTargetShuffleNode(Cse523ISD::MOVSS, DL, VT, A, B, DAG);
                return getTargetShuffleNode(Cse523ISD::MOVSD, DL, VT, A, B, DAG);
            }

            if (Subtarget->hasSSE2() && (VT == MVT::v4i32 || VT == MVT::v4f32)) {
                // fold (v4i32: vselect <0,0,-1,-1>, A, B) ->
                //      (v4i32 (bitcast (movsd (v2i64 (bitcast A)),
                //                             (v2i64 (bitcast B)))))
                //
                // fold (v4f32: vselect <0,0,-1,-1>, A, B) ->
                //      (v4f32 (bitcast (movsd (v2f64 (bitcast A)),
                //                             (v2f64 (bitcast B)))))
                //
                // fold (v4i32: vselect <-1,-1,0,0>, A, B) ->
                //      (v4i32 (bitcast (movsd (v2i64 (bitcast B)),
                //                             (v2i64 (bitcast A)))))
                //
                // fold (v4f32: vselect <-1,-1,0,0>, A, B) ->
                //      (v4f32 (bitcast (movsd (v2f64 (bitcast B)),
                //                             (v2f64 (bitcast A)))))

                CanFold = (isZero(Cond.getOperand(0)) &&
                        isZero(Cond.getOperand(1)) &&
                        isAllOnes(Cond.getOperand(2)) &&
                        isAllOnes(Cond.getOperand(3)));

                if (!CanFold && isAllOnes(Cond.getOperand(0)) &&
                        isAllOnes(Cond.getOperand(1)) &&
                        isZero(Cond.getOperand(2)) &&
                        isZero(Cond.getOperand(3))) {
                    CanFold = true;
                    std::swap(LHS, RHS);
                }

                if (CanFold) {
                    EVT NVT = (VT == MVT::v4i32) ? MVT::v2i64 : MVT::v2f64;
                    SDValue NewA = DAG.getNode(ISD::BITCAST, DL, NVT, LHS);
                    SDValue NewB = DAG.getNode(ISD::BITCAST, DL, NVT, RHS);
                    SDValue Select = getTargetShuffleNode(Cse523ISD::MOVSD, DL, NVT, NewA,
                            NewB, DAG);
                    return DAG.getNode(ISD::BITCAST, DL, VT, Select);
                }
            }
        }
    }

    // If we know that this node is legal then we know that it is going to be
    // matched by one of the SSE/AVX BLEND instructions. These instructions only
    // depend on the highest bit in each word. Try to use SimplifyDemandedBits
    // to simplify previous instructions.
    if (N->getOpcode() == ISD::VSELECT && DCI.isBeforeLegalizeOps() &&
            !DCI.isBeforeLegalize() && TLI.isOperationLegal(ISD::VSELECT, VT)) {
        unsigned BitWidth = Cond.getValueType().getScalarType().getSizeInBits();

        // Don't optimize vector selects that map to mask-registers.
        if (BitWidth == 1)
            return SDValue();

        // Check all uses of that condition operand to check whether it will be
        // consumed by non-BLEND instructions, which may depend on all bits are set
        // properly.
        for (SDNode::use_iterator I = Cond->use_begin(),
                E = Cond->use_end(); I != E; ++I)
            if (I->getOpcode() != ISD::VSELECT)
                // TODO: Add other opcodes eventually lowered into BLEND.
                return SDValue();

        assert(BitWidth >= 8 && BitWidth <= 64 && "Invalid mask size");
        APInt DemandedMask = APInt::getHighBitsSet(BitWidth, 1);

        APInt KnownZero, KnownOne;
        TargetLowering::TargetLoweringOpt TLO(DAG, DCI.isBeforeLegalize(),
                DCI.isBeforeLegalizeOps());
        if (TLO.ShrinkDemandedConstant(Cond, DemandedMask) ||
                TLI.SimplifyDemandedBits(Cond, DemandedMask, KnownZero, KnownOne, TLO))
            DCI.CommitTargetLoweringOpt(TLO);
    }

    return SDValue();
}

// Check whether a boolean test is testing a boolean value generated by
// Cse523ISD::SETCC. If so, return the operand of that SETCC and proper condition
// code.
//
// Simplify the following patterns:
// (Op (CMP (SETCC Cond EFLAGS) 1) EQ) or
// (Op (CMP (SETCC Cond EFLAGS) 0) NEQ)
// to (Op EFLAGS Cond)
//
// (Op (CMP (SETCC Cond EFLAGS) 0) EQ) or
// (Op (CMP (SETCC Cond EFLAGS) 1) NEQ)
// to (Op EFLAGS !Cond)
//
// where Op could be BRCOND or CMOV.
//
static SDValue checkBoolTestSetCCCombine(SDValue Cmp, Cse523::CondCode &CC) {
    // Quit if not CMP and SUB with its value result used.
    if (Cmp.getOpcode() != Cse523ISD::CMP &&
            (Cmp.getOpcode() != Cse523ISD::SUB || Cmp.getNode()->hasAnyUseOfValue(0)))
        return SDValue();

    // Quit if not used as a boolean value.
    if (CC != Cse523::COND_E && CC != Cse523::COND_NE)
        return SDValue();

    // Check CMP operands. One of them should be 0 or 1 and the other should be
    // an SetCC or extended from it.
    SDValue Op1 = Cmp.getOperand(0);
    SDValue Op2 = Cmp.getOperand(1);

    SDValue SetCC;
    const ConstantSDNode* C = 0;
    bool needOppositeCond = (CC == Cse523::COND_E);
    bool checkAgainstTrue = false; // Is it a comparison against 1?

    if ((C = dyn_cast<ConstantSDNode>(Op1)))
        SetCC = Op2;
    else if ((C = dyn_cast<ConstantSDNode>(Op2)))
        SetCC = Op1;
    else // Quit if all operands are not constants.
        return SDValue();

    if (C->getZExtValue() == 1) {
        needOppositeCond = !needOppositeCond;
        checkAgainstTrue = true;
    } else if (C->getZExtValue() != 0)
        // Quit if the constant is neither 0 or 1.
        return SDValue();

    bool truncatedToBoolWithAnd = false;
    // Skip (zext $x), (trunc $x), or (and $x, 1) node.
    while (SetCC.getOpcode() == ISD::ZERO_EXTEND ||
            SetCC.getOpcode() == ISD::TRUNCATE ||
            SetCC.getOpcode() == ISD::AND) {
        if (SetCC.getOpcode() == ISD::AND) {
            int OpIdx = -1;
            ConstantSDNode *CS;
            if ((CS = dyn_cast<ConstantSDNode>(SetCC.getOperand(0))) &&
                    CS->getZExtValue() == 1)
                OpIdx = 1;
            if ((CS = dyn_cast<ConstantSDNode>(SetCC.getOperand(1))) &&
                    CS->getZExtValue() == 1)
                OpIdx = 0;
            if (OpIdx == -1)
                break;
            SetCC = SetCC.getOperand(OpIdx);
            truncatedToBoolWithAnd = true;
        } else
            SetCC = SetCC.getOperand(0);
    }

    switch (SetCC.getOpcode()) {
        case Cse523ISD::SETCC_CARRY:
            // Since SETCC_CARRY gives output based on R = CF ? ~0 : 0, it's unsafe to
            // simplify it if the result of SETCC_CARRY is not canonicalized to 0 or 1,
            // i.e. it's a comparison against true but the result of SETCC_CARRY is not
            // truncated to i1 using 'and'.
            if (checkAgainstTrue && !truncatedToBoolWithAnd)
                break;
            assert(Cse523::CondCode(SetCC.getConstantOperandVal(0)) == Cse523::COND_B &&
                    "Invalid use of SETCC_CARRY!");
            // FALL THROUGH
        case Cse523ISD::SETCC:
            // Set the condition code or opposite one if necessary.
            CC = Cse523::CondCode(SetCC.getConstantOperandVal(0));
            if (needOppositeCond)
                CC = Cse523::GetOppositeBranchCondition(CC);
            return SetCC.getOperand(1);
        case Cse523ISD::CMOV: {
                                  // Check whether false/true value has canonical one, i.e. 0 or 1.
                                  ConstantSDNode *FVal = dyn_cast<ConstantSDNode>(SetCC.getOperand(0));
                                  ConstantSDNode *TVal = dyn_cast<ConstantSDNode>(SetCC.getOperand(1));
                                  // Quit if true value is not a constant.
                                  if (!TVal)
                                      return SDValue();
                                  // Quit if false value is not a constant.
                                  if (!FVal) {
                                      SDValue Op = SetCC.getOperand(0);
                                      // Skip 'zext' or 'trunc' node.
                                      if (Op.getOpcode() == ISD::ZERO_EXTEND ||
                                              Op.getOpcode() == ISD::TRUNCATE)
                                          Op = Op.getOperand(0);
                                      // A special case for rdrand/rdseed, where 0 is set if false cond is
                                      // found.
                                      if ((Op.getOpcode() != Cse523ISD::RDRAND &&
                                                  Op.getOpcode() != Cse523ISD::RDSEED) || Op.getResNo() != 0)
                                          return SDValue();
                                  }
                                  // Quit if false value is not the constant 0 or 1.
                                  bool FValIsFalse = true;
                                  if (FVal && FVal->getZExtValue() != 0) {
                                      if (FVal->getZExtValue() != 1)
                                          return SDValue();
                                      // If FVal is 1, opposite cond is needed.
                                      needOppositeCond = !needOppositeCond;
                                      FValIsFalse = false;
                                  }
                                  // Quit if TVal is not the constant opposite of FVal.
                                  if (FValIsFalse && TVal->getZExtValue() != 1)
                                      return SDValue();
                                  if (!FValIsFalse && TVal->getZExtValue() != 0)
                                      return SDValue();
                                  CC = Cse523::CondCode(SetCC.getConstantOperandVal(2));
                                  if (needOppositeCond)
                                      CC = Cse523::GetOppositeBranchCondition(CC);
                                  return SetCC.getOperand(3);
                              }
    }

    return SDValue();
}

/// Optimize Cse523ISD::CMOV [LHS, RHS, CONDCODE (e.g. Cse523::COND_NE), CONDVAL]
static SDValue PerformCMOVCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    SDLoc DL(N);

    // If the flag operand isn't dead, don't touch this CMOV.
    if (N->getNumValues() == 2 && !SDValue(N, 1).use_empty())
        return SDValue();

    SDValue FalseOp = N->getOperand(0);
    SDValue TrueOp = N->getOperand(1);
    Cse523::CondCode CC = (Cse523::CondCode)N->getConstantOperandVal(2);
    SDValue Cond = N->getOperand(3);

    if (CC == Cse523::COND_E || CC == Cse523::COND_NE) {
        switch (Cond.getOpcode()) {
            default: break;
            case Cse523ISD::BSR:
            case Cse523ISD::BSF:
                     // If operand of BSR / BSF are proven never zero, then ZF cannot be set.
                     if (DAG.isKnownNeverZero(Cond.getOperand(0)))
                         return (CC == Cse523::COND_E) ? FalseOp : TrueOp;
        }
    }

    SDValue Flags;

    Flags = checkBoolTestSetCCCombine(Cond, CC);
    if (Flags.getNode() &&
            // Extra check as FCMOV only supports a subset of Cse523 cond.
            (FalseOp.getValueType() != MVT::f80 || hasFPCMov(CC))) {
        SDValue Ops[] = { FalseOp, TrueOp,
            DAG.getConstant(CC, MVT::i64), Flags };
        return DAG.getNode(Cse523ISD::CMOV, DL, N->getVTList(),
                Ops, array_lengthof(Ops));
    }

    // If this is a select between two integer constants, try to do some
    // optimizations.  Note that the operands are ordered the opposite of SELECT
    // operands.
    if (ConstantSDNode *TrueC = dyn_cast<ConstantSDNode>(TrueOp)) {
        if (ConstantSDNode *FalseC = dyn_cast<ConstantSDNode>(FalseOp)) {
            // Canonicalize the TrueC/FalseC values so that TrueC (the true value) is
            // larger than FalseC (the false value).
            if (TrueC->getAPIntValue().ult(FalseC->getAPIntValue())) {
                CC = Cse523::GetOppositeBranchCondition(CC);
                std::swap(TrueC, FalseC);
                std::swap(TrueOp, FalseOp);
            }

            // Optimize C ? 8 : 0 -> zext(setcc(C)) << 3.  Likewise for any pow2/0.
            // This is efficient for any integer data type (including i8/i16) and
            // shift amount.
            if (FalseC->getAPIntValue() == 0 && TrueC->getAPIntValue().isPowerOf2()) {
                Cond = DAG.getNode(Cse523ISD::SETCC, DL, MVT::i64,
                        DAG.getConstant(CC, MVT::i64), Cond);

                // Zero extend the condition if needed.
                Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, TrueC->getValueType(0), Cond);

                unsigned ShAmt = TrueC->getAPIntValue().logBase2();
                Cond = DAG.getNode(ISD::SHL, DL, Cond.getValueType(), Cond,
                        DAG.getConstant(ShAmt, MVT::i64));
                if (N->getNumValues() == 2)  // Dead flag value?
                    return DCI.CombineTo(N, Cond, SDValue());
                return Cond;
            }

            // Optimize Cond ? cst+1 : cst -> zext(setcc(C)+cst.  This is efficient
            // for any integer data type, including i8/i16.
            if (FalseC->getAPIntValue()+1 == TrueC->getAPIntValue()) {
                Cond = DAG.getNode(Cse523ISD::SETCC, DL, MVT::i64,
                        DAG.getConstant(CC, MVT::i64), Cond);

                // Zero extend the condition if needed.
                Cond = DAG.getNode(ISD::ZERO_EXTEND, DL,
                        FalseC->getValueType(0), Cond);
                Cond = DAG.getNode(ISD::ADD, DL, Cond.getValueType(), Cond,
                        SDValue(FalseC, 0));

                if (N->getNumValues() == 2)  // Dead flag value?
                    return DCI.CombineTo(N, Cond, SDValue());
                return Cond;
            }

            // Optimize cases that will turn into an LEA instruction.  This requires
            // an i32 or i64 and an efficient multiplier (1, 2, 3, 4, 5, 8, 9).
            if (N->getValueType(0) == MVT::i32 || N->getValueType(0) == MVT::i64) {
                uint64_t Diff = TrueC->getZExtValue()-FalseC->getZExtValue();
                if (N->getValueType(0) == MVT::i32) Diff = (unsigned)Diff;

                bool isFastMultiplier = false;
                if (Diff < 10) {
                    switch ((unsigned char)Diff) {
                        default: break;
                        case 1:  // result = add base, cond
                        case 2:  // result = lea base(    , cond*2)
                        case 3:  // result = lea base(cond, cond*2)
                        case 4:  // result = lea base(    , cond*4)
                        case 5:  // result = lea base(cond, cond*4)
                        case 8:  // result = lea base(    , cond*8)
                        case 9:  // result = lea base(cond, cond*8)
                                 isFastMultiplier = true;
                                 break;
                    }
                }

                if (isFastMultiplier) {
                    APInt Diff = TrueC->getAPIntValue()-FalseC->getAPIntValue();
                    Cond = DAG.getNode(Cse523ISD::SETCC, DL, MVT::i64,
                            DAG.getConstant(CC, MVT::i64), Cond);
                    // Zero extend the condition if needed.
                    Cond = DAG.getNode(ISD::ZERO_EXTEND, DL, FalseC->getValueType(0),
                            Cond);
                    // Scale the condition by the difference.
                    if (Diff != 1)
                        Cond = DAG.getNode(ISD::MUL, DL, Cond.getValueType(), Cond,
                                DAG.getConstant(Diff, Cond.getValueType()));

                    // Add the base if non-zero.
                    if (FalseC->getAPIntValue() != 0)
                        Cond = DAG.getNode(ISD::ADD, DL, Cond.getValueType(), Cond,
                                SDValue(FalseC, 0));
                    if (N->getNumValues() == 2)  // Dead flag value?
                        return DCI.CombineTo(N, Cond, SDValue());
                    return Cond;
                }
            }
        }
    }

    // Handle these cases:
    //   (select (x != c), e, c) -> select (x != c), e, x),
    //   (select (x == c), c, e) -> select (x == c), x, e)
    // where the c is an integer constant, and the "select" is the combination
    // of CMOV and CMP.
    //
    // The rationale for this change is that the conditional-move from a constant
    // needs two instructions, however, conditional-move from a register needs
    // only one instruction.
    //
    // CAVEAT: By replacing a constant with a symbolic value, it may obscure
    //  some instruction-combining opportunities. This opt needs to be
    //  postponed as late as possible.
    //
    if (!DCI.isBeforeLegalize() && !DCI.isBeforeLegalizeOps()) {
        // the DCI.xxxx conditions are provided to postpone the optimization as
        // late as possible.

        ConstantSDNode *CmpAgainst = 0;
        if ((Cond.getOpcode() == Cse523ISD::CMP || Cond.getOpcode() == Cse523ISD::SUB) &&
                (CmpAgainst = dyn_cast<ConstantSDNode>(Cond.getOperand(1))) &&
                !isa<ConstantSDNode>(Cond.getOperand(0))) {

            if (CC == Cse523::COND_NE &&
                    CmpAgainst == dyn_cast<ConstantSDNode>(FalseOp)) {
                CC = Cse523::GetOppositeBranchCondition(CC);
                std::swap(TrueOp, FalseOp);
            }

            if (CC == Cse523::COND_E &&
                    CmpAgainst == dyn_cast<ConstantSDNode>(TrueOp)) {
                SDValue Ops[] = { FalseOp, Cond.getOperand(0),
                    DAG.getConstant(CC, MVT::i64), Cond };
                return DAG.getNode(Cse523ISD::CMOV, DL, N->getVTList (), Ops,
                        array_lengthof(Ops));
            }
        }
    }

    return SDValue();
}

/// PerformMulCombine - Optimize a single multiply with constant into two
/// in order to implement it with two cheaper instructions, e.g.
/// LEA + SHL, LEA + LEA.
static SDValue PerformMulCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI) {
    if (DCI.isBeforeLegalize() || DCI.isCalledByLegalizer())
        return SDValue();

    EVT VT = N->getValueType(0);
    if (VT != MVT::i64)
        return SDValue();

    ConstantSDNode *C = dyn_cast<ConstantSDNode>(N->getOperand(1));
    if (!C)
        return SDValue();
    uint64_t MulAmt = C->getZExtValue();
    if (isPowerOf2_64(MulAmt) || MulAmt == 3 || MulAmt == 5 || MulAmt == 9)
        return SDValue();

    uint64_t MulAmt1 = 0;
    uint64_t MulAmt2 = 0;
    if ((MulAmt % 9) == 0) {
        MulAmt1 = 9;
        MulAmt2 = MulAmt / 9;
    } else if ((MulAmt % 5) == 0) {
        MulAmt1 = 5;
        MulAmt2 = MulAmt / 5;
    } else if ((MulAmt % 3) == 0) {
        MulAmt1 = 3;
        MulAmt2 = MulAmt / 3;
    }
    if (MulAmt2 &&
            (isPowerOf2_64(MulAmt2) || MulAmt2 == 3 || MulAmt2 == 5 || MulAmt2 == 9)){
        SDLoc DL(N);

        if (isPowerOf2_64(MulAmt2) &&
                !(N->hasOneUse() && N->use_begin()->getOpcode() == ISD::ADD))
            // If second multiplifer is pow2, issue it first. We want the multiply by
            // 3, 5, or 9 to be folded into the addressing mode unless the lone use
            // is an add.
            std::swap(MulAmt1, MulAmt2);

        SDValue NewMul;
        if (isPowerOf2_64(MulAmt1))
            NewMul = DAG.getNode(ISD::SHL, DL, VT, N->getOperand(0),
                    DAG.getConstant(Log2_64(MulAmt1), MVT::i64));
        else
            NewMul = DAG.getNode(Cse523ISD::MUL_IMM, DL, VT, N->getOperand(0),
                    DAG.getConstant(MulAmt1, VT));

        if (isPowerOf2_64(MulAmt2))
            NewMul = DAG.getNode(ISD::SHL, DL, VT, NewMul,
                    DAG.getConstant(Log2_64(MulAmt2), MVT::i64));
        else
            NewMul = DAG.getNode(Cse523ISD::MUL_IMM, DL, VT, NewMul,
                    DAG.getConstant(MulAmt2, VT));

        // Do not add new nodes to DAG combiner worklist.
        DCI.CombineTo(N, NewMul, false);
    }
    return SDValue();
}

static SDValue PerformSHLCombine(SDNode *N, SelectionDAG &DAG) {
    SDValue N0 = N->getOperand(0);
    SDValue N1 = N->getOperand(1);
    ConstantSDNode *N1C = dyn_cast<ConstantSDNode>(N1);
    EVT VT = N0.getValueType();

    // fold (shl (and (setcc_c), c1), c2) -> (and setcc_c, (c1 << c2))
    // since the result of setcc_c is all zero's or all ones.
    if (VT.isInteger() && !VT.isVector() &&
            N1C && N0.getOpcode() == ISD::AND &&
            N0.getOperand(1).getOpcode() == ISD::Constant) {
        SDValue N00 = N0.getOperand(0);
        if (N00.getOpcode() == Cse523ISD::SETCC_CARRY ||
                ((N00.getOpcode() == ISD::ANY_EXTEND ||
                  N00.getOpcode() == ISD::ZERO_EXTEND) &&
                 N00.getOperand(0).getOpcode() == Cse523ISD::SETCC_CARRY)) {
            APInt Mask = cast<ConstantSDNode>(N0.getOperand(1))->getAPIntValue();
            APInt ShAmt = N1C->getAPIntValue();
            Mask = Mask.shl(ShAmt);
            if (Mask != 0)
                return DAG.getNode(ISD::AND, SDLoc(N), VT,
                        N00, DAG.getConstant(Mask, VT));
        }
    }

    // Hardware support for vector shifts is sparse which makes us scalarize the
    // vector operations in many cases. Also, on sandybridge ADD is faster than
    // shl.
    // (shl V, 1) -> add V,V
    if (isSplatVector(N1.getNode())) {
        assert(N0.getValueType().isVector() && "Invalid vector shift type");
        ConstantSDNode *N1C = dyn_cast<ConstantSDNode>(N1->getOperand(0));
        // We shift all of the values by one. In many cases we do not have
        // hardware support for this operation. This is better expressed as an ADD
        // of two values.
        if (N1C && (1 == N1C->getZExtValue())) {
            return DAG.getNode(ISD::ADD, SDLoc(N), VT, N0, N0);
        }
    }

    return SDValue();
}

/// \brief Returns a vector of 0s if the node in input is a vector logical
/// shift by a constant amount which is known to be bigger than or equal
/// to the vector element size in bits.
static SDValue performShiftToAllZeros(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);

    if (VT != MVT::v2i64 && VT != MVT::v4i32 && VT != MVT::v8i16 &&
            (!Subtarget->hasInt256() ||
             (VT != MVT::v4i64 && VT != MVT::v8i32 && VT != MVT::v16i16)))
        return SDValue();

    SDValue Amt = N->getOperand(1);
    SDLoc DL(N);
    if (isSplatVector(Amt.getNode())) {
        SDValue SclrAmt = Amt->getOperand(0);
        if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(SclrAmt)) {
            APInt ShiftAmt = C->getAPIntValue();
            unsigned MaxAmount = VT.getVectorElementType().getSizeInBits();

            // SSE2/AVX2 logical shifts always return a vector of 0s
            // if the shift amount is bigger than or equal to
            // the element size. The constant shift amount will be
            // encoded as a 8-bit immediate.
            if (ShiftAmt.trunc(8).uge(MaxAmount))
                return getZeroVector(VT, Subtarget, DAG, DL);
        }
    }

    return SDValue();
}

/// PerformShiftCombine - Combine shifts.
static SDValue PerformShiftCombine(SDNode* N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    if (N->getOpcode() == ISD::SHL) {
        SDValue V = PerformSHLCombine(N, DAG);
        if (V.getNode()) return V;
    }

    if (N->getOpcode() != ISD::SRA) {
        // Try to fold this logical shift into a zero vector.
        SDValue V = performShiftToAllZeros(N, DAG, Subtarget);
        if (V.getNode()) return V;
    }

    return SDValue();
}

// CMPEQCombine - Recognize the distinctive  (AND (setcc ...) (setcc ..))
// where both setccs reference the same FP CMP, and rewrite for CMPEQSS
// and friends.  Likewise for OR -> CMPNEQSS.
static SDValue CMPEQCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    unsigned opcode;

    // SSE1 supports CMP{eq|ne}SS, and SSE2 added CMP{eq|ne}SD, but
    // we're requiring SSE2 for both.
    if (Subtarget->hasSSE2() && isAndOrOfSetCCs(SDValue(N, 0U), opcode)) {
        SDValue N0 = N->getOperand(0);
        SDValue N1 = N->getOperand(1);
        SDValue CMP0 = N0->getOperand(1);
        SDValue CMP1 = N1->getOperand(1);
        SDLoc DL(N);

        // The SETCCs should both refer to the same CMP.
        if (CMP0.getOpcode() != Cse523ISD::CMP || CMP0 != CMP1)
            return SDValue();

        SDValue CMP00 = CMP0->getOperand(0);
        SDValue CMP01 = CMP0->getOperand(1);
        EVT     VT    = CMP00.getValueType();

        if (VT == MVT::f32 || VT == MVT::f64) {
            bool ExpectingFlags = false;
            // Check for any users that want flags:
            for (SDNode::use_iterator UI = N->use_begin(), UE = N->use_end();
                    !ExpectingFlags && UI != UE; ++UI)
                switch (UI->getOpcode()) {
                    default:
                    case ISD::BR_CC:
                    case ISD::BRCOND:
                    case ISD::SELECT:
                        ExpectingFlags = true;
                        break;
                    case ISD::CopyToReg:
                    case ISD::SIGN_EXTEND:
                    case ISD::ZERO_EXTEND:
                    case ISD::ANY_EXTEND:
                        break;
                }

            if (!ExpectingFlags) {
                enum Cse523::CondCode cc0 = (enum Cse523::CondCode)N0.getConstantOperandVal(0);
                enum Cse523::CondCode cc1 = (enum Cse523::CondCode)N1.getConstantOperandVal(0);

                if (cc1 == Cse523::COND_E || cc1 == Cse523::COND_NE) {
                    Cse523::CondCode tmp = cc0;
                    cc0 = cc1;
                    cc1 = tmp;
                }

                if ((cc0 == Cse523::COND_E  && cc1 == Cse523::COND_NP) ||
                        (cc0 == Cse523::COND_NE && cc1 == Cse523::COND_P)) {
                    bool is64BitFP = (CMP00.getValueType() == MVT::f64);
                    // FIXME: need symbolic constants for these magic numbers.
                    // See Cse523ATTInstPrinter.cpp:printSSECC().
                    unsigned cse523cc = (cc0 == Cse523::COND_E) ? 0 : 4;
                    SDValue OnesOrZeroesF = DAG.getNode(Cse523ISD::FSETCC, DL,
                            CMP00.getValueType(), CMP00, CMP01,
                            DAG.getConstant(cse523cc, MVT::i8));
                    MVT IntVT = (is64BitFP ? MVT::i64 : MVT::i32); 
                    SDValue OnesOrZeroesI = DAG.getNode(ISD::BITCAST, DL, IntVT,
                            OnesOrZeroesF);
                    SDValue ANDed = DAG.getNode(ISD::AND, DL, IntVT, OnesOrZeroesI,
                            DAG.getConstant(1, IntVT));
                    SDValue OneBitOfTruth = DAG.getNode(ISD::TRUNCATE, DL, MVT::i8, ANDed);
                    return OneBitOfTruth;
                }
            }
        }
    }
    return SDValue();
}

/// CanFoldXORWithAllOnes - Test whether the XOR operand is a AllOnes vector
/// so it can be folded inside ANDNP.
static bool CanFoldXORWithAllOnes(const SDNode *N) {
    EVT VT = N->getValueType(0);

    // Match direct AllOnes for 128 and 256-bit vectors
    if (ISD::isBuildVectorAllOnes(N))
        return true;

    // Look through a bit convert.
    if (N->getOpcode() == ISD::BITCAST)
        N = N->getOperand(0).getNode();

    // Sometimes the operand may come from a insert_subvector building a 256-bit
    // allones vector
    if (VT.is256BitVector() &&
            N->getOpcode() == ISD::INSERT_SUBVECTOR) {
        SDValue V1 = N->getOperand(0);
        SDValue V2 = N->getOperand(1);

        if (V1.getOpcode() == ISD::INSERT_SUBVECTOR &&
                V1.getOperand(0).getOpcode() == ISD::UNDEF &&
                ISD::isBuildVectorAllOnes(V1.getOperand(1).getNode()) &&
                ISD::isBuildVectorAllOnes(V2.getNode()))
            return true;
    }

    return false;
}

// On AVX/AVX2 the type v8i1 is legalized to v8i16, which is an XMM sized
// register. In most cases we actually compare or select YMM-sized registers
// and mixing the two types creates horrible code. This method optimizes
// some of the transition sequences.
static SDValue WidenMaskArithmetic(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    if (!VT.is256BitVector())
        return SDValue();

    assert((N->getOpcode() == ISD::ANY_EXTEND ||
                N->getOpcode() == ISD::ZERO_EXTEND ||
                N->getOpcode() == ISD::SIGN_EXTEND) && "Invalid Node");

    SDValue Narrow = N->getOperand(0);
    EVT NarrowVT = Narrow->getValueType(0);
    if (!NarrowVT.is128BitVector())
        return SDValue();

    if (Narrow->getOpcode() != ISD::XOR &&
            Narrow->getOpcode() != ISD::AND &&
            Narrow->getOpcode() != ISD::OR)
        return SDValue();

    SDValue N0  = Narrow->getOperand(0);
    SDValue N1  = Narrow->getOperand(1);
    SDLoc DL(Narrow);

    // The Left side has to be a trunc.
    if (N0.getOpcode() != ISD::TRUNCATE)
        return SDValue();

    // The type of the truncated inputs.
    EVT WideVT = N0->getOperand(0)->getValueType(0);
    if (WideVT != VT)
        return SDValue();

    // The right side has to be a 'trunc' or a constant vector.
    bool RHSTrunc = N1.getOpcode() == ISD::TRUNCATE;
    bool RHSConst = (isSplatVector(N1.getNode()) &&
            isa<ConstantSDNode>(N1->getOperand(0)));
    if (!RHSTrunc && !RHSConst)
        return SDValue();

    const TargetLowering &TLI = DAG.getTargetLoweringInfo();

    if (!TLI.isOperationLegalOrPromote(Narrow->getOpcode(), WideVT))
        return SDValue();

    // Set N0 and N1 to hold the inputs to the new wide operation.
    N0 = N0->getOperand(0);
    if (RHSConst) {
        N1 = DAG.getNode(ISD::ZERO_EXTEND, DL, WideVT.getScalarType(),
                N1->getOperand(0));
        SmallVector<SDValue, 8> C(WideVT.getVectorNumElements(), N1);
        N1 = DAG.getNode(ISD::BUILD_VECTOR, DL, WideVT, &C[0], C.size());
    } else if (RHSTrunc) {
        N1 = N1->getOperand(0);
    }

    // Generate the wide operation.
    SDValue Op = DAG.getNode(Narrow->getOpcode(), DL, WideVT, N0, N1);
    unsigned Opcode = N->getOpcode();
    switch (Opcode) {
        case ISD::ANY_EXTEND:
            return Op;
        case ISD::ZERO_EXTEND: {
                                   unsigned InBits = NarrowVT.getScalarType().getSizeInBits();
                                   APInt Mask = APInt::getAllOnesValue(InBits);
                                   Mask = Mask.zext(VT.getScalarType().getSizeInBits());
                                   return DAG.getNode(ISD::AND, DL, VT,
                                           Op, DAG.getConstant(Mask, VT));
                               }
        case ISD::SIGN_EXTEND:
                               return DAG.getNode(ISD::SIGN_EXTEND_INREG, DL, VT,
                                       Op, DAG.getValueType(NarrowVT));
        default:
                               llvm_unreachable("Unexpected opcode");
    }
}

static SDValue PerformAndCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    if (DCI.isBeforeLegalizeOps())
        return SDValue();

    SDValue R = CMPEQCombine(N, DAG, DCI, Subtarget);
    if (R.getNode())
        return R;

    // Create BEXTR and BZHI instructions
    // BZHI is X & ((1 << Y) - 1)
    // BEXTR is ((X >> imm) & (2**size-1))
    if (VT == MVT::i32 || VT == MVT::i64) {
        SDValue N0 = N->getOperand(0);
        SDValue N1 = N->getOperand(1);
        SDLoc DL(N);

        if (Subtarget->hasBMI2()) {
            // Check for (and (add (shl 1, Y), -1), X)
            if (N0.getOpcode() == ISD::ADD && isAllOnes(N0.getOperand(1))) {
                SDValue N00 = N0.getOperand(0);
                if (N00.getOpcode() == ISD::SHL) {
                    SDValue N001 = N00.getOperand(1);
                    assert(N001.getValueType() == MVT::i64 && "unexpected type");
                    ConstantSDNode *C = dyn_cast<ConstantSDNode>(N00.getOperand(0));
                    if (C && C->getZExtValue() == 1)
                        return DAG.getNode(Cse523ISD::BZHI, DL, VT, N1, N001);
                }
            }

            // Check for (and X, (add (shl 1, Y), -1))
            if (N1.getOpcode() == ISD::ADD && isAllOnes(N1.getOperand(1))) {
                SDValue N10 = N1.getOperand(0);
                if (N10.getOpcode() == ISD::SHL) {
                    SDValue N101 = N10.getOperand(1);
                    assert(N101.getValueType() == MVT::i64 && "unexpected type");
                    ConstantSDNode *C = dyn_cast<ConstantSDNode>(N10.getOperand(0));
                    if (C && C->getZExtValue() == 1)
                        return DAG.getNode(Cse523ISD::BZHI, DL, VT, N0, N101);
                }
            }
        }

        // Check for BEXTR.
        if ((Subtarget->hasBMI() || Subtarget->hasTBM()) &&
                (N0.getOpcode() == ISD::SRA || N0.getOpcode() == ISD::SRL)) {
            ConstantSDNode *MaskNode = dyn_cast<ConstantSDNode>(N1);
            ConstantSDNode *ShiftNode = dyn_cast<ConstantSDNode>(N0.getOperand(1));
            if (MaskNode && ShiftNode) {
                uint64_t Mask = MaskNode->getZExtValue();
                uint64_t Shift = ShiftNode->getZExtValue();
                if (isMask_64(Mask)) {
                    uint64_t MaskSize = CountPopulation_64(Mask);
                    if (Shift + MaskSize <= VT.getSizeInBits())
                        return DAG.getNode(Cse523ISD::BEXTR, DL, VT, N0.getOperand(0),
                                DAG.getConstant(Shift | (MaskSize << 8), VT));
                }
            }
        } // BEXTR

        return SDValue();
    }

    // Want to form ANDNP nodes:
    // 1) In the hopes of then easily combining them with OR and AND nodes
    //    to form PBLEND/PSIGN.
    // 2) To match ANDN packed intrinsics
    if (VT != MVT::v2i64 && VT != MVT::v4i64)
        return SDValue();

    SDValue N0 = N->getOperand(0);
    SDValue N1 = N->getOperand(1);
    SDLoc DL(N);

    // Check LHS for vnot
    if (N0.getOpcode() == ISD::XOR &&
            //ISD::isBuildVectorAllOnes(N0.getOperand(1).getNode()))
        CanFoldXORWithAllOnes(N0.getOperand(1).getNode()))
            return DAG.getNode(Cse523ISD::ANDNP, DL, VT, N0.getOperand(0), N1);

    // Check RHS for vnot
    if (N1.getOpcode() == ISD::XOR &&
            //ISD::isBuildVectorAllOnes(N1.getOperand(1).getNode()))
        CanFoldXORWithAllOnes(N1.getOperand(1).getNode()))
            return DAG.getNode(Cse523ISD::ANDNP, DL, VT, N1.getOperand(0), N0);

    return SDValue();
}

static SDValue PerformOrCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    if (DCI.isBeforeLegalizeOps())
        return SDValue();

    SDValue R = CMPEQCombine(N, DAG, DCI, Subtarget);
    if (R.getNode())
        return R;

    SDValue N0 = N->getOperand(0);
    SDValue N1 = N->getOperand(1);
    EVT VT = N->getValueType(0);

    // look for psign/blend
    if (VT == MVT::v2i64 || VT == MVT::v4i64) {
        if (!Subtarget->hasSSSE3() ||
                (VT == MVT::v4i64 && !Subtarget->hasInt256()))
            return SDValue();

        // Canonicalize pandn to RHS
        if (N0.getOpcode() == Cse523ISD::ANDNP)
            std::swap(N0, N1);
        // or (and (m, y), (pandn m, x))
        if (N0.getOpcode() == ISD::AND && N1.getOpcode() == Cse523ISD::ANDNP) {
            SDValue Mask = N1.getOperand(0);
            SDValue X    = N1.getOperand(1);
            SDValue Y;
            if (N0.getOperand(0) == Mask)
                Y = N0.getOperand(1);
            if (N0.getOperand(1) == Mask)
                Y = N0.getOperand(0);

            // Check to see if the mask appeared in both the AND and ANDNP and
            if (!Y.getNode())
                return SDValue();

            // Validate that X, Y, and Mask are BIT_CONVERTS, and see through them.
            // Look through mask bitcast.
            if (Mask.getOpcode() == ISD::BITCAST)
                Mask = Mask.getOperand(0);
            if (X.getOpcode() == ISD::BITCAST)
                X = X.getOperand(0);
            if (Y.getOpcode() == ISD::BITCAST)
                Y = Y.getOperand(0);

            EVT MaskVT = Mask.getValueType();

            // Validate that the Mask operand is a vector sra node.
            // FIXME: what to do for bytes, since there is a psignb/pblendvb, but
            // there is no psrai.b
            unsigned EltBits = MaskVT.getVectorElementType().getSizeInBits();
            unsigned SraAmt = ~0;
            if (Mask.getOpcode() == ISD::SRA) {
                SDValue Amt = Mask.getOperand(1);
                if (isSplatVector(Amt.getNode())) {
                    SDValue SclrAmt = Amt->getOperand(0);
                    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(SclrAmt))
                        SraAmt = C->getZExtValue();
                }
            } else if (Mask.getOpcode() == Cse523ISD::VSRAI) {
                SDValue SraC = Mask.getOperand(1);
                SraAmt  = cast<ConstantSDNode>(SraC)->getZExtValue();
            }
            if ((SraAmt + 1) != EltBits)
                return SDValue();

            SDLoc DL(N);

            // Now we know we at least have a plendvb with the mask val.  See if
            // we can form a psignb/w/d.
            // psign = x.type == y.type == mask.type && y = sub(0, x);
            if (Y.getOpcode() == ISD::SUB && Y.getOperand(1) == X &&
                    ISD::isBuildVectorAllZeros(Y.getOperand(0).getNode()) &&
                    X.getValueType() == MaskVT && Y.getValueType() == MaskVT) {
                assert((EltBits == 8 || EltBits == 16 || EltBits == 32) &&
                        "Unsupported VT for PSIGN");
                Mask = DAG.getNode(Cse523ISD::PSIGN, DL, MaskVT, X, Mask.getOperand(0));
                return DAG.getNode(ISD::BITCAST, DL, VT, Mask);
            }
            // PBLENDVB only available on SSE 4.1
            if (!Subtarget->hasSSE41())
                return SDValue();

            EVT BlendVT = (VT == MVT::v4i64) ? MVT::v32i8 : MVT::v16i8;

            X = DAG.getNode(ISD::BITCAST, DL, BlendVT, X);
            Y = DAG.getNode(ISD::BITCAST, DL, BlendVT, Y);
            Mask = DAG.getNode(ISD::BITCAST, DL, BlendVT, Mask);
            Mask = DAG.getNode(ISD::VSELECT, DL, BlendVT, Mask, Y, X);
            return DAG.getNode(ISD::BITCAST, DL, VT, Mask);
        }
    }

    if (VT != MVT::i16 && VT != MVT::i32 && VT != MVT::i64)
        return SDValue();

    // fold (or (x << c) | (y >> (64 - c))) ==> (shld64 x, y, c)
    MachineFunction &MF = DAG.getMachineFunction();
    bool OptForSize = MF.getFunction()->getAttributes().
        hasAttribute(AttributeSet::FunctionIndex, Attribute::OptimizeForSize);

    // SHLD/SHRD instructions have lower register pressure, but on some
    // platforms they have higher latency than the equivalent
    // series of shifts/or that would otherwise be generated.
    // Don't fold (or (x << c) | (y >> (64 - c))) if SHLD/SHRD instructions
    // have higher latencies and we are not optimizing for size.
    if (!OptForSize && Subtarget->isSHLDSlow())
        return SDValue();

    if (N0.getOpcode() == ISD::SRL && N1.getOpcode() == ISD::SHL)
        std::swap(N0, N1);
    if (N0.getOpcode() != ISD::SHL || N1.getOpcode() != ISD::SRL)
        return SDValue();
    if (!N0.hasOneUse() || !N1.hasOneUse())
        return SDValue();

    SDValue ShAmt0 = N0.getOperand(1);
    if (ShAmt0.getValueType() != MVT::i8)
        return SDValue();
    SDValue ShAmt1 = N1.getOperand(1);
    if (ShAmt1.getValueType() != MVT::i8)
        return SDValue();
    if (ShAmt0.getOpcode() == ISD::TRUNCATE)
        ShAmt0 = ShAmt0.getOperand(0);
    if (ShAmt1.getOpcode() == ISD::TRUNCATE)
        ShAmt1 = ShAmt1.getOperand(0);

    SDLoc DL(N);
    unsigned Opc = Cse523ISD::SHLD;
    SDValue Op0 = N0.getOperand(0);
    SDValue Op1 = N1.getOperand(0);
    if (ShAmt0.getOpcode() == ISD::SUB) {
        Opc = Cse523ISD::SHRD;
        std::swap(Op0, Op1);
        std::swap(ShAmt0, ShAmt1);
    }

    unsigned Bits = VT.getSizeInBits();
    if (ShAmt1.getOpcode() == ISD::SUB) {
        SDValue Sum = ShAmt1.getOperand(0);
        if (ConstantSDNode *SumC = dyn_cast<ConstantSDNode>(Sum)) {
            SDValue ShAmt1Op1 = ShAmt1.getOperand(1);
            if (ShAmt1Op1.getNode()->getOpcode() == ISD::TRUNCATE)
                ShAmt1Op1 = ShAmt1Op1.getOperand(0);
            if (SumC->getSExtValue() == Bits && ShAmt1Op1 == ShAmt0)
                return DAG.getNode(Opc, DL, VT,
                        Op0, Op1,
                        DAG.getNode(ISD::TRUNCATE, DL,
                            MVT::i8, ShAmt0));
        }
    } else if (ConstantSDNode *ShAmt1C = dyn_cast<ConstantSDNode>(ShAmt1)) {
        ConstantSDNode *ShAmt0C = dyn_cast<ConstantSDNode>(ShAmt0);
        if (ShAmt0C &&
                ShAmt0C->getSExtValue() + ShAmt1C->getSExtValue() == Bits)
            return DAG.getNode(Opc, DL, VT,
                    N0.getOperand(0), N1.getOperand(0),
                    DAG.getNode(ISD::TRUNCATE, DL,
                        MVT::i8, ShAmt0));
    }

    return SDValue();
}

// Generate NEG and CMOV for integer abs.
static SDValue performIntegerAbsCombine(SDNode *N, SelectionDAG &DAG) {
    EVT VT = N->getValueType(0);

    // Since Cse523 does not have CMOV for 8-bit integer, we don't convert
    // 8-bit integer abs to NEG and CMOV.
    if (VT.isInteger() && VT.getSizeInBits() == 8)
        return SDValue();

    SDValue N0 = N->getOperand(0);
    SDValue N1 = N->getOperand(1);
    SDLoc DL(N);

    // Check pattern of XOR(ADD(X,Y), Y) where Y is SRA(X, size(X)-1)
    // and change it to SUB and CMOV.
    if (VT.isInteger() && N->getOpcode() == ISD::XOR &&
            N0.getOpcode() == ISD::ADD &&
            N0.getOperand(1) == N1 &&
            N1.getOpcode() == ISD::SRA &&
            N1.getOperand(0) == N0.getOperand(0))
        if (ConstantSDNode *Y1C = dyn_cast<ConstantSDNode>(N1.getOperand(1)))
            if (Y1C->getAPIntValue() == VT.getSizeInBits()-1) {
                // Generate SUB & CMOV.
                SDValue Neg = DAG.getNode(Cse523ISD::SUB, DL, DAG.getVTList(VT, MVT::i64),
                        DAG.getConstant(0, VT), N0.getOperand(0));

                SDValue Ops[] = { N0.getOperand(0), Neg,
                    DAG.getConstant(Cse523::COND_GE, MVT::i64),
                    SDValue(Neg.getNode(), 1) };
                return DAG.getNode(Cse523ISD::CMOV, DL, DAG.getVTList(VT, MVT::Glue),
                        Ops, array_lengthof(Ops));
            }
    return SDValue();
}

// PerformXorCombine - Attempts to turn XOR nodes into BLSMSK nodes
static SDValue PerformXorCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    if (DCI.isBeforeLegalizeOps())
        return SDValue();

    if (Subtarget->hasCMov()) {
        SDValue RV = performIntegerAbsCombine(N, DAG);
        if (RV.getNode())
            return RV;
    }

    return SDValue();
}

/// PerformLOADCombine - Do target-specific dag combines on LOAD nodes.
static SDValue PerformLOADCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    LoadSDNode *Ld = cast<LoadSDNode>(N);
    EVT RegVT = Ld->getValueType(0);
    EVT MemVT = Ld->getMemoryVT();
    SDLoc dl(Ld);
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();
    unsigned RegSz = RegVT.getSizeInBits();

    // On Sandybridge unaligned 256bit loads are inefficient.
    ISD::LoadExtType Ext = Ld->getExtensionType();
    unsigned Alignment = Ld->getAlignment();
    bool IsAligned = Alignment == 0 || Alignment >= MemVT.getSizeInBits()/8;
    if (RegVT.is256BitVector() && !Subtarget->hasInt256() &&
            !DCI.isBeforeLegalizeOps() && !IsAligned && Ext == ISD::NON_EXTLOAD) {
        unsigned NumElems = RegVT.getVectorNumElements();
        if (NumElems < 2)
            return SDValue();

        SDValue Ptr = Ld->getBasePtr();
        SDValue Increment = DAG.getConstant(16, TLI.getPointerTy());

        EVT HalfVT = EVT::getVectorVT(*DAG.getContext(), MemVT.getScalarType(),
                NumElems/2);
        SDValue Load1 = DAG.getLoad(HalfVT, dl, Ld->getChain(), Ptr,
                Ld->getPointerInfo(), Ld->isVolatile(),
                Ld->isNonTemporal(), Ld->isInvariant(),
                Alignment);
        Ptr = DAG.getNode(ISD::ADD, dl, Ptr.getValueType(), Ptr, Increment);
        SDValue Load2 = DAG.getLoad(HalfVT, dl, Ld->getChain(), Ptr,
                Ld->getPointerInfo(), Ld->isVolatile(),
                Ld->isNonTemporal(), Ld->isInvariant(),
                std::min(16U, Alignment));
        SDValue TF = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                Load1.getValue(1),
                Load2.getValue(1));

        SDValue NewVec = DAG.getUNDEF(RegVT);
        NewVec = Insert128BitVector(NewVec, Load1, 0, DAG, dl);
        NewVec = Insert128BitVector(NewVec, Load2, NumElems/2, DAG, dl);
        return DCI.CombineTo(N, NewVec, TF, true);
    }

    // If this is a vector EXT Load then attempt to optimize it using a
    // shuffle. If SSSE3 is not available we may emit an illegal shuffle but the
    // expansion is still better than scalar code.
    // We generate Cse523ISD::VSEXT for SEXTLOADs if it's available, otherwise we'll
    // emit a shuffle and a arithmetic shift.
    // TODO: It is possible to support ZExt by zeroing the undef values
    // during the shuffle phase or after the shuffle.
    if (RegVT.isVector() && RegVT.isInteger() && Subtarget->hasSSE2() &&
            (Ext == ISD::EXTLOAD || Ext == ISD::SEXTLOAD)) {
        assert(MemVT != RegVT && "Cannot extend to the same type");
        assert(MemVT.isVector() && "Must load a vector from memory");

        unsigned NumElems = RegVT.getVectorNumElements();
        unsigned MemSz = MemVT.getSizeInBits();
        assert(RegSz > MemSz && "Register size must be greater than the mem size");

        if (Ext == ISD::SEXTLOAD && RegSz == 256 && !Subtarget->hasInt256())
            return SDValue();

        // All sizes must be a power of two.
        if (!isPowerOf2_32(RegSz * MemSz * NumElems))
            return SDValue();

        // Attempt to load the original value using scalar loads.
        // Find the largest scalar type that divides the total loaded size.
        MVT SclrLoadTy = MVT::i64;
        for (unsigned tp = MVT::FIRST_INTEGER_VALUETYPE;
                tp < MVT::LAST_INTEGER_VALUETYPE; ++tp) {
            MVT Tp = (MVT::SimpleValueType)tp;
            if (TLI.isTypeLegal(Tp) && ((MemSz % Tp.getSizeInBits()) == 0)) {
                SclrLoadTy = Tp;
            }
        }

        // On 32bit systems, we can't save 64bit integers. Try bitcasting to F64.
        if (TLI.isTypeLegal(MVT::f64) && SclrLoadTy.getSizeInBits() < 64 &&
                (64 <= MemSz))
            SclrLoadTy = MVT::f64;

        // Calculate the number of scalar loads that we need to perform
        // in order to load our vector from memory.
        unsigned NumLoads = MemSz / SclrLoadTy.getSizeInBits();
        if (Ext == ISD::SEXTLOAD && NumLoads > 1)
            return SDValue();

        unsigned loadRegZize = RegSz;
        if (Ext == ISD::SEXTLOAD && RegSz == 256)
            loadRegZize /= 2;

        // Represent our vector as a sequence of elements which are the
        // largest scalar that we can load.
        EVT LoadUnitVecVT = EVT::getVectorVT(*DAG.getContext(), SclrLoadTy,
                loadRegZize/SclrLoadTy.getSizeInBits());

        // Represent the data using the same element type that is stored in
        // memory. In practice, we ''widen'' MemVT.
        EVT WideVecVT =
            EVT::getVectorVT(*DAG.getContext(), MemVT.getScalarType(),
                    loadRegZize/MemVT.getScalarType().getSizeInBits());

        assert(WideVecVT.getSizeInBits() == LoadUnitVecVT.getSizeInBits() &&
                "Invalid vector type");

        // We can't shuffle using an illegal type.
        if (!TLI.isTypeLegal(WideVecVT))
            return SDValue();

        SmallVector<SDValue, 8> Chains;
        SDValue Ptr = Ld->getBasePtr();
        SDValue Increment = DAG.getConstant(SclrLoadTy.getSizeInBits()/8,
                TLI.getPointerTy());
        SDValue Res = DAG.getUNDEF(LoadUnitVecVT);

        for (unsigned i = 0; i < NumLoads; ++i) {
            // Perform a single load.
            SDValue ScalarLoad = DAG.getLoad(SclrLoadTy, dl, Ld->getChain(),
                    Ptr, Ld->getPointerInfo(),
                    Ld->isVolatile(), Ld->isNonTemporal(),
                    Ld->isInvariant(), Ld->getAlignment());
            Chains.push_back(ScalarLoad.getValue(1));
            // Create the first element type using SCALAR_TO_VECTOR in order to avoid
            // another round of DAGCombining.
            if (i == 0)
                Res = DAG.getNode(ISD::SCALAR_TO_VECTOR, dl, LoadUnitVecVT, ScalarLoad);
            else
                Res = DAG.getNode(ISD::INSERT_VECTOR_ELT, dl, LoadUnitVecVT, Res,
                        ScalarLoad, DAG.getIntPtrConstant(i));

            Ptr = DAG.getNode(ISD::ADD, dl, Ptr.getValueType(), Ptr, Increment);
        }

        SDValue TF = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, &Chains[0],
                Chains.size());

        // Bitcast the loaded value to a vector of the original element type, in
        // the size of the target vector type.
        SDValue SlicedVec = DAG.getNode(ISD::BITCAST, dl, WideVecVT, Res);
        unsigned SizeRatio = RegSz/MemSz;

        if (Ext == ISD::SEXTLOAD) {
            // If we have SSE4.1 we can directly emit a VSEXT node.
            if (Subtarget->hasSSE41()) {
                SDValue Sext = DAG.getNode(Cse523ISD::VSEXT, dl, RegVT, SlicedVec);
                return DCI.CombineTo(N, Sext, TF, true);
            }

            // Otherwise we'll shuffle the small elements in the high bits of the
            // larger type and perform an arithmetic shift. If the shift is not legal
            // it's better to scalarize.
            if (!TLI.isOperationLegalOrCustom(ISD::SRA, RegVT))
                return SDValue();

            // Redistribute the loaded elements into the different locations.
            SmallVector<int, 8> ShuffleVec(NumElems * SizeRatio, -1);
            for (unsigned i = 0; i != NumElems; ++i)
                ShuffleVec[i*SizeRatio + SizeRatio-1] = i;

            SDValue Shuff = DAG.getVectorShuffle(WideVecVT, dl, SlicedVec,
                    DAG.getUNDEF(WideVecVT),
                    &ShuffleVec[0]);

            Shuff = DAG.getNode(ISD::BITCAST, dl, RegVT, Shuff);

            // Build the arithmetic shift.
            unsigned Amt = RegVT.getVectorElementType().getSizeInBits() -
                MemVT.getVectorElementType().getSizeInBits();
            Shuff = DAG.getNode(ISD::SRA, dl, RegVT, Shuff,
                    DAG.getConstant(Amt, RegVT));

            return DCI.CombineTo(N, Shuff, TF, true);
        }

        // Redistribute the loaded elements into the different locations.
        SmallVector<int, 8> ShuffleVec(NumElems * SizeRatio, -1);
        for (unsigned i = 0; i != NumElems; ++i)
            ShuffleVec[i*SizeRatio] = i;

        SDValue Shuff = DAG.getVectorShuffle(WideVecVT, dl, SlicedVec,
                DAG.getUNDEF(WideVecVT),
                &ShuffleVec[0]);

        // Bitcast to the requested type.
        Shuff = DAG.getNode(ISD::BITCAST, dl, RegVT, Shuff);
        // Replace the original load with the new sequence
        // and return the new chain.
        return DCI.CombineTo(N, Shuff, TF, true);
    }

    return SDValue();
}

/// PerformSTORECombine - Do target-specific dag combines on STORE nodes.
static SDValue PerformSTORECombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    StoreSDNode *St = cast<StoreSDNode>(N);
    EVT VT = St->getValue().getValueType();
    EVT StVT = St->getMemoryVT();
    SDLoc dl(St);
    SDValue StoredVal = St->getOperand(1);
    const TargetLowering &TLI = DAG.getTargetLoweringInfo();

    // If we are saving a concatenation of two XMM registers, perform two stores.
    // On Sandy Bridge, 256-bit memory operations are executed by two
    // 128-bit ports. However, on Haswell it is better to issue a single 256-bit
    // memory  operation.
    unsigned Alignment = St->getAlignment();
    bool IsAligned = Alignment == 0 || Alignment >= VT.getSizeInBits()/8;
    if (VT.is256BitVector() && !Subtarget->hasInt256() &&
            StVT == VT && !IsAligned) {
        unsigned NumElems = VT.getVectorNumElements();
        if (NumElems < 2)
            return SDValue();

        SDValue Value0 = Extract128BitVector(StoredVal, 0, DAG, dl);
        SDValue Value1 = Extract128BitVector(StoredVal, NumElems/2, DAG, dl);

        SDValue Stride = DAG.getConstant(16, TLI.getPointerTy());
        SDValue Ptr0 = St->getBasePtr();
        SDValue Ptr1 = DAG.getNode(ISD::ADD, dl, Ptr0.getValueType(), Ptr0, Stride);

        SDValue Ch0 = DAG.getStore(St->getChain(), dl, Value0, Ptr0,
                St->getPointerInfo(), St->isVolatile(),
                St->isNonTemporal(), Alignment);
        SDValue Ch1 = DAG.getStore(St->getChain(), dl, Value1, Ptr1,
                St->getPointerInfo(), St->isVolatile(),
                St->isNonTemporal(),
                std::min(16U, Alignment));
        return DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Ch0, Ch1);
    }

    // Optimize trunc store (of multiple scalars) to shuffle and store.
    // First, pack all of the elements in one place. Next, store to memory
    // in fewer chunks.
    if (St->isTruncatingStore() && VT.isVector()) {
        llvm_unreachable("Unsupported Vector Stores");
    }

    // Turn load->store of MMX types into GPR load/stores.  This avoids clobbering
    // the FP state in cases where an emms may be missing.
    // A preferable solution to the general problem is to figure out the right
    // places to insert EMMS.  This qualifies as a quick hack.

    // Similarly, turn load->store of i64 into double load/stores in 32-bit mode.
    if (VT.getSizeInBits() != 64)
        return SDValue();

    const Function *F = DAG.getMachineFunction().getFunction();
    bool NoImplicitFloatOps = F->getAttributes().
        hasAttribute(AttributeSet::FunctionIndex, Attribute::NoImplicitFloat);
    bool F64IsLegal = !DAG.getTarget().Options.UseSoftFloat && !NoImplicitFloatOps
        && Subtarget->hasSSE2();
    if ((VT.isVector() ||
                (VT == MVT::i64 && F64IsLegal && !Subtarget->is64Bit())) &&
            isa<LoadSDNode>(St->getValue()) &&
            !cast<LoadSDNode>(St->getValue())->isVolatile() &&
            St->getChain().hasOneUse() && !St->isVolatile()) {
        SDNode* LdVal = St->getValue().getNode();
        LoadSDNode *Ld = 0;
        int TokenFactorIndex = -1;
        SmallVector<SDValue, 8> Ops;
        SDNode* ChainVal = St->getChain().getNode();
        // Must be a store of a load.  We currently handle two cases:  the load
        // is a direct child, and it's under an intervening TokenFactor.  It is
        // possible to dig deeper under nested TokenFactors.
        if (ChainVal == LdVal)
            Ld = cast<LoadSDNode>(St->getChain());
        else if (St->getValue().hasOneUse() &&
                ChainVal->getOpcode() == ISD::TokenFactor) {
            for (unsigned i = 0, e = ChainVal->getNumOperands(); i != e; ++i) {
                if (ChainVal->getOperand(i).getNode() == LdVal) {
                    TokenFactorIndex = i;
                    Ld = cast<LoadSDNode>(St->getValue());
                } else
                    Ops.push_back(ChainVal->getOperand(i));
            }
        }

        if (!Ld || !ISD::isNormalLoad(Ld))
            return SDValue();

        // If this is not the MMX case, i.e. we are just turning i64 load/store
        // into f64 load/store, avoid the transformation if there are multiple
        // uses of the loaded value.
        if (!VT.isVector() && !Ld->hasNUsesOfValue(1, 0))
            return SDValue();

        SDLoc LdDL(Ld);
        SDLoc StDL(N);
        // If we are a 64-bit capable cse523, lower to a single movq load/store pair.
        // Otherwise, if it's legal to use f64 SSE instructions, use f64 load/store
        // pair instead.
        EVT LdVT = MVT::i64;
        SDValue NewLd = DAG.getLoad(LdVT, LdDL, Ld->getChain(), Ld->getBasePtr(),
                Ld->getPointerInfo(), Ld->isVolatile(),
                Ld->isNonTemporal(), Ld->isInvariant(),
                Ld->getAlignment());
        SDValue NewChain = NewLd.getValue(1);
        if (TokenFactorIndex != -1) {
            Ops.push_back(NewChain);
            NewChain = DAG.getNode(ISD::TokenFactor, LdDL, MVT::Other, &Ops[0],
                    Ops.size());
        }
        return DAG.getStore(NewChain, StDL, NewLd, St->getBasePtr(),
                St->getPointerInfo(),
                St->isVolatile(), St->isNonTemporal(),
                St->getAlignment());
    }
    return SDValue();
}

/// isHorizontalBinOp - Return 'true' if this vector operation is "horizontal"
/// and return the operands for the horizontal operation in LHS and RHS.  A
/// horizontal operation performs the binary operation on successive elements
/// of its first operand, then on successive elements of its second operand,
/// returning the resulting values in a vector.  For example, if
///   A = < float a0, float a1, float a2, float a3 >
/// and
///   B = < float b0, float b1, float b2, float b3 >
/// then the result of doing a horizontal operation on A and B is
///   A horizontal-op B = < a0 op a1, a2 op a3, b0 op b1, b2 op b3 >.
/// In short, LHS and RHS are inspected to see if LHS op RHS is of the form
/// A horizontal-op B, for some already available A and B, and if so then LHS is
/// set to A, RHS to B, and the routine returns 'true'.
/// Note that the binary operation should have the property that if one of the
/// operands is UNDEF then the result is UNDEF.
static bool isHorizontalBinOp(SDValue &LHS, SDValue &RHS, bool IsCommutative) {
    // Look for the following pattern: if
    //   A = < float a0, float a1, float a2, float a3 >
    //   B = < float b0, float b1, float b2, float b3 >
    // and
    //   LHS = VECTOR_SHUFFLE A, B, <0, 2, 4, 6>
    //   RHS = VECTOR_SHUFFLE A, B, <1, 3, 5, 7>
    // then LHS op RHS = < a0 op a1, a2 op a3, b0 op b1, b2 op b3 >
    // which is A horizontal-op B.

    // At least one of the operands should be a vector shuffle.
    if (LHS.getOpcode() != ISD::VECTOR_SHUFFLE &&
            RHS.getOpcode() != ISD::VECTOR_SHUFFLE)
        return false;

    MVT VT = LHS.getSimpleValueType();

    assert((VT.is128BitVector() || VT.is256BitVector()) &&
            "Unsupported vector type for horizontal add/sub");

    // Handle 128 and 256-bit vector lengths. AVX defines horizontal add/sub to
    // operate independently on 128-bit lanes.
    unsigned NumElts = VT.getVectorNumElements();
    unsigned NumLanes = VT.getSizeInBits()/128;
    unsigned NumLaneElts = NumElts / NumLanes;
    assert((NumLaneElts % 2 == 0) &&
            "Vector type should have an even number of elements in each lane");
    unsigned HalfLaneElts = NumLaneElts/2;

    // View LHS in the form
    //   LHS = VECTOR_SHUFFLE A, B, LMask
    // If LHS is not a shuffle then pretend it is the shuffle
    //   LHS = VECTOR_SHUFFLE LHS, undef, <0, 1, ..., N-1>
    // NOTE: in what follows a default initialized SDValue represents an UNDEF of
    // type VT.
    SDValue A, B;
    SmallVector<int, 16> LMask(NumElts);
    if (LHS.getOpcode() == ISD::VECTOR_SHUFFLE) {
        if (LHS.getOperand(0).getOpcode() != ISD::UNDEF)
            A = LHS.getOperand(0);
        if (LHS.getOperand(1).getOpcode() != ISD::UNDEF)
            B = LHS.getOperand(1);
        ArrayRef<int> Mask = cast<ShuffleVectorSDNode>(LHS.getNode())->getMask();
        std::copy(Mask.begin(), Mask.end(), LMask.begin());
    } else {
        if (LHS.getOpcode() != ISD::UNDEF)
            A = LHS;
        for (unsigned i = 0; i != NumElts; ++i)
            LMask[i] = i;
    }

    // Likewise, view RHS in the form
    //   RHS = VECTOR_SHUFFLE C, D, RMask
    SDValue C, D;
    SmallVector<int, 16> RMask(NumElts);
    if (RHS.getOpcode() == ISD::VECTOR_SHUFFLE) {
        if (RHS.getOperand(0).getOpcode() != ISD::UNDEF)
            C = RHS.getOperand(0);
        if (RHS.getOperand(1).getOpcode() != ISD::UNDEF)
            D = RHS.getOperand(1);
        ArrayRef<int> Mask = cast<ShuffleVectorSDNode>(RHS.getNode())->getMask();
        std::copy(Mask.begin(), Mask.end(), RMask.begin());
    } else {
        if (RHS.getOpcode() != ISD::UNDEF)
            C = RHS;
        for (unsigned i = 0; i != NumElts; ++i)
            RMask[i] = i;
    }

    // Check that the shuffles are both shuffling the same vectors.
    if (!(A == C && B == D) && !(A == D && B == C))
        return false;

    // If everything is UNDEF then bail out: it would be better to fold to UNDEF.
    if (!A.getNode() && !B.getNode())
        return false;

    // If A and B occur in reverse order in RHS, then "swap" them (which means
    // rewriting the mask).
    if (A != C)
        CommuteVectorShuffleMask(RMask, NumElts);

    // At this point LHS and RHS are equivalent to
    //   LHS = VECTOR_SHUFFLE A, B, LMask
    //   RHS = VECTOR_SHUFFLE A, B, RMask
    // Check that the masks correspond to performing a horizontal operation.
    for (unsigned l = 0; l != NumElts; l += NumLaneElts) {
        for (unsigned i = 0; i != NumLaneElts; ++i) {
            int LIdx = LMask[i+l], RIdx = RMask[i+l];

            // Ignore any UNDEF components.
            if (LIdx < 0 || RIdx < 0 ||
                    (!A.getNode() && (LIdx < (int)NumElts || RIdx < (int)NumElts)) ||
                    (!B.getNode() && (LIdx >= (int)NumElts || RIdx >= (int)NumElts)))
                continue;

            // Check that successive elements are being operated on.  If not, this is
            // not a horizontal operation.
            unsigned Src = (i/HalfLaneElts); // each lane is split between srcs
            int Index = 2*(i%HalfLaneElts) + NumElts*Src + l;
            if (!(LIdx == Index && RIdx == Index + 1) &&
                    !(IsCommutative && LIdx == Index + 1 && RIdx == Index))
                return false;
        }
    }

    LHS = A.getNode() ? A : B; // If A is 'UNDEF', use B for it.
    RHS = B.getNode() ? B : A; // If B is 'UNDEF', use A for it.
    return true;
}

/// PerformFADDCombine - Do target-specific dag combines on floating point adds.
static SDValue PerformFADDCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);

    // Try to synthesize horizontal adds from adds of shuffles.
    if (((Subtarget->hasSSE3() && (VT == MVT::v4f32 || VT == MVT::v2f64)) ||
                (Subtarget->hasFp256() && (VT == MVT::v8f32 || VT == MVT::v4f64))) &&
            isHorizontalBinOp(LHS, RHS, true))
        return DAG.getNode(Cse523ISD::FHADD, SDLoc(N), VT, LHS, RHS);
    return SDValue();
}

/// PerformFSUBCombine - Do target-specific dag combines on floating point subs.
static SDValue PerformFSUBCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);

    // Try to synthesize horizontal subs from subs of shuffles.
    if (((Subtarget->hasSSE3() && (VT == MVT::v4f32 || VT == MVT::v2f64)) ||
                (Subtarget->hasFp256() && (VT == MVT::v8f32 || VT == MVT::v4f64))) &&
            isHorizontalBinOp(LHS, RHS, false))
        return DAG.getNode(Cse523ISD::FHSUB, SDLoc(N), VT, LHS, RHS);
    return SDValue();
}

/// PerformFORCombine - Do target-specific dag combines on Cse523ISD::FOR and
/// Cse523ISD::FXOR nodes.
static SDValue PerformFORCombine(SDNode *N, SelectionDAG &DAG) {
    assert(N->getOpcode() == Cse523ISD::FOR || N->getOpcode() == Cse523ISD::FXOR);
    // F[X]OR(0.0, x) -> x
    // F[X]OR(x, 0.0) -> x
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(0)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(1);
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(1)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(0);
    return SDValue();
}

/// PerformFMinFMaxCombine - Do target-specific dag combines on Cse523ISD::FMIN and
/// Cse523ISD::FMAX nodes.
static SDValue PerformFMinFMaxCombine(SDNode *N, SelectionDAG &DAG) {
    assert(N->getOpcode() == Cse523ISD::FMIN || N->getOpcode() == Cse523ISD::FMAX);

    // Only perform optimizations if UnsafeMath is used.
    if (!DAG.getTarget().Options.UnsafeFPMath)
        return SDValue();

    // If we run in unsafe-math mode, then convert the FMAX and FMIN nodes
    // into FMINC and FMAXC, which are Commutative operations.
    unsigned NewOp = 0;
    switch (N->getOpcode()) {
        default: llvm_unreachable("unknown opcode");
        case Cse523ISD::FMIN:  NewOp = Cse523ISD::FMINC; break;
        case Cse523ISD::FMAX:  NewOp = Cse523ISD::FMAXC; break;
    }

    return DAG.getNode(NewOp, SDLoc(N), N->getValueType(0),
            N->getOperand(0), N->getOperand(1));
}

/// PerformFANDCombine - Do target-specific dag combines on Cse523ISD::FAND nodes.
static SDValue PerformFANDCombine(SDNode *N, SelectionDAG &DAG) {
    // FAND(0.0, x) -> 0.0
    // FAND(x, 0.0) -> 0.0
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(0)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(0);
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(1)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(1);
    return SDValue();
}

/// PerformFANDNCombine - Do target-specific dag combines on Cse523ISD::FANDN nodes
static SDValue PerformFANDNCombine(SDNode *N, SelectionDAG &DAG) {
    // FANDN(x, 0.0) -> 0.0
    // FANDN(0.0, x) -> x
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(0)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(1);
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(N->getOperand(1)))
        if (C->getValueAPF().isPosZero())
            return N->getOperand(1);
    return SDValue();
}

static SDValue PerformBTCombine(SDNode *N,
        SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI) {
    // BT ignores high bits in the bit index operand.
    SDValue Op1 = N->getOperand(1);
    if (Op1.hasOneUse()) {
        unsigned BitWidth = Op1.getValueSizeInBits();
        APInt DemandedMask = APInt::getLowBitsSet(BitWidth, Log2_32(BitWidth));
        APInt KnownZero, KnownOne;
        TargetLowering::TargetLoweringOpt TLO(DAG, !DCI.isBeforeLegalize(),
                !DCI.isBeforeLegalizeOps());
        const TargetLowering &TLI = DAG.getTargetLoweringInfo();
        if (TLO.ShrinkDemandedConstant(Op1, DemandedMask) ||
                TLI.SimplifyDemandedBits(Op1, DemandedMask, KnownZero, KnownOne, TLO))
            DCI.CommitTargetLoweringOpt(TLO);
    }
    return SDValue();
}

static SDValue PerformVZEXT_MOVLCombine(SDNode *N, SelectionDAG &DAG) {
    SDValue Op = N->getOperand(0);
    if (Op.getOpcode() == ISD::BITCAST)
        Op = Op.getOperand(0);
    EVT VT = N->getValueType(0), OpVT = Op.getValueType();
    if (Op.getOpcode() == Cse523ISD::VZEXT_LOAD &&
            VT.getVectorElementType().getSizeInBits() ==
            OpVT.getVectorElementType().getSizeInBits()) {
        return DAG.getNode(ISD::BITCAST, SDLoc(N), VT, Op);
    }
    return SDValue();
}

static SDValue PerformSIGN_EXTEND_INREGCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    if (!VT.isVector())
        return SDValue();

    SDValue N0 = N->getOperand(0);
    SDValue N1 = N->getOperand(1);
    EVT ExtraVT = cast<VTSDNode>(N1)->getVT();
    SDLoc dl(N);

    // The SIGN_EXTEND_INREG to v4i64 is expensive operation on the
    // both SSE and AVX2 since there is no sign-extended shift right
    // operation on a vector with 64-bit elements.
    //(sext_in_reg (v4i64 anyext (v4i32 x )), ExtraVT) ->
    // (v4i64 sext (v4i32 sext_in_reg (v4i32 x , ExtraVT)))
    if (VT == MVT::v4i64 && (N0.getOpcode() == ISD::ANY_EXTEND ||
                N0.getOpcode() == ISD::SIGN_EXTEND)) {
        SDValue N00 = N0.getOperand(0);

        // EXTLOAD has a better solution on AVX2,
        // it may be replaced with Cse523ISD::VSEXT node.
        if (N00.getOpcode() == ISD::LOAD && Subtarget->hasInt256())
            if (!ISD::isNormalLoad(N00.getNode()))
                return SDValue();

        if (N00.getValueType() == MVT::v4i32 && ExtraVT.getSizeInBits() < 128) {
            SDValue Tmp = DAG.getNode(ISD::SIGN_EXTEND_INREG, dl, MVT::v4i32,
                    N00, N1);
            return DAG.getNode(ISD::SIGN_EXTEND, dl, MVT::v4i64, Tmp);
        }
    }
    return SDValue();
}

static SDValue PerformSExtCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    if (!DCI.isBeforeLegalizeOps())
        return SDValue();

    if (!Subtarget->hasFp256())
        return SDValue();

    EVT VT = N->getValueType(0);
    if (VT.isVector() && VT.getSizeInBits() == 256) {
        SDValue R = WidenMaskArithmetic(N, DAG, DCI, Subtarget);
        if (R.getNode())
            return R;
    }

    return SDValue();
}

static SDValue PerformFMACombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget* Subtarget) {
    SDLoc dl(N);
    EVT VT = N->getValueType(0);

    // Let legalize expand this if it isn't a legal type yet.
    if (!DAG.getTargetLoweringInfo().isTypeLegal(VT))
        return SDValue();

    EVT ScalarVT = VT.getScalarType();
    if ((ScalarVT != MVT::f32 && ScalarVT != MVT::f64) ||
            (!Subtarget->hasFMA() && !Subtarget->hasFMA4()))
        return SDValue();

    SDValue A = N->getOperand(0);
    SDValue B = N->getOperand(1);
    SDValue C = N->getOperand(2);

    bool NegA = (A.getOpcode() == ISD::FNEG);
    bool NegB = (B.getOpcode() == ISD::FNEG);
    bool NegC = (C.getOpcode() == ISD::FNEG);

    // Negative multiplication when NegA xor NegB
    bool NegMul = (NegA != NegB);
    if (NegA)
        A = A.getOperand(0);
    if (NegB)
        B = B.getOperand(0);
    if (NegC)
        C = C.getOperand(0);

    unsigned Opcode;
    if (!NegMul)
        Opcode = (!NegC) ? Cse523ISD::FMADD : Cse523ISD::FMSUB;
    else
        Opcode = (!NegC) ? Cse523ISD::FNMADD : Cse523ISD::FNMSUB;

    return DAG.getNode(Opcode, dl, VT, A, B, C);
}

static SDValue PerformZExtCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    // (i32 zext (and (i8  cse523isd::setcc_carry), 1)) ->
    //           (and (i32 cse523isd::setcc_carry), 1)
    // This eliminates the zext. This transformation is necessary because
    // ISD::SETCC is always legalized to i8.
    SDLoc dl(N);
    SDValue N0 = N->getOperand(0);
    EVT VT = N->getValueType(0);

    if (N0.getOpcode() == ISD::AND &&
            N0.hasOneUse() &&
            N0.getOperand(0).hasOneUse()) {
        SDValue N00 = N0.getOperand(0);
        if (N00.getOpcode() == Cse523ISD::SETCC_CARRY) {
            ConstantSDNode *C = dyn_cast<ConstantSDNode>(N0.getOperand(1));
            if (!C || C->getZExtValue() != 1)
                return SDValue();
            return DAG.getNode(ISD::AND, dl, VT,
                    DAG.getNode(Cse523ISD::SETCC_CARRY, dl, VT,
                        N00.getOperand(0), N00.getOperand(1)),
                    DAG.getConstant(1, VT));
        }
    }

    if (N0.getOpcode() == ISD::TRUNCATE &&
            N0.hasOneUse() &&
            N0.getOperand(0).hasOneUse()) {
        SDValue N00 = N0.getOperand(0);
        if (N00.getOpcode() == Cse523ISD::SETCC_CARRY) {
            return DAG.getNode(ISD::AND, dl, VT,
                    DAG.getNode(Cse523ISD::SETCC_CARRY, dl, VT,
                        N00.getOperand(0), N00.getOperand(1)),
                    DAG.getConstant(1, VT));
        }
    }
    if (VT.is256BitVector()) {
        SDValue R = WidenMaskArithmetic(N, DAG, DCI, Subtarget);
        if (R.getNode())
            return R;
    }

    return SDValue();
}

// Optimize x == -y --> x+y == 0
//          x != -y --> x+y != 0
static SDValue PerformISDSETCCCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget* Subtarget) {
    ISD::CondCode CC = cast<CondCodeSDNode>(N->getOperand(2))->get();
    SDValue LHS = N->getOperand(0);
    SDValue RHS = N->getOperand(1);
    EVT VT = N->getValueType(0);
    SDLoc DL(N);

    if ((CC == ISD::SETNE || CC == ISD::SETEQ) && LHS.getOpcode() == ISD::SUB)
        if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(LHS.getOperand(0)))
            if (C->getAPIntValue() == 0 && LHS.hasOneUse()) {
                SDValue addV = DAG.getNode(ISD::ADD, SDLoc(N),
                        LHS.getValueType(), RHS, LHS.getOperand(1));
                return DAG.getSetCC(SDLoc(N), N->getValueType(0),
                        addV, DAG.getConstant(0, addV.getValueType()), CC);
            }
    if ((CC == ISD::SETNE || CC == ISD::SETEQ) && RHS.getOpcode() == ISD::SUB)
        if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(RHS.getOperand(0)))
            if (C->getAPIntValue() == 0 && RHS.hasOneUse()) {
                SDValue addV = DAG.getNode(ISD::ADD, SDLoc(N),
                        RHS.getValueType(), LHS, RHS.getOperand(1));
                return DAG.getSetCC(SDLoc(N), N->getValueType(0),
                        addV, DAG.getConstant(0, addV.getValueType()), CC);
            }

    if (VT.getScalarType() == MVT::i1) {
        bool IsSEXT0 = (LHS.getOpcode() == ISD::SIGN_EXTEND) &&
            (LHS.getOperand(0).getValueType().getScalarType() ==  MVT::i1);
        bool IsVZero0 = ISD::isBuildVectorAllZeros(LHS.getNode());
        if (!IsSEXT0 && !IsVZero0)
            return SDValue();
        bool IsSEXT1 = (RHS.getOpcode() == ISD::SIGN_EXTEND) &&
            (RHS.getOperand(0).getValueType().getScalarType() ==  MVT::i1);
        bool IsVZero1 = ISD::isBuildVectorAllZeros(RHS.getNode());

        if (!IsSEXT1 && !IsVZero1)
            return SDValue();

        if (IsSEXT0 && IsVZero1) {
            assert(VT == LHS.getOperand(0).getValueType() && "Uexpected operand type");
            if (CC == ISD::SETEQ)
                return DAG.getNOT(DL, LHS.getOperand(0), VT);
            return LHS.getOperand(0);
        }
        if (IsSEXT1 && IsVZero0) {
            assert(VT == RHS.getOperand(0).getValueType() && "Uexpected operand type");
            if (CC == ISD::SETEQ)
                return DAG.getNOT(DL, RHS.getOperand(0), VT);
            return RHS.getOperand(0);
        }
    }

    return SDValue();
}

// Helper function of PerformSETCCCombine. It is to materialize "setb reg"
// as "sbb reg,reg", since it can be extended without zext and produces
// an all-ones bit which is more useful than 0/1 in some cases.
static SDValue MaterializeSETB(SDLoc DL, SDValue EFLAGS, SelectionDAG &DAG,
        MVT VT) {
    if (VT == MVT::i64)
        return DAG.getNode(ISD::AND, DL, VT,
                DAG.getNode(Cse523ISD::SETCC_CARRY, DL, MVT::i64,
                    DAG.getConstant(Cse523::COND_B, MVT::i64), EFLAGS),
                DAG.getConstant(1, VT));
    assert (0 && "Unexpected type for SECCC node");
}

// Optimize  RES = Cse523ISD::SETCC CONDCODE, EFLAG_INPUT
static SDValue PerformSETCCCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    SDLoc DL(N);
    Cse523::CondCode CC = Cse523::CondCode(N->getConstantOperandVal(0));
    SDValue EFLAGS = N->getOperand(1);

    if (CC == Cse523::COND_A) {
        // Try to convert COND_A into COND_B in an attempt to facilitate
        // materializing "setb reg".
        //
        // Do not flip "e > c", where "c" is a constant, because Cmp instruction
        // cannot take an immediate as its first operand.
        //
        if (EFLAGS.getOpcode() == Cse523ISD::SUB && EFLAGS.hasOneUse() &&
                EFLAGS.getValueType().isInteger() &&
                !isa<ConstantSDNode>(EFLAGS.getOperand(1))) {
            SDValue NewSub = DAG.getNode(Cse523ISD::SUB, SDLoc(EFLAGS),
                    EFLAGS.getNode()->getVTList(),
                    EFLAGS.getOperand(1), EFLAGS.getOperand(0));
            SDValue NewEFLAGS = SDValue(NewSub.getNode(), EFLAGS.getResNo());
            return MaterializeSETB(DL, NewEFLAGS, DAG, N->getSimpleValueType(0));
        }
    }

    // Materialize "setb reg" as "sbb reg,reg", since it can be extended without
    // a zext and produces an all-ones bit which is more useful than 0/1 in some
    // cases.
    if (CC == Cse523::COND_B)
        return MaterializeSETB(DL, EFLAGS, DAG, N->getSimpleValueType(0));

    SDValue Flags;

    Flags = checkBoolTestSetCCCombine(EFLAGS, CC);
    if (Flags.getNode()) {
        SDValue Cond = DAG.getConstant(CC, MVT::i64);
        return DAG.getNode(Cse523ISD::SETCC, DL, N->getVTList(), Cond, Flags);
    }

    return SDValue();
}

// Optimize branch condition evaluation.
//
static SDValue PerformBrCondCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    SDLoc DL(N);
    SDValue Chain = N->getOperand(0);
    SDValue Dest = N->getOperand(1);
    SDValue EFLAGS = N->getOperand(3);
    Cse523::CondCode CC = Cse523::CondCode(N->getConstantOperandVal(2));

    SDValue Flags;

    Flags = checkBoolTestSetCCCombine(EFLAGS, CC);
    if (Flags.getNode()) {
        SDValue Cond = DAG.getConstant(CC, MVT::i64);
        return DAG.getNode(Cse523ISD::BRCOND, DL, N->getVTList(), Chain, Dest, Cond,
                Flags);
    }

    return SDValue();
}

static SDValue PerformSINT_TO_FPCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523TargetLowering *XTLI) {
    SDValue Op0 = N->getOperand(0);
    EVT InVT = Op0->getValueType(0);

    // SINT_TO_FP(v4i8) -> SINT_TO_FP(SEXT(v4i8 to v4i32))
    if (InVT == MVT::v8i8 || InVT == MVT::v4i8) {
        SDLoc dl(N);
        MVT DstVT = InVT == MVT::v4i8 ? MVT::v4i32 : MVT::v8i32;
        SDValue P = DAG.getNode(ISD::SIGN_EXTEND, dl, DstVT, Op0);
        return DAG.getNode(ISD::SINT_TO_FP, dl, N->getValueType(0), P);
    }

    // Transform (SINT_TO_FP (i64 ...)) into an x87 operation if we have
    // a 32-bit target where SSE doesn't support i64->FP operations.
    if (Op0.getOpcode() == ISD::LOAD) {
        LoadSDNode *Ld = cast<LoadSDNode>(Op0.getNode());
        EVT VT = Ld->getValueType(0);
        if (!Ld->isVolatile() && !N->getValueType(0).isVector() &&
                ISD::isNON_EXTLoad(Op0.getNode()) && Op0.hasOneUse() &&
                !XTLI->getSubtarget()->is64Bit() &&
                VT == MVT::i64) {
            SDValue FILDChain = XTLI->BuildFILD(SDValue(N, 0), Ld->getValueType(0),
                    Ld->getChain(), Op0, DAG);
            DAG.ReplaceAllUsesOfValueWith(Op0.getValue(1), FILDChain.getValue(1));
            return FILDChain;
        }
    }
    return SDValue();
}

// Optimize RES, EFLAGS = Cse523ISD::ADC LHS, RHS, EFLAGS
static SDValue PerformADCCombine(SDNode *N, SelectionDAG &DAG,
        Cse523TargetLowering::DAGCombinerInfo &DCI) {
    // If the LHS and RHS of the ADC node are zero, then it can't overflow and
    // the result is either zero or one (depending on the input carry bit).
    // Strength reduce this down to a "set on carry" aka SETCC_CARRY&1.
    if (Cse523::isZeroNode(N->getOperand(0)) &&
            Cse523::isZeroNode(N->getOperand(1)) &&
            // We don't have a good way to replace an EFLAGS use, so only do this when
            // dead right now.
            SDValue(N, 1).use_empty()) {
        SDLoc DL(N);
        EVT VT = N->getValueType(0);
        SDValue CarryOut = DAG.getConstant(0, N->getValueType(1));
        SDValue Res1 = DAG.getNode(ISD::AND, DL, VT,
                DAG.getNode(Cse523ISD::SETCC_CARRY, DL, VT,
                    DAG.getConstant(Cse523::COND_B,MVT::i64),
                    N->getOperand(2)),
                DAG.getConstant(1, VT));
        return DCI.CombineTo(N, Res1, CarryOut);
    }

    return SDValue();
}

// fold (add Y, (sete  X, 0)) -> adc  0, Y
//      (add Y, (setne X, 0)) -> sbb -1, Y
//      (sub (sete  X, 0), Y) -> sbb  0, Y
//      (sub (setne X, 0), Y) -> adc -1, Y
static SDValue OptimizeConditionalInDecrement(SDNode *N, SelectionDAG &DAG) {
    SDLoc DL(N);

    // Look through ZExts.
    SDValue Ext = N->getOperand(N->getOpcode() == ISD::SUB ? 1 : 0);
    if (Ext.getOpcode() != ISD::ZERO_EXTEND || !Ext.hasOneUse())
        return SDValue();

    SDValue SetCC = Ext.getOperand(0);
    if (SetCC.getOpcode() != Cse523ISD::SETCC || !SetCC.hasOneUse())
        return SDValue();

    Cse523::CondCode CC = (Cse523::CondCode)SetCC.getConstantOperandVal(0);
    if (CC != Cse523::COND_E && CC != Cse523::COND_NE)
        return SDValue();

    SDValue Cmp = SetCC.getOperand(1);
    if (Cmp.getOpcode() != Cse523ISD::CMP || !Cmp.hasOneUse() ||
            !Cse523::isZeroNode(Cmp.getOperand(1)) ||
            !Cmp.getOperand(0).getValueType().isInteger())
        return SDValue();

    SDValue CmpOp0 = Cmp.getOperand(0);
    SDValue NewCmp = DAG.getNode(Cse523ISD::CMP, DL, MVT::i64, CmpOp0,
            DAG.getConstant(1, CmpOp0.getValueType()));

    SDValue OtherVal = N->getOperand(N->getOpcode() == ISD::SUB ? 0 : 1);
    if (CC == Cse523::COND_NE)
        return DAG.getNode(N->getOpcode() == ISD::SUB ? Cse523ISD::ADC : Cse523ISD::SUB,
                DL, OtherVal.getValueType(), OtherVal,
                DAG.getConstant(-1ULL, OtherVal.getValueType()), NewCmp);
    return DAG.getNode(N->getOpcode() == ISD::SUB ? Cse523ISD::SUB : Cse523ISD::ADC,
            DL, OtherVal.getValueType(), OtherVal,
            DAG.getConstant(0, OtherVal.getValueType()), NewCmp);
}

/// PerformADDCombine - Do target-specific dag combines on integer adds.
static SDValue PerformAddCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    EVT VT = N->getValueType(0);
    SDValue Op0 = N->getOperand(0);
    SDValue Op1 = N->getOperand(1);

    // Try to synthesize horizontal adds from adds of shuffles.
    if (((Subtarget->hasSSSE3() && (VT == MVT::v8i16 || VT == MVT::v4i32)) ||
                (Subtarget->hasInt256() && (VT == MVT::v16i16 || VT == MVT::v8i32))) &&
            isHorizontalBinOp(Op0, Op1, true))
        return DAG.getNode(Cse523ISD::HADD, SDLoc(N), VT, Op0, Op1);

    return OptimizeConditionalInDecrement(N, DAG);
}

static SDValue PerformSubCombine(SDNode *N, SelectionDAG &DAG,
        const Cse523Subtarget *Subtarget) {
    SDValue Op0 = N->getOperand(0);
    SDValue Op1 = N->getOperand(1);

    // Cse523 can't encode an immediate LHS of a sub. See if we can push the
    // negation into a preceding instruction.
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op0)) {
        // If the RHS of the sub is a XOR with one use and a constant, invert the
        // immediate. Then add one to the LHS of the sub so we can turn
        // X-Y -> X+~Y+1, saving one register.
        if (Op1->hasOneUse() && Op1.getOpcode() == ISD::XOR &&
                isa<ConstantSDNode>(Op1.getOperand(1))) {
            APInt XorC = cast<ConstantSDNode>(Op1.getOperand(1))->getAPIntValue();
            EVT VT = Op0.getValueType();
            SDValue NewXor = DAG.getNode(ISD::XOR, SDLoc(Op1), VT,
                    Op1.getOperand(0),
                    DAG.getConstant(~XorC, VT));
            return DAG.getNode(ISD::ADD, SDLoc(N), VT, NewXor,
                    DAG.getConstant(C->getAPIntValue()+1, VT));
        }
    }

    // Try to synthesize horizontal adds from adds of shuffles.
    EVT VT = N->getValueType(0);
    if (((Subtarget->hasSSSE3() && (VT == MVT::v8i16 || VT == MVT::v4i32)) ||
                (Subtarget->hasInt256() && (VT == MVT::v16i16 || VT == MVT::v8i32))) &&
            isHorizontalBinOp(Op0, Op1, true))
        return DAG.getNode(Cse523ISD::HSUB, SDLoc(N), VT, Op0, Op1);

    return OptimizeConditionalInDecrement(N, DAG);
}

/// performVZEXTCombine - Performs build vector combines
static SDValue performVZEXTCombine(SDNode *N, SelectionDAG &DAG,
        TargetLowering::DAGCombinerInfo &DCI,
        const Cse523Subtarget *Subtarget) {
    // (vzext (bitcast (vzext (x)) -> (vzext x)
    SDValue In = N->getOperand(0);
    while (In.getOpcode() == ISD::BITCAST)
        In = In.getOperand(0);

    if (In.getOpcode() != Cse523ISD::VZEXT)
        return SDValue();

    return DAG.getNode(Cse523ISD::VZEXT, SDLoc(N), N->getValueType(0),
            In.getOperand(0));
}

SDValue Cse523TargetLowering::PerformDAGCombine(SDNode *N,
        DAGCombinerInfo &DCI) const {
    SelectionDAG &DAG = DCI.DAG;
    switch (N->getOpcode()) {
        default: break;
        case ISD::EXTRACT_VECTOR_ELT:
                 return PerformEXTRACT_VECTOR_ELTCombine(N, DAG, DCI);
        case ISD::VSELECT:
        case ISD::SELECT:         return PerformSELECTCombine(N, DAG, DCI, Subtarget);
        case Cse523ISD::CMOV:        return PerformCMOVCombine(N, DAG, DCI, Subtarget);
        case ISD::ADD:            return PerformAddCombine(N, DAG, Subtarget);
        case ISD::SUB:            return PerformSubCombine(N, DAG, Subtarget);
        case Cse523ISD::ADC:         return PerformADCCombine(N, DAG, DCI);
        case ISD::MUL:            return PerformMulCombine(N, DAG, DCI);
        case ISD::SHL:
        case ISD::SRA:
        case ISD::SRL:            return PerformShiftCombine(N, DAG, DCI, Subtarget);
        case ISD::AND:            return PerformAndCombine(N, DAG, DCI, Subtarget);
        case ISD::OR:             return PerformOrCombine(N, DAG, DCI, Subtarget);
        case ISD::XOR:            return PerformXorCombine(N, DAG, DCI, Subtarget);
        case ISD::LOAD:           return PerformLOADCombine(N, DAG, DCI, Subtarget);
        case ISD::STORE:          return PerformSTORECombine(N, DAG, Subtarget);
        case ISD::SINT_TO_FP:     return PerformSINT_TO_FPCombine(N, DAG, this);
        case ISD::FADD:           return PerformFADDCombine(N, DAG, Subtarget);
        case ISD::FSUB:           return PerformFSUBCombine(N, DAG, Subtarget);
        case Cse523ISD::FXOR:
        case Cse523ISD::FOR:         return PerformFORCombine(N, DAG);
        case Cse523ISD::FMIN:
        case Cse523ISD::FMAX:        return PerformFMinFMaxCombine(N, DAG);
        case Cse523ISD::FAND:        return PerformFANDCombine(N, DAG);
        case Cse523ISD::FANDN:       return PerformFANDNCombine(N, DAG);
        case Cse523ISD::BT:          return PerformBTCombine(N, DAG, DCI);
        case Cse523ISD::VZEXT_MOVL:  return PerformVZEXT_MOVLCombine(N, DAG);
        case ISD::ANY_EXTEND:
        case ISD::ZERO_EXTEND:    return PerformZExtCombine(N, DAG, DCI, Subtarget);
        case ISD::SIGN_EXTEND:    return PerformSExtCombine(N, DAG, DCI, Subtarget);
        case ISD::SIGN_EXTEND_INREG: return PerformSIGN_EXTEND_INREGCombine(N, DAG, Subtarget);
        case ISD::TRUNCATE:       return PerformTruncateCombine(N, DAG,DCI,Subtarget);
        case ISD::SETCC:          return PerformISDSETCCCombine(N, DAG, Subtarget);
        case Cse523ISD::SETCC:       return PerformSETCCCombine(N, DAG, DCI, Subtarget);
        case Cse523ISD::BRCOND:      return PerformBrCondCombine(N, DAG, DCI, Subtarget);
        case Cse523ISD::VZEXT:       return performVZEXTCombine(N, DAG, DCI, Subtarget);
        case Cse523ISD::SHUFP:       // Handle all target specific shuffles
        case Cse523ISD::PALIGNR:
        case Cse523ISD::UNPCKH:
        case Cse523ISD::UNPCKL:
        case Cse523ISD::MOVHLPS:
        case Cse523ISD::MOVLHPS:
        case Cse523ISD::PSHUFD:
        case Cse523ISD::PSHUFHW:
        case Cse523ISD::PSHUFLW:
        case Cse523ISD::MOVSS:
        case Cse523ISD::MOVSD:
        case Cse523ISD::VPERMILP:
        case Cse523ISD::VPERM2X128:
        case ISD::VECTOR_SHUFFLE: return PerformShuffleCombine(N, DAG, DCI,Subtarget);
        case ISD::FMA:            return PerformFMACombine(N, DAG, Subtarget);
    }

    return SDValue();
}

/// isTypeDesirableForOp - Return true if the target has native support for
/// the specified value type and it is 'desirable' to use the type for the
/// given node type. e.g. On cse523 i16 is legal, but undesirable since i16
/// instruction encodings are longer and some i16 instructions are slow.
bool Cse523TargetLowering::isTypeDesirableForOp(unsigned Opc, EVT VT) const {
    if (!isTypeLegal(VT))
        return false;
    if (VT != MVT::i16)
        return true;

    switch (Opc) {
        default:
            return true;
        case ISD::LOAD:
        case ISD::SIGN_EXTEND:
        case ISD::ZERO_EXTEND:
        case ISD::ANY_EXTEND:
        case ISD::SHL:
        case ISD::SRL:
        case ISD::SUB:
        case ISD::ADD:
        case ISD::MUL:
        case ISD::AND:
        case ISD::OR:
        case ISD::XOR:
            return false;
    }
}

/// IsDesirableToPromoteOp - This method query the target whether it is
/// beneficial for dag combiner to promote the specified node. If true, it
/// should return the desired promotion type by reference.
bool Cse523TargetLowering::IsDesirableToPromoteOp(SDValue Op, EVT &PVT) const {
    EVT VT = Op.getValueType();
    if (VT != MVT::i16)
        return false;

    bool Promote = false;
    bool Commute = false;
    switch (Op.getOpcode()) {
        default: break;
        case ISD::LOAD: {
                            LoadSDNode *LD = cast<LoadSDNode>(Op);
                            // If the non-extending load has a single use and it's not live out, then it
                            // might be folded.
                            if (LD->getExtensionType() == ISD::NON_EXTLOAD /*&&
                                                                             Op.hasOneUse()*/) {
                                for (SDNode::use_iterator UI = Op.getNode()->use_begin(),
                                        UE = Op.getNode()->use_end(); UI != UE; ++UI) {
                                    // The only case where we'd want to promote LOAD (rather then it being
                                    // promoted as an operand is when it's only use is liveout.
                                    if (UI->getOpcode() != ISD::CopyToReg)
                                        return false;
                                }
                            }
                            Promote = true;
                            break;
                        }
        case ISD::SIGN_EXTEND:
        case ISD::ZERO_EXTEND:
        case ISD::ANY_EXTEND:
                 Promote = true;
                 break;
        case ISD::SHL:
        case ISD::SRL: {
                           SDValue N0 = Op.getOperand(0);
                           // Look out for (store (shl (load), x)).
                           if (MayFoldLoad(N0) && MayFoldIntoStore(Op))
                               return false;
                           Promote = true;
                           break;
                       }
        case ISD::ADD:
        case ISD::MUL:
        case ISD::AND:
        case ISD::OR:
        case ISD::XOR:
                       Commute = true;
                       // fallthrough
        case ISD::SUB: {
                           SDValue N0 = Op.getOperand(0);
                           SDValue N1 = Op.getOperand(1);
                           if (!Commute && MayFoldLoad(N1))
                               return false;
                           // Avoid disabling potential load folding opportunities.
                           if (MayFoldLoad(N0) && (!isa<ConstantSDNode>(N1) || MayFoldIntoStore(Op)))
                               return false;
                           if (MayFoldLoad(N1) && (!isa<ConstantSDNode>(N0) || MayFoldIntoStore(Op)))
                               return false;
                           Promote = true;
                       }
    }

    assert(0);
    PVT = MVT::i32;
    return Promote;
}

//===----------------------------------------------------------------------===//
//                           Cse523 Inline Assembly Support
//===----------------------------------------------------------------------===//

namespace {
    // Helper to match a string separated by whitespace.
    bool matchAsmImpl(StringRef s, ArrayRef<const StringRef *> args) {
        s = s.substr(s.find_first_not_of(" \t")); // Skip leading whitespace.

        for (unsigned i = 0, e = args.size(); i != e; ++i) {
            StringRef piece(*args[i]);
            if (!s.startswith(piece)) // Check if the piece matches.
                return false;

            s = s.substr(piece.size());
            StringRef::size_type pos = s.find_first_not_of(" \t");
            if (pos == 0) // We matched a prefix.
                return false;

            s = s.substr(pos);
        }

        return s.empty();
    }
    const VariadicFunction1<bool, StringRef, StringRef, matchAsmImpl> matchAsm={};
}

static bool clobbersFlagRegisters(const SmallVector<StringRef, 4> &AsmPieces) {

    if (AsmPieces.size() == 3 || AsmPieces.size() == 4) {
        if (std::count(AsmPieces.begin(), AsmPieces.end(), "~{cc}") &&
                std::count(AsmPieces.begin(), AsmPieces.end(), "~{flags}") &&
                std::count(AsmPieces.begin(), AsmPieces.end(), "~{fpsr}")) {

            if (AsmPieces.size() == 3)
                return true;
            else if (std::count(AsmPieces.begin(), AsmPieces.end(), "~{dirflag}"))
                return true;
        }
    }
    return false;
}

bool Cse523TargetLowering::ExpandInlineAsm(CallInst *CI) const {
    InlineAsm *IA = cast<InlineAsm>(CI->getCalledValue());

    std::string AsmStr = IA->getAsmString();

    IntegerType *Ty = dyn_cast<IntegerType>(CI->getType());
    if (!Ty || Ty->getBitWidth() % 16 != 0)
        return false;

    // TODO: should remove alternatives from the asmstring: "foo {a|b}" -> "foo a"
    SmallVector<StringRef, 4> AsmPieces;
    SplitString(AsmStr, AsmPieces, ";\n");

    switch (AsmPieces.size()) {
        default: return false;
        case 1:
                 // FIXME: this should verify that we are targeting a 486 or better.  If not,
                 // we will turn this bswap into something that will be lowered to logical
                 // ops instead of emitting the bswap asm.  For now, we don't support 486 or
                 // lower so don't worry about this.
                 // bswap $0
                 if (matchAsm(AsmPieces[0], "bswap", "$0") ||
                         matchAsm(AsmPieces[0], "bswapl", "$0") ||
                         matchAsm(AsmPieces[0], "bswapq", "$0") ||
                         matchAsm(AsmPieces[0], "bswap", "${0:q}") ||
                         matchAsm(AsmPieces[0], "bswapl", "${0:q}") ||
                         matchAsm(AsmPieces[0], "bswapq", "${0:q}")) {
                     // No need to check constraints, nothing other than the equivalent of
                     // "=r,0" would be valid here.
                     return IntrinsicLowering::LowerToByteSwap(CI);
                 }

                 // rorw $$8, ${0:w}  -->  llvm.bswap.i16
                 if (CI->getType()->isIntegerTy(16) &&
                         IA->getConstraintString().compare(0, 5, "=r,0,") == 0 &&
                         (matchAsm(AsmPieces[0], "rorw", "$$8,", "${0:w}") ||
                          matchAsm(AsmPieces[0], "rolw", "$$8,", "${0:w}"))) {
                     AsmPieces.clear();
                     const std::string &ConstraintsStr = IA->getConstraintString();
                     SplitString(StringRef(ConstraintsStr).substr(5), AsmPieces, ",");
                     array_pod_sort(AsmPieces.begin(), AsmPieces.end());
                     if (clobbersFlagRegisters(AsmPieces))
                         return IntrinsicLowering::LowerToByteSwap(CI);
                 }
                 break;
        case 3:
                 if (CI->getType()->isIntegerTy(32) &&
                         IA->getConstraintString().compare(0, 5, "=r,0,") == 0 &&
                         matchAsm(AsmPieces[0], "rorw", "$$8,", "${0:w}") &&
                         matchAsm(AsmPieces[1], "rorl", "$$16,", "$0") &&
                         matchAsm(AsmPieces[2], "rorw", "$$8,", "${0:w}")) {
                     AsmPieces.clear();
                     const std::string &ConstraintsStr = IA->getConstraintString();
                     SplitString(StringRef(ConstraintsStr).substr(5), AsmPieces, ",");
                     array_pod_sort(AsmPieces.begin(), AsmPieces.end());
                     if (clobbersFlagRegisters(AsmPieces))
                         return IntrinsicLowering::LowerToByteSwap(CI);
                 }

                 if (CI->getType()->isIntegerTy(64)) {
                     InlineAsm::ConstraintInfoVector Constraints = IA->ParseConstraints();
                     if (Constraints.size() >= 2 &&
                             Constraints[0].Codes.size() == 1 && Constraints[0].Codes[0] == "A" &&
                             Constraints[1].Codes.size() == 1 && Constraints[1].Codes[0] == "0") {
                         // bswap %eax / bswap %edx / xchgl %eax, %edx  -> llvm.bswap.i64
                         if (matchAsm(AsmPieces[0], "bswap", "%eax") &&
                                 matchAsm(AsmPieces[1], "bswap", "%edx") &&
                                 matchAsm(AsmPieces[2], "xchgl", "%eax,", "%edx"))
                             return IntrinsicLowering::LowerToByteSwap(CI);
                     }
                 }
                 break;
    }
    return false;
}

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
Cse523TargetLowering::ConstraintType
Cse523TargetLowering::getConstraintType(const std::string &Constraint) const {
    if (Constraint.size() == 1) {
        switch (Constraint[0]) {
            case 'R':
            case 'q':
            case 'Q':
            case 'f':
            case 't':
            case 'u':
            case 'y':
            case 'x':
            case 'Y':
            case 'l':
                return C_RegisterClass;
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'S':
            case 'D':
            case 'A':
                return C_Register;
            case 'I':
            case 'J':
            case 'K':
            case 'L':
            case 'M':
            case 'N':
            case 'G':
            case 'C':
            case 'e':
            case 'Z':
                return C_Other;
            default:
                break;
        }
    }
    return TargetLowering::getConstraintType(Constraint);
}

/// Examine constraint type and operand type and determine a weight value.
/// This object must already have been set up with the operand type
/// and the current alternative constraint selected.
TargetLowering::ConstraintWeight
Cse523TargetLowering::getSingleConstraintMatchWeight(
        AsmOperandInfo &info, const char *constraint) const {
    ConstraintWeight weight = CW_Invalid;
    Value *CallOperandVal = info.CallOperandVal;
    // If we don't have a value, we can't do a match,
    // but allow it at the lowest weight.
    if (CallOperandVal == NULL)
        return CW_Default;
    Type *type = CallOperandVal->getType();
    // Look at the constraint type.
    switch (*constraint) {
        default:
            weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
        case 'R':
        case 'q':
        case 'Q':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'S':
        case 'D':
        case 'A':
            if (CallOperandVal->getType()->isIntegerTy())
                weight = CW_SpecificReg;
            break;
        case 'f':
        case 't':
        case 'u':
            if (type->isFloatingPointTy())
                weight = CW_SpecificReg;
            break;
        case 'y':
            assert(0);
            break;
        case 'x':
        case 'Y':
            if (((type->getPrimitiveSizeInBits() == 128) && Subtarget->hasSSE1()) ||
                    ((type->getPrimitiveSizeInBits() == 256) && Subtarget->hasFp256()))
                weight = CW_Register;
            break;
        case 'I':
            if (ConstantInt *C = dyn_cast<ConstantInt>(info.CallOperandVal)) {
                if (C->getZExtValue() <= 31)
                    weight = CW_Constant;
            }
            break;
        case 'J':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if (C->getZExtValue() <= 63)
                    weight = CW_Constant;
            }
            break;
        case 'K':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if ((C->getSExtValue() >= -0x80) && (C->getSExtValue() <= 0x7f))
                    weight = CW_Constant;
            }
            break;
        case 'L':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if ((C->getZExtValue() == 0xff) || (C->getZExtValue() == 0xffff))
                    weight = CW_Constant;
            }
            break;
        case 'M':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if (C->getZExtValue() <= 3)
                    weight = CW_Constant;
            }
            break;
        case 'N':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if (C->getZExtValue() <= 0xff)
                    weight = CW_Constant;
            }
            break;
        case 'G':
        case 'C':
            if (dyn_cast<ConstantFP>(CallOperandVal)) {
                weight = CW_Constant;
            }
            break;
        case 'e':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if ((C->getSExtValue() >= -0x80000000LL) &&
                        (C->getSExtValue() <= 0x7fffffffLL))
                    weight = CW_Constant;
            }
            break;
        case 'Z':
            if (ConstantInt *C = dyn_cast<ConstantInt>(CallOperandVal)) {
                if (C->getZExtValue() <= 0xffffffff)
                    weight = CW_Constant;
            }
            break;
    }
    return weight;
}

/// LowerXConstraint - try to replace an X constraint, which matches anything,
/// with another that has more specific requirements based on the type of the
/// corresponding operand.
const char *Cse523TargetLowering::
LowerXConstraint(EVT ConstraintVT) const {
    // FP X constraints get lowered to SSE1/2 registers if available, otherwise
    // 'f' like normal targets.
    if (ConstraintVT.isFloatingPoint()) {
        if (Subtarget->hasSSE2())
            return "Y";
        if (Subtarget->hasSSE1())
            return "x";
    }

    return TargetLowering::LowerXConstraint(ConstraintVT);
}

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
void Cse523TargetLowering::LowerAsmOperandForConstraint(SDValue Op,
        std::string &Constraint,
        std::vector<SDValue>&Ops,
        SelectionDAG &DAG) const {
    SDValue Result(0, 0);

    // Only support length 1 constraints for now.
    if (Constraint.length() > 1) return;

    char ConstraintLetter = Constraint[0];
    switch (ConstraintLetter) {
        default: break;
        case 'I':
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                     if (C->getZExtValue() <= 31) {
                         Result = DAG.getTargetConstant(C->getZExtValue(), Op.getValueType());
                         break;
                     }
                 }
                 return;
        case 'J':
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                     if (C->getZExtValue() <= 63) {
                         Result = DAG.getTargetConstant(C->getZExtValue(), Op.getValueType());
                         break;
                     }
                 }
                 return;
        case 'K':
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                     if (isInt<8>(C->getSExtValue())) {
                         Result = DAG.getTargetConstant(C->getZExtValue(), Op.getValueType());
                         break;
                     }
                 }
                 return;
        case 'N':
                 if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                     if (C->getZExtValue() <= 255) {
                         Result = DAG.getTargetConstant(C->getZExtValue(), Op.getValueType());
                         break;
                     }
                 }
                 return;
        case 'e': {
                      // 32-bit signed value
                      if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                          if (ConstantInt::isValueValidForType(Type::getInt32Ty(*DAG.getContext()),
                                      C->getSExtValue())) {
                              // Widen to 64 bits here to get it sign extended.
                              Result = DAG.getTargetConstant(C->getSExtValue(), MVT::i64);
                              break;
                          }
                          // FIXME gcc accepts some relocatable values here too, but only in certain
                          // memory models; it's complicated.
                      }
                      return;
                  }
        case 'Z': {
                      // 32-bit unsigned value
                      if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
                          if (ConstantInt::isValueValidForType(Type::getInt32Ty(*DAG.getContext()),
                                      C->getZExtValue())) {
                              Result = DAG.getTargetConstant(C->getZExtValue(), Op.getValueType());
                              break;
                          }
                      }
                      // FIXME gcc accepts some relocatable values here too, but only in certain
                      // memory models; it's complicated.
                      return;
                  }
        case 'i': {
                      // Literal immediates are always ok.
                      if (ConstantSDNode *CST = dyn_cast<ConstantSDNode>(Op)) {
                          // Widen to 64 bits here to get it sign extended.
                          Result = DAG.getTargetConstant(CST->getSExtValue(), MVT::i64);
                          break;
                      }

                      // In any sort of PIC mode addresses need to be computed at runtime by
                      // adding in a register or some sort of table lookup.  These can't
                      // be used as immediates.
                      if (Subtarget->isPICStyleGOT() || Subtarget->isPICStyleStubPIC())
                          return;

                      // If we are in non-pic codegen mode, we allow the address of a global (with
                      // an optional displacement) to be used with 'i'.
                      GlobalAddressSDNode *GA = 0;
                      int64_t Offset = 0;

                      // Match either (GA), (GA+C), (GA+C1+C2), etc.
                      while (1) {
                          if ((GA = dyn_cast<GlobalAddressSDNode>(Op))) {
                              Offset += GA->getOffset();
                              break;
                          } else if (Op.getOpcode() == ISD::ADD) {
                              if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op.getOperand(1))) {
                                  Offset += C->getZExtValue();
                                  Op = Op.getOperand(0);
                                  continue;
                              }
                          } else if (Op.getOpcode() == ISD::SUB) {
                              if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op.getOperand(1))) {
                                  Offset += -C->getZExtValue();
                                  Op = Op.getOperand(0);
                                  continue;
                              }
                          }

                          // Otherwise, this isn't something we can handle, reject it.
                          return;
                      }

                      const GlobalValue *GV = GA->getGlobal();
                      // If we require an extra load to get this address, as in PIC mode, we
                      // can't accept it.
                      if (isGlobalStubReference(Subtarget->ClassifyGlobalReference(GV,
                                      getTargetMachine())))
                          return;

                      Result = DAG.getTargetGlobalAddress(GV, SDLoc(Op),
                              GA->getValueType(0), Offset);
                      break;
                  }
    }

    if (Result.getNode()) {
        Ops.push_back(Result);
        return;
    }
    return TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

std::pair<unsigned, const TargetRegisterClass*>
Cse523TargetLowering::getRegForInlineAsmConstraint(const std::string &Constraint,
        MVT VT) const {
    // First, see if this is a constraint that directly corresponds to an LLVM
    // register class.
    if (Constraint.size() == 1) {
        // GCC Constraint Letters
        switch (Constraint[0]) {
            default: break;
                     // TODO: Slight differences here in allocation order and leaving
                     // RIP in the class. Do they matter any more here than they do
                     // in the normal allocation?
            case 'q':   // GENERAL_REGS in 64-bit mode, Q_REGS in 32-bit mode.
                     if (VT == MVT::i64 || VT == MVT::f64)
                         return std::make_pair(0U, &Cse523::GR64RegClass);
                     break;
                     // 32-bit fallthrough
            case 'Q':   // Q_REGS
                     if (VT == MVT::i64)
                         return std::make_pair(0U, &Cse523::GR64_ABCDRegClass);
                     break;
            case 'r':   // GENERAL_REGS
            case 'l':   // INDEX_REGS
                     return std::make_pair(0U, &Cse523::GR64RegClass);
            case 'R':   // LEGACY_REGS
                     return std::make_pair(0U, &Cse523::GR64_NOREXRegClass);
            case 'y':   // MMX_REGS if MMX allowed.
            case 'Y':   // SSE_REGS if SSE2 allowed
            case 'f':  // FP Stack registers.
            case 'x':   // SSE_REGS if SSE1 allowed or AVX_REGS if AVX allowed
                     assert(0);
                     break;
        }
    }

    // Use the default implementation in TargetLowering to convert the register
    // constraint into a member of a register class.
    std::pair<unsigned, const TargetRegisterClass*> Res;
    Res = TargetLowering::getRegForInlineAsmConstraint(Constraint, VT);

    // Not found as a standard register?
    if (Res.second == 0) {

        // Unsupported Constraints
        // Map st(0) -> st(7) -> ST0
        if (Constraint.size() == 7 && Constraint[0] == '{' &&
                tolower(Constraint[1]) == 's' &&
                tolower(Constraint[2]) == 't' &&
                Constraint[3] == '(' &&
                (Constraint[4] >= '0' && Constraint[4] <= '7') &&
                Constraint[5] == ')' &&
                Constraint[6] == '}') {
            assert(0);
        }
        // GCC allows "st(0)" to be called just plain "st".
        if (StringRef("{st}").equals_lower(Constraint)) {
            assert(0);
        }
        // 'A' means EAX + EDX.
        if (Constraint == "A") {
            assert(0);
        }

        // flags -> EFLAGS
        if (StringRef("{flags}").equals_lower(Constraint)) {
            Res.first = Cse523::EFLAGS;
            Res.second = &Cse523::CCRRegClass;
            return Res;
        }

        unsigned DestReg = 0;

        if      (StringRef("{ax}").equals_lower(Constraint))  DestReg = Cse523::RAX;
        else if (StringRef("{bx}").equals_lower(Constraint))  DestReg = Cse523::RBX;
        else if (StringRef("{cx}").equals_lower(Constraint))  DestReg = Cse523::RCX;
        else if (StringRef("{dx}").equals_lower(Constraint))  DestReg = Cse523::RDX;
        else if (StringRef("{si}").equals_lower(Constraint))  DestReg = Cse523::RSI;
        else if (StringRef("{di}").equals_lower(Constraint))  DestReg = Cse523::RDI;
        else if (StringRef("{bp}").equals_lower(Constraint))  DestReg = Cse523::RBP;
        else if (StringRef("{sp}").equals_lower(Constraint))  DestReg = Cse523::RSP;

        if (DestReg) {
            Res.first = DestReg;
            Res.second = &Cse523::GR64RegClass;
        }
    }

    return Res;
}
