//===-- Cse523RegisterInfo.h - Cse523 Register Information Impl -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Cse523 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef Cse523REGISTERINFO_H
#define Cse523REGISTERINFO_H

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "Cse523GenRegisterInfo.inc"

namespace llvm {
  class Type;
  class TargetInstrInfo;
  class Cse523TargetMachine;

class Cse523RegisterInfo : public Cse523GenRegisterInfo {
public:
  Cse523TargetMachine &TM;

private:
  /// Is64Bit - Is the target 64-bits.
  ///
  bool Is64Bit;

  /// IsWin64 - Is the target on of win64 flavours
  ///
  bool IsWin64;

  /// SlotSize - Stack slot size in bytes.
  ///
  unsigned SlotSize;

  /// StackPtr - Cse523 physical register used as stack ptr.
  ///
  unsigned StackPtr;

  /// FramePtr - Cse523 physical register used as frame ptr.
  ///
  unsigned FramePtr;

  /// BasePtr - Cse523 physical register used as a base ptr in complex stack
  /// frames. I.e., when we need a 3rd base, not just SP and FP, due to
  /// variable size stack objects.
  unsigned BasePtr;

public:
  Cse523RegisterInfo(Cse523TargetMachine &tm);

  // FIXME: This should be tablegen'd like getDwarfRegNum is
  int getSEHRegNum(unsigned i) const;

  /// getCompactUnwindRegNum - This function maps the register to the number for
  /// compact unwind encoding. Return -1 if the register isn't valid.
  int getCompactUnwindRegNum(unsigned RegNum, bool isEH) const;

  /// Code Generation virtual methods...
  ///
  virtual bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const;

  /// getMatchingSuperRegClass - Return a subclass of the specified register
  /// class A so that each register in it has a sub-register of the
  /// specified sub-register index which is in the specified register class B.
  virtual const TargetRegisterClass *
  getMatchingSuperRegClass(const TargetRegisterClass *A,
                           const TargetRegisterClass *B, unsigned Idx) const;

  virtual const TargetRegisterClass *
  getSubClassWithSubReg(const TargetRegisterClass *RC, unsigned Idx) const;

  const TargetRegisterClass*
  getLargestLegalSuperClass(const TargetRegisterClass *RC) const;

  /// getPointerRegClass - Returns a TargetRegisterClass used for pointer
  /// values.
  const TargetRegisterClass *
  getPointerRegClass(const MachineFunction &MF, unsigned Kind = 0) const;

  /// getCrossCopyRegClass - Returns a legal register class to copy a register
  /// in the specified class to or from. Returns NULL if it is possible to copy
  /// between a two registers of the specified class.
  const TargetRegisterClass *
  getCrossCopyRegClass(const TargetRegisterClass *RC) const;

  unsigned getRegPressureLimit(const TargetRegisterClass *RC,
                               MachineFunction &MF) const;

  /// getCalleeSavedRegs - Return a null-terminated list of all of the
  /// callee-save registers on this target.
  const uint16_t *getCalleeSavedRegs(const MachineFunction* MF = 0) const;
  const uint32_t *getCallPreservedMask(CallingConv::ID) const;
  const uint32_t *getNoPreservedMask() const;

  /// getReservedRegs - Returns a bitset indexed by physical register number
  /// indicating if a register is a special register that has particular uses and
  /// should be considered unavailable at all times, e.g. SP, RA. This is used by
  /// register scavenger to determine what registers are free.
  BitVector getReservedRegs(const MachineFunction &MF) const;

  bool hasBasePointer(const MachineFunction &MF) const;

  bool canRealignStack(const MachineFunction &MF) const;

  bool needsStackRealignment(const MachineFunction &MF) const;

  bool hasReservedSpillSlot(const MachineFunction &MF, unsigned Reg,
                            int &FrameIdx) const;

  void eliminateFrameIndex(MachineBasicBlock::iterator MI,
                           int SPAdj, unsigned FIOperandNum,
                           RegScavenger *RS = NULL) const;

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const;
  unsigned getStackRegister() const { return StackPtr; }
  unsigned getBaseRegister() const { return BasePtr; }
  // FIXME: Move to FrameInfok
  unsigned getSlotSize() const { return SlotSize; }
};

// getCse523SubSuperRegister - Cse523 utility function. It returns the sub or super
// register of a specific Cse523 register.
// e.g. getCse523SubSuperRegister(Cse523::EAX, MVT::i16) return Cse523:AX
unsigned getCse523SubSuperRegister(unsigned, MVT::SimpleValueType, bool High=false);

//get512BitRegister - Cse523 utility - returns 512-bit super register
unsigned get512BitSuperRegister(unsigned Reg);

} // End llvm namespace

#endif
