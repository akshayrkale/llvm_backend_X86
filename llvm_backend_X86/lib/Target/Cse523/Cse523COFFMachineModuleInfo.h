//===-- Cse523coffmachinemoduleinfo.h - Cse523 COFF MMI Impl ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is an MMI implementation for Cse523 COFF (windows) targets.
//
//===----------------------------------------------------------------------===//

#ifndef CSE523COFF_MACHINEMODULEINFO_H
#define CSE523COFF_MACHINEMODULEINFO_H

#include "Cse523MachineFunctionInfo.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/CodeGen/MachineModuleInfo.h"

namespace llvm {
  class Cse523MachineFunctionInfo;
  class DataLayout;

/// Cse523COFFMachineModuleInfo - This is a MachineModuleInfoImpl implementation
/// for Cse523 COFF targets.
class Cse523COFFMachineModuleInfo : public MachineModuleInfoImpl {
  DenseSet<MCSymbol const *> Externals;
public:
  Cse523COFFMachineModuleInfo(const MachineModuleInfo &) {}
  virtual ~Cse523COFFMachineModuleInfo();

  void addExternalFunction(MCSymbol* Symbol) {
    Externals.insert(Symbol);
  }

  typedef DenseSet<MCSymbol const *>::const_iterator externals_iterator;
  externals_iterator externals_begin() const { return Externals.begin(); }
  externals_iterator externals_end() const { return Externals.end(); }
};



} // end namespace llvm

#endif