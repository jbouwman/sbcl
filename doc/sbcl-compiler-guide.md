# SBCL Compiler Internals Guide

## Table of Contents

1. [Overview](#overview)
2. [Compiler Pipeline](#compiler-pipeline)
3. [Intermediate Representations](#intermediate-representations)
   - [IR1 (Intermediate Representation 1)](#ir1-intermediate-representation-1)
   - [IR2 (Intermediate Representation 2)](#ir2-intermediate-representation-2)
4. [Key Data Structures](#key-data-structures)
5. [Virtual Machine Abstraction](#virtual-machine-abstraction)
6. [Foreign Function Interface (FFI)](#foreign-function-interface-ffi)
7. [The SRBV Branch: Struct Return-by-Value](#the-srbv-branch-struct-return-by-value)
8. [Glossary](#glossary)

---

## Overview

SBCL (Steel Bank Common Lisp) inherits its compiler from CMU Common Lisp, which was developed at Carnegie Mellon University. The compiler is a sophisticated, multi-pass optimizing compiler that transforms Lisp source code through several intermediate representations before generating native machine code.

The compiler's design philosophy emphasizes:
- **Retargetability**: The compiler can be adapted to different CPU architectures through a well-defined VM (Virtual Machine) interface
- **Optimization**: Multiple optimization passes operate on intermediate representations
- **Type inference**: Extensive type propagation enables efficient code generation
- **Portability**: Platform-independent code is cleanly separated from platform-specific backends

---

## Compiler Pipeline

The SBCL compiler processes code through these major phases:

```
┌─────────────────┐
│   Source Code   │
└────────┬────────┘
         │ Parsing
         ▼
┌─────────────────┐
│  IR1 Conversion │  ← Source transforms, macroexpansion
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ IR1 Optimization│  ← Type inference, constraint propagation
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  LTN (Local     │  ← Policy decisions, TN allocation
│  TN Numbering)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ IR2 Conversion  │  ← Template selection, VOP generation
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Register Alloc  │  ← Pack, lifetime analysis
│   (Pack)        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Code Generation │  ← Assembly, emit machine code
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Machine Code   │
└─────────────────┘
```

### Key Files in the Pipeline

| Phase | Primary Files |
|-------|--------------|
| IR1 Conversion | `ir1tran.lisp`, `ir1-translators.lisp` |
| IR1 Optimization | `ir1opt.lisp`, `constraint.lisp`, `ir1util.lisp` |
| Type Derivation | `srctran.lisp`, `typetran.lisp`, `fndb.lisp` |
| LTN Analysis | `ltn.lisp`, `gtn.lisp` |
| IR2 Conversion | `ir2tran.lisp` |
| Register Allocation | `pack.lisp`, `life.lisp` |
| Code Generation | `codegen.lisp`, `assem.lisp` |

---

## Intermediate Representations

### IR1 (Intermediate Representation 1)

IR1 is a **flow graph** representation of the source code. It preserves high-level Lisp semantics while making control flow explicit.

#### Core Concepts

**Nodes**: Represent operations in the program
- `REF`: Reference to a variable or constant
- `COMBINATION`: Function application (call)
- `IF`: Conditional branch
- `BIND`: Variable binding (let/let*)
- `RETURN`: Return from function
- `SET`: Variable assignment (setq)
- `CAST`: Type assertion
- `ENTRY`/`EXIT`: Non-local control transfer markers

**Continuations**: Represent "where control goes next"
- **CTRANs** (Control TRANsfers): Model control flow between nodes
- **LVARs** (Linear VARiables): Model data flow (values passed between nodes)

```
┌─────────────────────────────────────────────────────────┐
│  Note on Historical Terminology:                        │
│  "Continuation" was split into CTRAN and LVAR to       │
│  decouple control flow from data flow. Many comments   │
│  still reference the old unified "continuation" model. │
└─────────────────────────────────────────────────────────┘
```

**Blocks (CBLOCK)**: A sequence of nodes with single entry/exit points
- Basic blocks for control flow analysis
- Linked together to form the control flow graph

**Components**: A collection of related functions being compiled together
- Supports block compilation and cross-function optimization

**Lambda**: Represents functions
- `CLAMBDA`: The main lambda structure
- `FUNCTIONAL`: Parent structure for all function-like entities
- `OPTIONAL-DISPATCH`: Handles functions with optional/keyword arguments

#### Lexical Environment

The `LEXENV` structure tracks:
- Variable bindings (`vars`)
- Function bindings (`funs`)
- Block names (`blocks`)
- Tag names for `tagbody`/`go` (`tags`)
- Type declarations (`type-restrictions`)
- Current `OPTIMIZE` policy

#### IR1 Optimization

**Transforms** rewrite IR1 for efficiency:
- **Source Transforms**: Macro-like rewrites before IR1 conversion
- **IR1 Transforms** (`deftransform`): Pattern-based rewrites during IR1 optimization
- **Derive-Type Optimizers**: Infer result types from argument types

**Constraint Propagation**: Tracks value constraints through:
- Type checks (`typep`)
- Numeric comparisons
- Equality tests

### IR2 (Intermediate Representation 2)

IR2 is a **virtual machine instruction** representation. It's closer to the target machine while still being architecture-independent.

#### Core Concepts

**VOPs (Virtual OPerations)**: The fundamental unit of IR2
- Each VOP corresponds to a pattern of machine instructions
- Defined via `define-vop` with precise specifications of:
  - Arguments and their storage requirements
  - Results and their storage classes
  - Temporary registers needed
  - Code generation (:generator clause)

**TNs (Temporary Names)**: Virtual registers
- Abstract locations for values during compilation
- Later assigned to physical registers or stack locations
- Types:
  - **Normal TNs**: Allocated by pack
  - **Wired TNs**: Fixed to specific physical locations
  - **Restricted TNs**: Must be in certain storage classes
  - **Component TNs**: Live throughout a component
  - **Alias TNs**: Share storage with other TNs

**Templates**: VOP descriptors
- Specify operand constraints
- Define cost model for instruction selection
- Multiple templates for the same operation allow backend optimization

**IR2-BLOCK**: IR2 annotation for basic blocks
- Contains the list of VOPs
- Tracks live TNs at entry/exit

---

## Key Data Structures

### Storage Classes (SCs)

Storage classes categorize where values can live:

```lisp
;; Common storage classes on x86-64:
DESCRIPTOR-REG    ; General-purpose register holding Lisp objects
UNSIGNED-REG      ; Register holding unsigned integers
SIGNED-REG        ; Register holding signed integers
SAP-REG           ; System Area Pointer (raw memory address)
SINGLE-REG        ; SSE register for single-float
DOUBLE-REG        ; SSE register for double-float
CONTROL-STACK     ; Stack slot for Lisp objects
UNSIGNED-STACK    ; Stack slot for unsigned integers
CONSTANT          ; Compile-time constant
```

### Storage Bases (SBs)

Storage bases are collections of storage locations:
- `:FINITE`: Fixed set of locations (e.g., CPU registers)
- `:UNBOUNDED`: Growable (e.g., stack)
- `:NON-PACKED`: Not subject to register allocation

### Primitive Types

Map Lisp types to VM representations:
- `T`: Any Lisp object
- `FIXNUM`: Small integer (immediate)
- `SINGLE-FLOAT`, `DOUBLE-FLOAT`: Floating-point numbers
- `SYSTEM-AREA-POINTER`: Raw memory address
- `UNSIGNED-BYTE-64`: 64-bit unsigned integer

---

## Virtual Machine Abstraction

### VOP Definition

VOPs are defined with `define-vop`:

```lisp
(define-vop (two-arg-+)
  (:translate +)                           ; Implements CL:+
  (:policy :fast-safe)                     ; Safety/speed policy
  (:args (x :scs (any-reg) :target r)      ; First argument
         (y :scs (any-reg)))               ; Second argument
  (:results (r :scs (any-reg)))            ; Result
  (:generator 1                            ; Cost = 1
    (inst add r x y)))                     ; Emit ADD instruction
```

### Key VOP Clauses

| Clause | Purpose |
|--------|---------|
| `:translate` | Links VOP to Lisp function |
| `:args` | Input operand specifications |
| `:results` | Output operand specifications |
| `:temporary` | Scratch registers needed |
| `:policy` | Which optimization policies allow this VOP |
| `:generator` | Code that emits assembly |
| `:guard` | Runtime condition for VOP selection |

### Backend Organization

Each target architecture has files in `src/compiler/{arch}/`:
- `vm.lisp`: Storage classes, primitive types
- `insts.lisp`: Instruction definitions
- `move.lisp`: Move/coercion VOPs
- `call.lisp`: Calling convention
- `c-call.lisp`: Foreign function interface
- `arith.lisp`: Arithmetic operations
- `array.lisp`: Array operations
- `float.lisp`: Floating-point operations

---

## Foreign Function Interface (FFI)

The FFI allows Lisp to call C functions and vice versa. Key components:

### Alien Types

Defined in `src/code/c-call.lisp`:
- `alien-integer-type`: C integers
- `alien-single-float-type`, `alien-double-float-type`: Floats
- `alien-pointer-type`: Pointers
- `alien-record-type`: C structs/unions
- `alien-fun-type`: Function signatures

### Calling Convention

Each platform implements its ABI:
- **x86-64 (System V AMD64 ABI)**:
  - Integer args: RDI, RSI, RDX, RCX, R8, R9
  - Float args: XMM0-XMM7
  - Return: RAX (int), XMM0 (float)

- **ARM64 (AAPCS64)**:
  - Integer args: X0-X7
  - Float args: V0-V7
  - Return: X0 (int), V0 (float)

### Key FFI Transforms

`aliencomp.lisp` contains transforms for:
- `alien-funcall`: Call foreign functions
- `deport`: Convert Lisp values to C values
- `naturalize`: Convert C values to Lisp values

---

## The SRBV Branch: Struct Return-by-Value

The **srbv** (Struct Return By Value) branch adds support for C functions that return structs by value—a capability previously missing from SBCL's FFI.

### The Problem

Prior to this work, SBCL could not handle:

```c
struct Point { double x, y; };
struct Point make_point(double x, double y);  // Returns struct by value
```

Calling such functions from Lisp would silently corrupt memory.

### ABI Classification

The solution implements ABI-compliant struct classification:

#### System V AMD64 ABI (x86-64)

Structs are classified into **eightbytes** (8-byte chunks):

| Classification | Meaning |
|----------------|---------|
| `:INTEGER` | Fits in general-purpose register |
| `:SSE` | Fits in SSE register |
| `:MEMORY` | Must be returned via hidden pointer |

Rules:
1. Structs > 16 bytes → `:MEMORY`
2. Each eightbyte classified independently
3. If any eightbyte is `:MEMORY`, entire struct is `:MEMORY`
4. `:INTEGER` dominates `:SSE` in merged eightbytes

#### ARM64 AAPCS64

Additional concept: **HFA (Homogeneous Floating-point Aggregate)**

```
┌──────────────────────────────────────────────────────────┐
│ HFA: A struct containing 1-4 members of the same         │
│ floating-point type (float or double), returned in       │
│ floating-point registers V0-V3.                          │
└──────────────────────────────────────────────────────────┘
```

Rules:
1. Structs > 16 bytes (and not HFA) → `:MEMORY`
2. HFAs with 1-4 float/double members → float registers
3. Small structs (≤16 bytes) → X0, X1 registers
4. Large structs use hidden pointer in X8

### Implementation Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     aliencomp.lisp                              │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ struct-return-info(type)                                    ││
│  │   → (values in-registers-p eightbytes size)                 ││
│  └─────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ generate-struct-store-code(temps eightbytes result-sap)     ││
│  │   → SETF forms to store registers to memory                 ││
│  └─────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ alien-funcall transform                                     ││
│  │   → Handles register returns and hidden pointer returns     ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    code/c-call.lisp                             │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ struct-classification (defstruct)                           ││
│  │   - eightbytes: list of classifications                     ││
│  │   - size: byte size                                         ││
│  │   - alignment: required alignment                           ││
│  │   - memory-p: T if hidden pointer needed                    ││
│  └─────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ (record :arg-tn) / (record :result-tn)                      ││
│  │   → Dispatch to platform-specific handlers                  ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
                              │
           ┌──────────────────┴──────────────────┐
           ▼                                     ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│  arm64/c-call.lisp          │   │  x86-64/c-call.lisp         │
│  ─────────────────────────  │   │  ─────────────────────────  │
│  classify-struct-arm64()    │   │  classify-struct-x86-64()   │
│  hfa-base-type()            │   │  classify-field-x86-64()    │
│  record-arg-tn-arm64()      │   │  merge-classes()            │
│  record-result-tn-arm64()   │   │  record-arg-tn-x86-64()     │
│                             │   │  record-result-tn-x86-64()  │
│  VOPs:                      │   │                             │
│  - load-struct-int-arg      │   │  VOPs:                      │
│  - load-struct-single-arg   │   │  - load-struct-int-arg      │
│  - load-struct-double-arg   │   │  - load-struct-sse-arg      │
│  - set-struct-return-pointer│   │                             │
└─────────────────────────────┘   └─────────────────────────────┘
```

### Data Flow for Struct Return

**Small Struct (returned in registers):**

```
┌─────────────────────────────────────────────────────────────┐
│ 1. C function returns in RAX/RDX (x86-64) or X0/X1 (ARM64) │
│ 2. IR2 extracts values to temporary TNs                    │
│ 3. %make-alien allocates heap memory                       │
│ 4. generate-struct-store-code emits SETFs to copy to heap  │
│ 5. %sap-alien wraps pointer as alien object                │
└─────────────────────────────────────────────────────────────┘
```

**Large Struct (hidden pointer):**

```
┌─────────────────────────────────────────────────────────────┐
│ ARM64:                                                      │
│ 1. %make-alien allocates heap memory                        │
│ 2. Pointer passed in X8 (via set-struct-return-pointer VOP) │
│ 3. C function writes result directly to allocated memory    │
│ 4. %sap-alien wraps pointer as alien object                 │
├─────────────────────────────────────────────────────────────┤
│ x86-64:                                                     │
│ 1. %make-alien allocates heap memory                        │
│ 2. Pointer passed as implicit first argument (in RDI)       │
│ 3. C function writes result directly to allocated memory    │
│ 4. Pointer returned in RAX, wrapped as alien object         │
└─────────────────────────────────────────────────────────────┘
```

### Key Implementation Choices

1. **Heap Allocation for Returns**: All struct returns allocate heap memory. This simplifies GC interaction and avoids stack lifetime issues.

2. **Platform-Specific Classification**: Each architecture implements its own `classify-struct-*` function following its ABI exactly.

3. **Eightbyte Model**: Borrowed from the AMD64 ABI, the "eightbyte" abstraction generalizes to ARM64 with minor modifications.

4. **VOP-Based Loading**: New VOPs (`load-struct-int-arg`, `load-struct-sse-arg`, etc.) handle the details of loading struct fields into the correct registers.

5. **Transform-Level Integration**: The `alien-funcall` transform was extended rather than creating a separate code path, maintaining consistency with existing FFI handling.

---

## Glossary

| Term | Definition |
|------|------------|
| **AAPCS64** | ARM Architecture Procedure Call Standard for 64-bit. The calling convention for ARM64. |
| **ABI** | Application Binary Interface. Specifies calling conventions, data layout, and system calls. |
| **Alien** | SBCL's FFI representation of foreign (C) data types and functions. |
| **Backend** | The architecture-specific portion of the compiler (e.g., x86-64, ARM64). |
| **CBLOCK** | A basic block in IR1—a sequence of nodes with single entry/exit. |
| **CLAMBDA** | The IR1 representation of a Lisp function (lambda). |
| **Component** | A unit of compilation containing related functions. Enables cross-function optimization. |
| **CTRAN** | Control TRANsfer. Represents where control flows to in IR1. |
| **Deport** | Convert a Lisp value to its C representation for FFI calls. |
| **Eightbyte** | An 8-byte unit used for ABI classification of struct fields. |
| **GTN** | Global TN analysis. Allocates TNs for values that cross basic block boundaries. |
| **HFA** | Homogeneous Floating-point Aggregate. An ARM64 ABI concept for structs containing only floats. |
| **IR1** | Intermediate Representation 1. High-level flow graph preserving Lisp semantics. |
| **IR2** | Intermediate Representation 2. Lower-level VOP-based representation. |
| **LEXENV** | Lexical Environment. Tracks bindings, declarations, and policy during compilation. |
| **LTN** | Local TN analysis. Allocates TNs for values within basic blocks and selects VOPs. |
| **LVAR** | Linear VARiable. Represents a value (data flow) in IR1. |
| **Move** | A VOP that transfers values between storage classes. |
| **Naturalize** | Convert a C value to its Lisp representation after FFI calls. |
| **Node** | A single operation in IR1 (e.g., function call, variable reference). |
| **Pack** | Register allocation phase. Assigns physical locations to TNs. |
| **Policy** | Optimization settings (SPEED, SAFETY, DEBUG, SPACE, COMPILATION-SPEED). |
| **Primitive Type** | A VM-level type classification (e.g., FIXNUM, DOUBLE-FLOAT). |
| **SAP** | System Area Pointer. A raw machine address, not a Lisp object. |
| **SB** | Storage Base. A collection of storage locations (registers or stack). |
| **SC** | Storage Class. Categories of storage locations with specific properties. |
| **SRET** | Struct RETurn. The hidden pointer parameter for large struct returns. |
| **System V AMD64 ABI** | The standard calling convention for Unix-like x86-64 systems. |
| **Template** | A VOP descriptor specifying operand constraints and costs. |
| **TN** | Temporary Name. A virtual register during compilation, later packed to physical location. |
| **Transform** | A rewrite rule that converts one IR1 form to another, often for optimization. |
| **VOP** | Virtual OPeration. The fundamental instruction unit in IR2. |
| **Wired TN** | A TN that must reside in a specific physical location. |
| **XEP** | eXternal Entry Point. The entry point for a function called from unknown callers. |

---

## References

- CMU CL Compiler Documentation: `doc/internals/cmu/`
- SBCL Source: `src/compiler/`
- System V AMD64 ABI: https://gitlab.com/x86-psABIs/x86-64-ABI
- ARM64 AAPCS64: ARM IHI 0055

---

*This document describes the SBCL compiler as of the srbv branch, which adds struct-by-value FFI support for x86-64 and ARM64 platforms.*
