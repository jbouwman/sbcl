# SBCL FFI Struct-by-Value: Platform-Specific ABI Refactoring Plan

## Executive Summary

This document proposes refactoring the SBCL struct-by-value FFI implementation
to replace the AMD64-centric "eightbyte" abstraction with platform-native ABI
models. This change improves correctness, maintainability, and extensibility
for current and future architectures.

---

## Motivation

The current implementation in the `srbv` branch uses a `struct-classification`
structure with an `:eightbytes` slot—a concept native to the System V AMD64 ABI
but awkwardly mapped onto other platforms:

```lisp
;; Current: AMD64-centric model forced onto all platforms
(defstruct struct-classification
  (eightbytes nil :type list)  ; ← AMD64 concept
  (size 0)
  (alignment 1)
  (memory-p nil))
```

This leads to impedance mismatches:

| Platform | Native Concept | Current Mapping |
|----------|---------------|-----------------|
| x86-64 | Eightbyte classification | ✓ Native fit |
| ARM64 | HFA (Homogeneous Floating-point Aggregate) | Awkward: `:sse-single` × N |
| RISC-V | Field flattening | Not expressible cleanly |

---

## ABI Comparison

### Register Usage for Struct Returns

| Aspect | x86-64 (System V) | ARM64 (AAPCS64) | RISC-V (LP64D) |
|--------|-------------------|-----------------|----------------|
| **Core concept** | Eightbyte classification | HFA + size rules | Field flattening |
| **Small struct limit** | 16 bytes | 16 bytes | 16 bytes (2×XLEN) |
| **Float handling** | SSE class per eightbyte | HFA (1-4 same type) | Per-field register selection |
| **Mixed int+float** | Merged in eightbyte | Not HFA → integer regs | Split across int & float regs |
| **Hidden pointer** | Implicit first arg (RDI) | Explicit in x8 | First arg (a0), returned in a0 |

### Concrete Examples

```c
// Example 1: Two floats
struct { float x, y; };

// x86-64: One SSE eightbyte → XMM0
// ARM64:  HFA with 2 singles → s0, s1
// RISC-V: Flattened → fa0, fa1

// Example 2: Mixed int and float
struct { int32_t a; float b; };

// x86-64: One INTEGER eightbyte → RAX (both fields packed)
// ARM64:  Not HFA → x0 (both fields packed)
// RISC-V: Flattened → a0 (int), fa0 (float) — SPLIT!

// Example 3: Double and int64
struct { double d; int64_t i; };

// x86-64: SSE + INTEGER → XMM0, RAX
// ARM64:  Not HFA → x0, x1
// RISC-V: Flattened → fa0, a0 — different order than x86-64!
```

---

## Proposed Architecture

### Design Principles

1. **Platform-native models**: Each architecture defines its own classification structure
2. **Protocol-based abstraction**: Shared code uses generic functions, not structure slots
3. **No cross-platform leakage**: AMD64 concepts stay in AMD64 code
4. **Extensibility**: Adding new platforms requires only implementing the protocol

### Component Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                      aliencomp.lisp                                 │
│                   (platform-agnostic)                               │
│                                                                     │
│   Uses protocol:                                                    │
│   - classify-alien-record                                           │
│   - struct-return-memory-p                                          │
│   - struct-return-int-count / float-count                           │
│   - generate-struct-return-stores                                   │
│   - hidden-pointer-style                                            │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 │ protocol calls
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      src/code/c-call.lisp                           │
│                                                                     │
│   Protocol definitions (defgeneric)                                 │
│   No platform-specific code                                         │
└─────────────────────────────────────────────────────────────────────┘
                                 │
            ┌────────────────────┼────────────────────┐
            ▼                    ▼                    ▼
┌───────────────────┐ ┌───────────────────┐ ┌───────────────────┐
│ x86-64/c-call.lisp│ │ arm64/c-call.lisp │ │ riscv/c-call.lisp │
│                   │ │                   │ │                   │
│ x86-64-struct-    │ │ arm64-struct-     │ │ riscv-struct-     │
│   class           │ │   class           │ │   class           │
│                   │ │                   │ │                   │
│ - eightbytes      │ │ - hfa-p           │ │ - field-          │
│ - memory-p        │ │ - hfa-base-type   │ │   assignments     │
│                   │ │ - hfa-count       │ │ - memory-p        │
│                   │ │ - gpr-count       │ │                   │
│                   │ │ - memory-p        │ │                   │
│                   │ │                   │ │                   │
│ Methods:          │ │ Methods:          │ │ Methods:          │
│ - classify-...    │ │ - classify-...    │ │ - classify-...    │
│ - generate-...    │ │ - generate-...    │ │ - generate-...    │
└───────────────────┘ └───────────────────┘ └───────────────────┘
```

---

## Implementation Details

### Phase 1: Protocol Definition

**File: `src/code/c-call.lisp`**

```lisp
;;;; Platform-Agnostic Protocol for Struct Passing/Returning

(defgeneric classify-alien-record (type)
  (:documentation
   "Classify an alien record type for the current platform's ABI.
    Returns a platform-specific classification object."))

(defgeneric struct-return-memory-p (classification)
  (:documentation
   "Return T if the struct must be returned via hidden pointer."))

(defgeneric struct-return-register-count (classification)
  (:documentation
   "Total number of registers used for return (integer + floating-point)."))

(defgeneric struct-return-int-count (classification)
  (:documentation
   "Number of integer/general-purpose registers used for return."))

(defgeneric struct-return-float-count (classification)
  (:documentation
   "Number of floating-point registers used for return."))

(defgeneric struct-byte-size (classification)
  (:documentation
   "Total size of the struct in bytes."))

(defgeneric struct-alignment (classification)
  (:documentation
   "Required alignment of the struct in bytes."))

(defgeneric generate-struct-return-stores (classification temps result-sap)
  (:documentation
   "Generate forms to store register values to memory.
    TEMPS: list of gensyms bound to values from return registers
           (integer registers first, then floating-point)
    RESULT-SAP: gensym bound to the destination SAP
    Returns: list of SETF forms"))

(defgeneric hidden-pointer-style (classification)
  (:documentation
   "How the hidden struct-return pointer is passed, or NIL if not applicable.
    :IMPLICIT-FIRST-ARG - Pointer passed as implicit first integer argument,
                          returned in first integer return register (x86-64)
    :EXPLICIT-REGISTER  - Pointer passed in dedicated register not part of
                          normal argument sequence (ARM64: x8)
    :FIRST-ARG-RETURNED - Pointer passed as first argument AND returned
                          in first return register (RISC-V)"))

(defgeneric generate-struct-arg-loads (classification source-sap target-tns)
  (:documentation
   "Generate VOPs to load struct fields into argument registers.
    SOURCE-SAP: TN containing pointer to struct data
    TARGET-TNS: list of TNs for destination registers
    Returns: list of VOP emission forms"))
```

### Phase 2: Platform-Specific Implementations

#### x86-64 Implementation

**File: `src/compiler/x86-64/c-call.lisp`**

```lisp
;;;; x86-64 Struct Classification (System V AMD64 ABI)

(defstruct (x86-64-struct-class
            (:constructor make-x86-64-struct-class)
            (:copier nil))
  "Classification for x86-64 System V ABI struct passing.
   Uses the native eightbyte classification model."
  ;; List of eightbyte classes: :INTEGER, :SSE, :MEMORY
  (eightbytes nil :type list)
  ;; Total size in bytes
  (size 0 :type (unsigned-byte 32))
  ;; Required alignment
  (alignment 1 :type (unsigned-byte 16))
  ;; T if must use hidden pointer
  (memory-p nil :type boolean))
```

#### ARM64 Implementation

**File: `src/compiler/arm64/c-call.lisp`**

```lisp
;;;; ARM64 Struct Classification (AAPCS64)

(defstruct (arm64-struct-class
            (:constructor make-arm64-struct-class)
            (:copier nil))
  "Classification for ARM64 AAPCS64 struct passing."
  ;; Total size in bytes
  (size 0 :type (unsigned-byte 32))
  ;; Required alignment
  (alignment 1 :type (unsigned-byte 16))
  ;; T if must use hidden pointer (x8)
  (memory-p nil :type boolean)
  ;; HFA (Homogeneous Floating-point Aggregate) fields
  (hfa-p nil :type boolean)
  (hfa-base-type nil :type (member nil single-float double-float))
  (hfa-count 0 :type (integer 0 4))
  ;; For non-HFA small structs: number of X registers (0, 1, or 2)
  (gpr-count 0 :type (integer 0 2)))
```

#### RISC-V Implementation

**File: `src/compiler/riscv/c-call.lisp`**

```lisp
;;;; RISC-V Struct Classification (LP64D ABI)

(defstruct (riscv-struct-class
            (:constructor make-riscv-struct-class)
            (:copier nil))
  "Classification for RISC-V LP64D ABI struct passing.
   Uses the field-flattening model where each scalar field
   is independently assigned to its natural register type."
  ;; Total size in bytes
  (size 0 :type (unsigned-byte 32))
  ;; Required alignment
  (alignment 1 :type (unsigned-byte 16))
  ;; T if must use hidden pointer
  (memory-p nil :type boolean)
  ;; Field assignments: list of (offset size register-class)
  ;; register-class is :GPR or :FPR
  ;; Ordered by offset within struct
  (field-assignments nil :type list))
```

---

## Migration Timeline

```
Phase 1: Foundation (Week 1)
├── Define protocol in src/code/c-call.lisp
├── Create x86-64-struct-class with methods
├── Create arm64-struct-class with methods
├── Stub riscv-struct-class
└── Unit tests for classification

Phase 2: Integration (Week 2)
├── Refactor aliencomp.lisp to use protocol
├── Update derive-type optimizer
├── Remove old struct-classification uses
├── Integration tests

Phase 3: RISC-V (Week 3)
├── Complete RISC-V classification
├── Implement RISC-V VOPs
├── RISC-V-specific tests
└── Cross-compilation testing

Phase 4: Cleanup (Week 4)
├── Remove deprecated code
├── Update documentation
├── Performance benchmarks
├── Full test matrix (all platforms × all struct types)
```

---

## File Change Summary

| File | Action | Description |
|------|--------|-------------|
| `src/code/c-call.lisp` | **Modify** | Remove `struct-classification`, add protocol generics |
| `src/compiler/aliencomp.lisp` | **Modify** | Use protocol, remove platform conditionals |
| `src/compiler/x86-64/c-call.lisp` | **Modify** | Add `x86-64-struct-class`, implement protocol |
| `src/compiler/arm64/c-call.lisp` | **Modify** | Add `arm64-struct-class`, implement protocol |
| `src/compiler/riscv/c-call.lisp` | **Create** | New: `riscv-struct-class`, full implementation |
| `tests/alien-struct-classification.lisp` | **Create** | New: classification unit tests |
| `tests/alien-struct-by-value.impure.lisp` | **Modify** | Platform-conditional tests, RISC-V cases |

---

## Benefits

1. **Correctness**: Each platform implements its exact ABI specification
2. **Clarity**: Code reads in terms of platform-native concepts (HFA, flattening, eightbyte)
3. **Maintainability**: Changes to one platform don't risk breaking others
4. **Extensibility**: New platforms implement the protocol without touching shared code
5. **Testability**: Classification logic can be unit-tested per-platform

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Regression in existing platforms | Comprehensive test suite, parallel implementation |
| Protocol too restrictive for future ABIs | Design protocol with extension points |
| Performance overhead from generic functions | Profile; can specialize if needed |
| Cross-compilation complexity | Test matrix covers all host×target combinations |
