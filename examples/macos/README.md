# macOS Struct-by-Value FFI Examples

These examples demonstrate SBCL's struct-by-value return support (srbv branch)
with various macOS frameworks.

## Prerequisites

- macOS (tested on macOS 12+)
- SBCL built from the srbv branch
- Xcode Command Line Tools (for compiling the SIMD wrapper)

## Examples

### accelerate-blas-demo.lisp

Complete BLAS (Basic Linear Algebra Subprograms) demo using Apple's Accelerate
framework. Demonstrates Level 1 (vector), Level 2 (matrix-vector), and Level 3
(matrix-matrix) operations.

```lisp
(load "accelerate-blas-demo.lisp")
(blas-demo:run-demo)
```

### glkit-math.lisp

3D math operations using Apple's GLKit framework. Shows vector and quaternion
operations (small struct returns) plus matrix operations (hidden pointer returns).

```lisp
(load "glkit-math.lisp")
(glkit-math:demo)
```

### core-graphics-example.lisp

Core Graphics (Quartz 2D) bindings for 2D graphics. Demonstrates CGPoint (16 bytes),
CGRect (32 bytes), and CGAffineTransform (48 bytes) struct returns.

```lisp
(load "core-graphics-example.lisp")
(cg-example:run-demo)
```

### cmtime-example.lisp

Core Media time arithmetic for video/audio work. CMTime (24 bytes) and CMTimeRange
(48 bytes) provide precise rational time representation.

```lisp
(load "cmtime-example.lisp")
(cmtime-example:run-demo)
```

### objc-bridge.lisp

Minimal Objective-C runtime bridge demonstrating struct returns via
`objc_msgSend` and `objc_msgSend_stret`.

```lisp
(load "objc-bridge.lisp")
(objc-bridge:demo)
```

### simd-wrapper.c

C wrapper for Apple's SIMD intrinsics. These are inline functions in the headers
and need a wrapper to be callable from FFI.

Compile with:
```bash
clang -O3 -shared -o libsimd-wrapper.dylib simd-wrapper.c
```

## Struct Return Conventions

| Struct | Size | x86-64 | ARM64 |
|--------|------|--------|-------|
| CGPoint | 16 bytes | XMM0/XMM1 | d0/d1 (HFA) |
| CGSize | 16 bytes | XMM0/XMM1 | d0/d1 (HFA) |
| simd_float4 | 16 bytes | XMM0 | s0-s3 (HFA) |
| CMTime | 24 bytes | Hidden ptr | Hidden ptr (x8) |
| CGRect | 32 bytes | Hidden ptr | Hidden ptr (x8) |
| CGAffineTransform | 48 bytes | Hidden ptr | Hidden ptr (x8) |
| simd_float4x4 | 64 bytes | Hidden ptr | Hidden ptr (x8) |

## Key Concepts

1. **Small structs** (≤16 bytes) are returned in registers
2. **Large structs** (>16 bytes) use a hidden pointer argument
3. **HFA (Homogeneous Floating-point Aggregate)** on ARM64: structs of 1-4
   same-type floats use floating-point registers
4. **Eightbyte classification** on x86-64: each 8-byte chunk is classified as
   INTEGER or SSE

## Documentation

See `doc/sbcl-compiler-guide.md` for detailed information about SBCL's compiler
internals and the struct-by-value implementation.

See `doc/platform-abi-refactoring-plan.md` for a proposal to improve the
cross-platform abstraction.
