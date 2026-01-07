/*
 * simd-wrapper.c - C wrapper for SIMD intrinsics
 *
 * Apple's <simd/simd.h> functions are mostly inline/intrinsics and cannot
 * be called directly from FFI. This wrapper provides real library symbols
 * that can be linked against.
 *
 * Compile on macOS:
 *   clang -O3 -shared -o libsimd-wrapper.dylib simd-wrapper.c
 *
 * The wrapper exposes struct-by-value returns that demonstrate SBCL's
 * FFI capabilities with various sizes:
 *   - simd_float2:  8 bytes (2 × float)
 *   - simd_float3: 16 bytes (3 × float + padding)
 *   - simd_float4: 16 bytes (4 × float)
 *   - simd_float4x4: 64 bytes (4 × 4 × float)
 */

#include <simd/simd.h>
#include <math.h>

/* ============================================================================
 * Vector Creation
 * ============================================================================ */

simd_float2 simd_wrapper_make_float2(float x, float y) {
    return simd_make_float2(x, y);
}

simd_float3 simd_wrapper_make_float3(float x, float y, float z) {
    return simd_make_float3(x, y, z);
}

simd_float4 simd_wrapper_make_float4(float x, float y, float z, float w) {
    return simd_make_float4(x, y, z, w);
}

/* ============================================================================
 * Vector Arithmetic
 * ============================================================================ */

simd_float2 simd_wrapper_add2(simd_float2 a, simd_float2 b) {
    return a + b;
}

simd_float3 simd_wrapper_add3(simd_float3 a, simd_float3 b) {
    return a + b;
}

simd_float4 simd_wrapper_add4(simd_float4 a, simd_float4 b) {
    return a + b;
}

simd_float2 simd_wrapper_sub2(simd_float2 a, simd_float2 b) {
    return a - b;
}

simd_float3 simd_wrapper_sub3(simd_float3 a, simd_float3 b) {
    return a - b;
}

simd_float4 simd_wrapper_sub4(simd_float4 a, simd_float4 b) {
    return a - b;
}

simd_float2 simd_wrapper_mul2(simd_float2 a, simd_float2 b) {
    return a * b;
}

simd_float3 simd_wrapper_mul3(simd_float3 a, simd_float3 b) {
    return a * b;
}

simd_float4 simd_wrapper_mul4(simd_float4 a, simd_float4 b) {
    return a * b;
}

simd_float2 simd_wrapper_scale2(simd_float2 v, float s) {
    return v * s;
}

simd_float3 simd_wrapper_scale3(simd_float3 v, float s) {
    return v * s;
}

simd_float4 simd_wrapper_scale4(simd_float4 v, float s) {
    return v * s;
}

/* ============================================================================
 * Vector Operations
 * ============================================================================ */

float simd_wrapper_dot2(simd_float2 a, simd_float2 b) {
    return simd_dot(a, b);
}

float simd_wrapper_dot3(simd_float3 a, simd_float3 b) {
    return simd_dot(a, b);
}

float simd_wrapper_dot4(simd_float4 a, simd_float4 b) {
    return simd_dot(a, b);
}

float simd_wrapper_length2(simd_float2 v) {
    return simd_length(v);
}

float simd_wrapper_length3(simd_float3 v) {
    return simd_length(v);
}

float simd_wrapper_length4(simd_float4 v) {
    return simd_length(v);
}

simd_float2 simd_wrapper_normalize2(simd_float2 v) {
    return simd_normalize(v);
}

simd_float3 simd_wrapper_normalize3(simd_float3 v) {
    return simd_normalize(v);
}

simd_float4 simd_wrapper_normalize4(simd_float4 v) {
    return simd_normalize(v);
}

simd_float3 simd_wrapper_cross(simd_float3 a, simd_float3 b) {
    return simd_cross(a, b);
}

simd_float2 simd_wrapper_reflect2(simd_float2 v, simd_float2 n) {
    return simd_reflect(v, n);
}

simd_float3 simd_wrapper_reflect3(simd_float3 v, simd_float3 n) {
    return simd_reflect(v, n);
}

/* ============================================================================
 * Interpolation
 * ============================================================================ */

simd_float2 simd_wrapper_lerp2(simd_float2 a, simd_float2 b, float t) {
    return simd_mix(a, b, t);
}

simd_float3 simd_wrapper_lerp3(simd_float3 a, simd_float3 b, float t) {
    return simd_mix(a, b, t);
}

simd_float4 simd_wrapper_lerp4(simd_float4 a, simd_float4 b, float t) {
    return simd_mix(a, b, t);
}

/* ============================================================================
 * Matrix Creation (4x4)
 * ============================================================================ */

simd_float4x4 simd_wrapper_matrix_identity(void) {
    return matrix_identity_float4x4;
}

simd_float4x4 simd_wrapper_matrix_scale(float sx, float sy, float sz) {
    return simd_matrix(
        simd_make_float4(sx, 0, 0, 0),
        simd_make_float4(0, sy, 0, 0),
        simd_make_float4(0, 0, sz, 0),
        simd_make_float4(0, 0, 0, 1)
    );
}

simd_float4x4 simd_wrapper_matrix_translation(float tx, float ty, float tz) {
    return simd_matrix(
        simd_make_float4(1, 0, 0, 0),
        simd_make_float4(0, 1, 0, 0),
        simd_make_float4(0, 0, 1, 0),
        simd_make_float4(tx, ty, tz, 1)
    );
}

simd_float4x4 simd_wrapper_matrix_rotation_x(float radians) {
    float c = cosf(radians);
    float s = sinf(radians);
    return simd_matrix(
        simd_make_float4(1, 0, 0, 0),
        simd_make_float4(0, c, s, 0),
        simd_make_float4(0, -s, c, 0),
        simd_make_float4(0, 0, 0, 1)
    );
}

simd_float4x4 simd_wrapper_matrix_rotation_y(float radians) {
    float c = cosf(radians);
    float s = sinf(radians);
    return simd_matrix(
        simd_make_float4(c, 0, -s, 0),
        simd_make_float4(0, 1, 0, 0),
        simd_make_float4(s, 0, c, 0),
        simd_make_float4(0, 0, 0, 1)
    );
}

simd_float4x4 simd_wrapper_matrix_rotation_z(float radians) {
    float c = cosf(radians);
    float s = sinf(radians);
    return simd_matrix(
        simd_make_float4(c, s, 0, 0),
        simd_make_float4(-s, c, 0, 0),
        simd_make_float4(0, 0, 1, 0),
        simd_make_float4(0, 0, 0, 1)
    );
}

simd_float4x4 simd_wrapper_matrix_rotation_axis(simd_float3 axis, float radians) {
    return simd_matrix4x4(simd_quaternion(radians, axis));
}

/* ============================================================================
 * Matrix Operations
 * ============================================================================ */

simd_float4x4 simd_wrapper_matrix_multiply(simd_float4x4 a, simd_float4x4 b) {
    return simd_mul(a, b);
}

simd_float4 simd_wrapper_matrix_transform_point(simd_float4x4 m, simd_float4 v) {
    return simd_mul(m, v);
}

simd_float4x4 simd_wrapper_matrix_inverse(simd_float4x4 m) {
    return simd_inverse(m);
}

simd_float4x4 simd_wrapper_matrix_transpose(simd_float4x4 m) {
    return simd_transpose(m);
}

/* ============================================================================
 * Camera/Projection Matrices
 * ============================================================================ */

simd_float4x4 simd_wrapper_matrix_look_at(
    simd_float3 eye,
    simd_float3 center,
    simd_float3 up)
{
    simd_float3 z = simd_normalize(eye - center);
    simd_float3 x = simd_normalize(simd_cross(up, z));
    simd_float3 y = simd_cross(z, x);

    return simd_matrix(
        simd_make_float4(x.x, y.x, z.x, 0),
        simd_make_float4(x.y, y.y, z.y, 0),
        simd_make_float4(x.z, y.z, z.z, 0),
        simd_make_float4(-simd_dot(x, eye), -simd_dot(y, eye), -simd_dot(z, eye), 1)
    );
}

simd_float4x4 simd_wrapper_matrix_perspective(
    float fovy_radians,
    float aspect,
    float near,
    float far)
{
    float y_scale = 1.0f / tanf(fovy_radians * 0.5f);
    float x_scale = y_scale / aspect;
    float z_range = far - near;

    return simd_matrix(
        simd_make_float4(x_scale, 0, 0, 0),
        simd_make_float4(0, y_scale, 0, 0),
        simd_make_float4(0, 0, -(far + near) / z_range, -1),
        simd_make_float4(0, 0, -2 * far * near / z_range, 0)
    );
}

simd_float4x4 simd_wrapper_matrix_ortho(
    float left,
    float right,
    float bottom,
    float top,
    float near,
    float far)
{
    float rl = right - left;
    float tb = top - bottom;
    float fn = far - near;

    return simd_matrix(
        simd_make_float4(2.0f / rl, 0, 0, 0),
        simd_make_float4(0, 2.0f / tb, 0, 0),
        simd_make_float4(0, 0, -2.0f / fn, 0),
        simd_make_float4(-(right + left) / rl, -(top + bottom) / tb, -(far + near) / fn, 1)
    );
}

/* ============================================================================
 * Quaternion Operations
 * ============================================================================ */

simd_quatf simd_wrapper_quaternion_identity(void) {
    return simd_quaternion(0, 0, 0, 1);
}

simd_quatf simd_wrapper_quaternion_axis_angle(simd_float3 axis, float radians) {
    return simd_quaternion(radians, axis);
}

simd_quatf simd_wrapper_quaternion_multiply(simd_quatf q1, simd_quatf q2) {
    return simd_mul(q1, q2);
}

simd_quatf simd_wrapper_quaternion_normalize(simd_quatf q) {
    return simd_normalize(q);
}

simd_quatf simd_wrapper_quaternion_conjugate(simd_quatf q) {
    return simd_conjugate(q);
}

simd_quatf simd_wrapper_quaternion_slerp(simd_quatf q1, simd_quatf q2, float t) {
    return simd_slerp(q1, q2, t);
}

simd_float3 simd_wrapper_quaternion_rotate_vector(simd_quatf q, simd_float3 v) {
    return simd_act(q, v);
}

simd_float4x4 simd_wrapper_quaternion_to_matrix(simd_quatf q) {
    return simd_matrix4x4(q);
}

/*
 * ABI Notes for SBCL FFI:
 *
 * Struct sizes and return conventions:
 *
 * simd_float2 (8 bytes):
 *   x86-64: One SSE register (XMM0, lower 64 bits)
 *   ARM64: Two floats in s0, s1 (HFA)
 *
 * simd_float3 (16 bytes with padding):
 *   x86-64: One SSE register (XMM0, full 128 bits)
 *   ARM64: Three floats in s0, s1, s2 (HFA) - uses 4th slot for alignment
 *
 * simd_float4 (16 bytes):
 *   x86-64: One SSE register (XMM0, full 128 bits)
 *   ARM64: Four floats in s0, s1, s2, s3 (HFA)
 *
 * simd_float4x4 (64 bytes):
 *   x86-64: Hidden pointer in RDI, returned in RAX
 *   ARM64: Hidden pointer in x8
 *
 * simd_quatf (16 bytes):
 *   Same as simd_float4
 *
 * These demonstrate the full range of struct return behaviors
 * that SBCL's srbv branch needs to handle.
 */
