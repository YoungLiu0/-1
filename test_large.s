  .data
  .globl g_counter
g_counter:
  .word 0
  .globl g_result
g_result:
  .word 0

  .text
  .globl main
test_constant_prop:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
.L0:
  li t0, 10
  sw t0, -4(fp)
  li t1, 20
  sw t1, -8(fp)
  lw t2, -4(fp)
  lw t3, -8(fp)
  add t4, t2, t3
  sw t4, -12(fp)
  lw t5, -12(fp)
  li t6, 2
  mul s1, t5, t6
  sw s1, -16(fp)
  lw s2, -16(fp)
  li s3, 5
  sub s4, s2, s3
  mv a0, s4
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_copy_prop:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L1:
  lw t0, -4(fp)
  sw t0, -8(fp)
  lw t1, -8(fp)
  sw t1, -12(fp)
  lw t2, -12(fp)
  sw t2, -16(fp)
  lw t3, -16(fp)
  li t4, 10
  add t5, t3, t4
  sw t5, -20(fp)
  lw t6, -20(fp)
  mv a0, t6
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_cse:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
  sw a1, -8(fp)
.L2:
  lw t0, -4(fp)
  lw t1, -8(fp)
  add t2, t0, t1
  sw t2, -12(fp)
  lw t3, -4(fp)
  lw t4, -8(fp)
  add t5, t3, t4
  sw t5, -16(fp)
  lw t6, -4(fp)
  li s1, 2
  mul s2, t6, s1
  sw s2, -20(fp)
  lw s3, -4(fp)
  li s4, 2
  mul s5, s3, s4
  sw s5, -24(fp)
  lw s6, -12(fp)
  lw s7, -16(fp)
  add s8, s6, s7
  lw s9, -20(fp)
  add s10, s8, s9
  lw s11, -24(fp)
  add s10, s10, s11
  mv a0, s10
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_dead_code:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L3:
  li t0, 100
  sw t0, -8(fp)
  li t1, 200
  sw t1, -12(fp)
  lw t2, -4(fp)
  li t3, 2
  mul t4, t2, t3
  sw t4, -16(fp)
  beqz t5, if_end_2
then_0:
  li t6, 999
  sw t6, -20(fp)
  lw s1, -16(fp)
  mv a0, s1
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
if_end_2:
  lw s2, -16(fp)
  li s3, 1
  add s4, s2, s3
  mv a0, s4
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_algebra:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L4:
  lw t0, -4(fp)
  sw t0, -8(fp)
  lw t1, -4(fp)
  sw t1, -12(fp)
  li t2, 0
  sw t2, -16(fp)
  lw t3, -4(fp)
  sw t3, -20(fp)
  lw t4, -4(fp)
  sw t4, -24(fp)
  lw t5, -8(fp)
  lw t6, -12(fp)
  add s1, t5, t6
  lw s2, -16(fp)
  add s3, s1, s2
  lw s4, -20(fp)
  add s5, s3, s4
  lw s6, -24(fp)
  add s7, s5, s6
  mv a0, s7
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_unreachable:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
then_0:
  lw t0, -4(fp)
  li t1, 10
  add t2, t0, t1
  mv a0, t2
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
test_loop:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L5:
  li t0, 0
  sw t0, -8(fp)
  li t1, 0
  sw t1, -12(fp)
  li t2, 5
  sw t2, -16(fp)
while_start_0:
  beqz t3, while_end_2
while_body_1:
  lw t4, -16(fp)
  sw t4, -20(fp)
  lw t5, -8(fp)
  lw t6, -20(fp)
  li s1, 2
  mul s2, t6, s1
  add s3, t5, s2
  sw s3, -8(fp)
  lw s4, -12(fp)
  li s5, 1
  add s6, s4, s5
  sw s6, -12(fp)
  j while_start_0
while_end_2:
  lw s7, -8(fp)
  mv a0, s7
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_nested:
  addi sp, sp, -48
  sw ra, 44(sp)
  sw fp, 40(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
  sw a1, -8(fp)
.L6:
  li t0, 10
  sw t0, -12(fp)
  li t1, 20
  sw t1, -16(fp)
  li t2, 0
  sw t2, -20(fp)
  li t3, 0
  sw t3, -24(fp)
  beqz t4, else_1
then_0:
  lw t5, -12(fp)
  lw t6, -16(fp)
  add s1, t5, t6
  sw s1, -20(fp)
  beqz s2, else_4
then_3:
  lw s3, -20(fp)
  sw s3, -28(fp)
  lw s4, -28(fp)
  sw s4, -32(fp)
  lw s5, -32(fp)
  li s6, 2
  mul s7, s5, s6
  sw s7, -24(fp)
  j if_end_5
else_4:
  lw s8, -20(fp)
  li s9, 10
  add s10, s8, s9
  sw s10, -24(fp)
if_end_5:
  j if_end_2
else_1:
  li s11, 0
  sw s11, -24(fp)
if_end_2:
  lw s7, -24(fp)
  mv a0, s7
  lw ra, 44(sp)
  lw fp, 40(sp)
  addi sp, sp, 48
  ret
helper:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L7:
  lw t0, -4(fp)
  lw t1, -4(fp)
  mul t2, t0, t1
  mv a0, t2
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
test_global_and_call:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L8:
  li t0, 100
  sw t0, -8(fp)
  lw t1, -8(fp)
  sw t1, -12(fp)
  lw t2, -4(fp)
  la t3, g_counter
  sw t2, 0(t3)
  lw t4, -12(fp)
  mv a0, t4
  call helper
  mv t5, a0
  la t6, g_result
  sw t5, 0(t6)
  la s1, g_result
  lw s2, 0(s1)
  la s3, g_counter
  lw s4, 0(s3)
  add s5, s2, s4
  mv a0, s5
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_complex_expr:
  addi sp, sp, -48
  sw ra, 44(sp)
  sw fp, 40(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
  sw a1, -8(fp)
  sw a2, -12(fp)
.L9:
  lw t0, -4(fp)
  lw t1, -8(fp)
  add t2, t0, t1
  li t3, 2
  mul t4, t2, t3
  sw t4, -16(fp)
  lw t5, -4(fp)
  lw t6, -8(fp)
  add s1, t5, t6
  li s2, 2
  mul s3, s1, s2
  sw s3, -20(fp)
  lw s4, -16(fp)
  lw s5, -20(fp)
  add s6, s4, s5
  sw s6, -24(fp)
  lw s7, -12(fp)
  li s8, 3
  mul s9, s7, s8
  lw s10, -12(fp)
  li s11, 3
  mul s8, s10, s11
  sub s9, s9, s8
  sw s9, -28(fp)
  lw s10, -24(fp)
  lw s11, -28(fp)
  add t0, s10, s11
  mv a0, t0
  lw ra, 44(sp)
  lw fp, 40(sp)
  addi sp, sp, 48
  ret
test_short_circuit:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
  sw a1, -8(fp)
.L10:
  li t0, 0
  sw t0, -12(fp)
  beqz t1, and_false_3
.L11:
  j and_end_4
and_false_3:
and_end_4:
  beqz t2, else_1
then_0:
  li t3, 1
  sw t3, -12(fp)
  j if_end_2
else_1:
  li t4, 0
  sw t4, -12(fp)
if_end_2:
  bnez t5, or_true_8
.L12:
  j or_end_9
or_true_8:
or_end_9:
  beqz t6, if_end_7
then_5:
  lw s1, -12(fp)
  li s2, 10
  add s3, s1, s2
  sw s3, -12(fp)
if_end_7:
  lw s4, -12(fp)
  mv a0, s4
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_nested_loop:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L13:
  li t0, 0
  sw t0, -8(fp)
  li t1, 0
  sw t1, -12(fp)
  li t2, 3
  sw t2, -16(fp)
while_start_0:
  beqz t3, while_end_2
while_body_1:
  li t4, 0
  sw t4, -20(fp)
while_start_3:
  beqz t5, while_end_5
while_body_4:
  lw t6, -16(fp)
  sw t6, -24(fp)
  lw s1, -8(fp)
  lw s2, -24(fp)
  add s3, s1, s2
  sw s3, -8(fp)
  lw s4, -20(fp)
  li s5, 1
  add s6, s4, s5
  sw s6, -20(fp)
  j while_start_3
while_end_5:
  lw s7, -12(fp)
  li s8, 1
  add s9, s7, s8
  sw s9, -12(fp)
  j while_start_0
while_end_2:
  lw s10, -8(fp)
  mv a0, s10
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
test_conditional_const:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L14:
  li t0, 0
  sw t0, -8(fp)
  beqz t1, else_1
then_0:
  lw t2, -4(fp)
  li t3, 10
  add t4, t2, t3
  sw t4, -8(fp)
  j if_end_2
else_1:
  lw t5, -4(fp)
  li t6, 10
  sub s1, t5, t6
  sw s1, -8(fp)
if_end_2:
  lw s2, -8(fp)
  mv a0, s2
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
test_dataflow:
  addi sp, sp, -32
  sw ra, 28(sp)
  sw fp, 24(sp)
  addi fp, sp, 0
  sw a0, -4(fp)
.L15:
  lw t0, -4(fp)
  sw t0, -8(fp)
  lw t1, -8(fp)
  li t2, 5
  add t3, t1, t2
  sw t3, -12(fp)
  li t4, 0
  sw t4, -16(fp)
  li t5, 0
  sw t5, -20(fp)
  beqz t6, else_1
then_0:
  lw s1, -12(fp)
  li s2, 2
  mul s3, s1, s2
  sw s3, -16(fp)
  lw s4, -16(fp)
  lw s5, -8(fp)
  add s6, s4, s5
  sw s6, -20(fp)
  j if_end_2
else_1:
  lw s7, -12(fp)
  li s8, 3
  mul s9, s7, s8
  sw s9, -16(fp)
  lw s10, -16(fp)
  lw s11, -8(fp)
  sub s8, s10, s11
  sw s8, -20(fp)
if_end_2:
  lw s9, -20(fp)
  lw s10, -12(fp)
  add s11, s9, s10
  sw s11, -24(fp)
  lw t0, -24(fp)
  mv a0, t0
  lw ra, 28(sp)
  lw fp, 24(sp)
  addi sp, sp, 32
  ret
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, sp, 0
.L16:
  li t0, 0
  sw t0, -4(fp)
  li t1, 0
  sw t1, -8(fp)
  call test_constant_prop
  mv t2, a0
  sw t2, -8(fp)
  lw t3, -4(fp)
  lw t4, -8(fp)
  add t5, t3, t4
  sw t5, -4(fp)
  li t6, 42
  mv a0, t6
  call test_copy_prop
  mv s1, a0
  sw s1, -8(fp)
  lw s2, -4(fp)
  lw s3, -8(fp)
  add s4, s2, s3
  sw s4, -4(fp)
  li t6, 10
  mv a0, t6
  li s5, 20
  mv a1, s5
  call test_cse
  mv s6, a0
  sw s6, -8(fp)
  lw s7, -4(fp)
  lw s8, -8(fp)
  add s9, s7, s8
  sw s9, -4(fp)
  li t6, 5
  mv a0, t6
  call test_dead_code
  mv s10, a0
  sw s10, -8(fp)
  lw s11, -4(fp)
  lw s8, -8(fp)
  add s9, s11, s8
  sw s9, -4(fp)
  li t6, 100
  mv a0, t6
  call test_algebra
  mv s10, a0
  sw s10, -8(fp)
  lw s11, -4(fp)
  lw t0, -8(fp)
  add t1, s11, t0
  sw t1, -4(fp)
  li t6, 7
  mv a0, t6
  call test_unreachable
  mv t2, a0
  sw t2, -8(fp)
  lw t3, -4(fp)
  lw t4, -8(fp)
  add t5, t3, t4
  sw t5, -4(fp)
  li t6, 10
  mv a0, t6
  call test_loop
  mv t6, a0
  sw t6, -8(fp)
  lw s1, -4(fp)
  lw s2, -8(fp)
  add s3, s1, s2
  sw s3, -4(fp)
  li t6, 1
  mv a0, t6
  li s5, 1
  mv a1, s5
  call test_nested
  mv s4, a0
  sw s4, -8(fp)
  lw s5, -4(fp)
  lw s6, -8(fp)
  add s7, s5, s6
  sw s7, -4(fp)
  li t6, 8
  mv a0, t6
  call test_global_and_call
  mv s8, a0
  sw s8, -8(fp)
  lw s9, -4(fp)
  lw s10, -8(fp)
  add s11, s9, s10
  sw s11, -4(fp)
  li t6, 5
  mv a0, t6
  li s5, 10
  mv a1, s5
  li s6, 20
  mv a2, s6
  call test_complex_expr
  mv t0, a0
  sw t0, -8(fp)
  lw t1, -4(fp)
  lw t2, -8(fp)
  add t3, t1, t2
  sw t3, -4(fp)
  li t6, 5
  mv a0, t6
  li s5, 3
  mv a1, s5
  call test_short_circuit
  mv t4, a0
  sw t4, -8(fp)
  lw t5, -4(fp)
  lw t6, -8(fp)
  add s1, t5, t6
  sw s1, -4(fp)
  li t6, 5
  mv a0, t6
  call test_nested_loop
  mv s2, a0
  sw s2, -8(fp)
  lw s3, -4(fp)
  lw s4, -8(fp)
  add s5, s3, s4
  sw s5, -4(fp)
  li t6, 50
  mv a0, t6
  call test_conditional_const
  mv s6, a0
  sw s6, -8(fp)
  lw s7, -4(fp)
  lw s8, -8(fp)
  add s9, s7, s8
  sw s9, -4(fp)
  li t6, 15
  mv a0, t6
  call test_dataflow
  mv s10, a0
  sw s10, -8(fp)
  lw s11, -4(fp)
  lw t0, -8(fp)
  add t1, s11, t0
  sw t1, -4(fp)
  lw t2, -4(fp)
  mv a0, t2
  lw ra, 12(sp)
  lw fp, 8(sp)
  addi sp, sp, 16
  ret
