  .text
  .globl mul
mul:
  addi sp, sp, -64
  sw ra, 60(sp)
  sw fp, 56(sp)
  addi fp, sp, 64
  sw s0, -12(fp)
  sw s1, -16(fp)
  sw s2, -20(fp)
  sw s3, -24(fp)
  sw s4, -28(fp)
  sw s5, -32(fp)
  sw s6, -36(fp)
  sw s7, -40(fp)
  sw s8, -44(fp)
  sw s9, -48(fp)
  sw s10, -52(fp)
  sw s11, -56(fp)
  sw a0, -60(fp)
  sw a1, -64(fp)
mul..L0:
  lw t0, -64(fp)
  lw t1, -60(fp)
  mul t2, t1, t0
  mv a0, t2
  lw s0, -12(fp)
  lw s1, -16(fp)
  lw s2, -20(fp)
  lw s3, -24(fp)
  lw s4, -28(fp)
  lw s5, -32(fp)
  lw s6, -36(fp)
  lw s7, -40(fp)
  lw s8, -44(fp)
  lw s9, -48(fp)
  lw s10, -52(fp)
  lw s11, -56(fp)
  lw ra, 60(sp)
  lw fp, 56(sp)
  addi sp, sp, 64
  ret

  .globl compute
compute:
  addi sp, sp, -112
  sw ra, 108(sp)
  sw fp, 104(sp)
  addi fp, sp, 112
  sw s0, -12(fp)
  sw s1, -16(fp)
  sw s2, -20(fp)
  sw s3, -24(fp)
  sw s4, -28(fp)
  sw s5, -32(fp)
  sw s6, -36(fp)
  sw s7, -40(fp)
  sw s8, -44(fp)
  sw s9, -48(fp)
  sw s10, -52(fp)
  sw s11, -56(fp)
  sw a0, -60(fp)
  sw a1, -64(fp)
  sw a2, -68(fp)
  sw a3, -72(fp)
  sw a4, -76(fp)
  sw a5, -80(fp)
  sw a6, -84(fp)
  sw a7, -88(fp)
  lw t0, 0(fp)
  sw t0, -92(fp)
  lw t0, 4(fp)
  sw t0, -96(fp)
compute..L1:
  lw t0, -96(fp)
  lw t1, -92(fp)
  lw t2, -88(fp)
  lw t3, -84(fp)
  lw t4, -80(fp)
  lw t5, -76(fp)
  lw t6, -72(fp)
  lw s1, -68(fp)
  lw s2, -64(fp)
  lw s3, -60(fp)
  add s4, s3, s2
  add s2, s4, s1
  add s1, s2, t6
  add t6, s1, t5
  add t5, t6, t4
  add t4, t5, t3
  add t3, t4, t2
  add t2, t3, t1
  add t1, t2, t0
  sw t1, -100(fp)
  lw t0, -100(fp)
  li t1, 100
  slt t2, t1, t0
  beqz t2, compute.if_end_2
compute.then_0:
  lw t0, -100(fp)
  li t1, 50
  sub t2, t0, t1
  sw t2, -100(fp)
compute.if_end_2:
compute.while_start_3:
  lw t0, -100(fp)
  li t1, 10
  slt t2, t1, t0
  beqz t2, compute.while_end_5
compute.while_body_4:
  lw t0, -100(fp)
  li t1, 2
  div t2, t0, t1
  sw t2, -100(fp)
  j compute.while_start_3
compute.while_end_5:
  lw t0, -100(fp)
  mv a0, t0
  lw s0, -12(fp)
  lw s1, -16(fp)
  lw s2, -20(fp)
  lw s3, -24(fp)
  lw s4, -28(fp)
  lw s5, -32(fp)
  lw s6, -36(fp)
  lw s7, -40(fp)
  lw s8, -44(fp)
  lw s9, -48(fp)
  lw s10, -52(fp)
  lw s11, -56(fp)
  lw ra, 108(sp)
  lw fp, 104(sp)
  addi sp, sp, 112
  ret

  .globl main
main:
  addi sp, sp, -80
  sw ra, 76(sp)
  sw fp, 72(sp)
  addi fp, sp, 80
  sw s0, -12(fp)
  sw s1, -16(fp)
  sw s2, -20(fp)
  sw s3, -24(fp)
  sw s4, -28(fp)
  sw s5, -32(fp)
  sw s6, -36(fp)
  sw s7, -40(fp)
  sw s8, -44(fp)
  sw s9, -48(fp)
  sw s10, -52(fp)
  sw s11, -56(fp)
main..L2:
  addi sp, sp, -28
  sw t0, 0(sp)
  sw t1, 4(sp)
  sw t2, 8(sp)
  sw t3, 12(sp)
  sw t4, 16(sp)
  sw t5, 20(sp)
  sw t6, 24(sp)
  li t0, 3
  mv a0, t0
  li t1, 4
  mv a1, t1
  call mul
  lw t0, 0(sp)
  lw t1, 4(sp)
  lw t2, 8(sp)
  lw t3, 12(sp)
  lw t4, 16(sp)
  lw t5, 20(sp)
  lw t6, 24(sp)
  addi sp, sp, 28
  mv t2, a0
  sw t2, -60(fp)
  addi sp, sp, -8
  addi sp, sp, -28
  sw t0, 0(sp)
  sw t1, 4(sp)
  sw t2, 8(sp)
  sw t3, 12(sp)
  sw t4, 16(sp)
  sw t5, 20(sp)
  sw t6, 24(sp)
  li t0, 1
  mv a0, t0
  li t1, 2
  mv a1, t1
  li t0, 3
  mv a2, t0
  li t0, 4
  mv a3, t0
  li t0, 5
  mv a4, t0
  li t0, 6
  mv a5, t0
  li t0, 7
  mv a6, t0
  li t0, 8
  mv a7, t0
  li t0, 9
  sw t0, 0(sp)
  li t0, 10
  sw t0, 4(sp)
  call compute
  lw t0, 0(sp)
  lw t1, 4(sp)
  lw t2, 8(sp)
  lw t3, 12(sp)
  lw t4, 16(sp)
  lw t5, 20(sp)
  lw t6, 24(sp)
  addi sp, sp, 28
  addi sp, sp, 8
  mv t0, a0
  sw t0, -64(fp)
  lw t0, -64(fp)
  lw t1, -60(fp)
  add t2, t1, t0
  sw t2, -68(fp)
  lw t0, -68(fp)
  mv a0, t0
  lw s0, -12(fp)
  lw s1, -16(fp)
  lw s2, -20(fp)
  lw s3, -24(fp)
  lw s4, -28(fp)
  lw s5, -32(fp)
  lw s6, -36(fp)
  lw s7, -40(fp)
  lw s8, -44(fp)
  lw s9, -48(fp)
  lw s10, -52(fp)
  lw s11, -56(fp)
  lw ra, 76(sp)
  lw fp, 72(sp)
  addi sp, sp, 80
  ret
