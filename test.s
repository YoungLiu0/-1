  .text
  .globl sum
sum:
  addi sp, sp, -144
  sw ra, 140(sp)
  sw fp, 136(sp)
  addi fp, sp, 144
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
  lw s1, 0(fp)
  sw s1, -92(fp)
  lw s1, 4(fp)
  sw s1, -96(fp)
  lw s1, 8(fp)
  sw s1, -100(fp)
  lw s1, 12(fp)
  sw s1, -104(fp)
  lw s1, 16(fp)
  sw s1, -108(fp)
  lw s1, 20(fp)
  sw s1, -112(fp)
  lw s1, 24(fp)
  sw s1, -116(fp)
  lw s1, 28(fp)
  sw s1, -120(fp)
  lw s1, 32(fp)
  sw s1, -124(fp)
  lw s1, 36(fp)
  sw s1, -128(fp)
  lw s1, 40(fp)
  sw s1, -132(fp)
  lw s1, 44(fp)
  sw s1, -136(fp)
  lw s1, -60(fp)
  lw s2, -64(fp)
  add s3, s1, s2
  lw s1, -68(fp)
  add s2, s3, s1
  lw s1, -72(fp)
  add s3, s2, s1
  lw s1, -76(fp)
  add s2, s3, s1
  lw s1, -80(fp)
  add s3, s2, s1
  lw s1, -84(fp)
  add s2, s3, s1
  lw s1, -88(fp)
  add s3, s2, s1
  lw s1, -92(fp)
  add s2, s3, s1
  lw s1, -96(fp)
  add s3, s2, s1
  lw s1, -100(fp)
  add s2, s3, s1
  lw s1, -104(fp)
  add s3, s2, s1
  lw s1, -108(fp)
  add s2, s3, s1
  lw s1, -112(fp)
  add s3, s2, s1
  lw s1, -116(fp)
  add s2, s3, s1
  lw s1, -120(fp)
  add s3, s2, s1
  lw s1, -124(fp)
  add s2, s3, s1
  lw s1, -128(fp)
  add s3, s2, s1
  lw s1, -132(fp)
  add s2, s3, s1
  lw s1, -136(fp)
  add s3, s2, s1
  mv a0, s3
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
  lw ra, 140(sp)
  lw fp, 136(sp)
  addi sp, sp, 144
  ret

  .globl main
main:
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
  addi sp, sp, -48
  li s1, 1
  mv a0, s1
  li s1, 2
  mv a1, s1
  li s1, 3
  mv a2, s1
  li s1, 4
  mv a3, s1
  li s1, 5
  mv a4, s1
  li s1, 6
  mv a5, s1
  li s1, 7
  mv a6, s1
  li s1, 8
  mv a7, s1
  li s1, 9
  sw s1, 0(sp)
  li s1, 10
  sw s1, 4(sp)
  li s1, 11
  sw s1, 8(sp)
  li s1, 12
  sw s1, 12(sp)
  li s1, 13
  sw s1, 16(sp)
  li s1, 14
  sw s1, 20(sp)
  li s1, 15
  sw s1, 24(sp)
  li s1, 16
  sw s1, 28(sp)
  li s1, 17
  sw s1, 32(sp)
  li s1, 18
  sw s1, 36(sp)
  li s1, 19
  sw s1, 40(sp)
  li s1, 20
  sw s1, 44(sp)
  call sum
  addi sp, sp, 48
  mv s1, a0
  mv a0, s1
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
