;=========================================================================
; testram.asm - Test Breadboard PC RAM and ROM
; Based on XI 8088 Bios code by Sergey Kiselev
; nasm -f bin -o testram.bin -l testram.lst testram.asm
;
; Writes 8 bit ascending byte to addresses 00000 - 7FFFF (first 512K)
; Then reads the bytes in this range until reset
;=========================================================================

cpu	8086

; This macro includes the setloc macro to pad code with FF to a given address
%include "macro.inc"

%define	START		8000h		; BIOS starts at offset 8000h Physical Address is F8000 [F000:8000]
    

org	START		; Use only upper 32 KiB of ROM

test_start:
    ; Set up the segment registers
    mov ax, 0
    mov bx, 0
    mov ds, ax
    xor di, di

fill_memory:
    ; Write the incrementing bl to memory
    mov [ds:di], bl

    ; Increment bl and the index register
    inc bl
    inc di
    ; If di is 0, increase the data segment register to next 64K block
    jnz fill_memory
    ; Next 64K byte segment, can't add 1000 to ds directly, use ax
    mov ax,ds
    add ax,0x1000
    mov ds,ax

    ; Repeat until all of memory is filled when DS=8000 (done 0000 - 7000)
    cmp ax,0x8000
    jne fill_memory

    ; Now read back values until reset
read:
    mov ax, 0
    mov bx, 0
    mov ds, ax
    xor di, di

read_memory:
    ; Read the value from memory, we don't do anything else with it currently
    mov bl, [ds:di]

    ; Increment the index register
    inc di
    ; If di is 0, increase the data segment register to next 64K block
    jnz read_memory
    
    ; Next 64K byte segment, can't add 1000 to ds directly, use ax
    mov ax,ds
    add ax, 0x1000
    mov ds,ax

    ; Repeat until all of RAM is read
    cmp ax, 8000
    jne read_memory

    ; End of program, repeat for ever!
    jmp read

;=========================================================================
; start - at power up or reset execution starts here (F000:FFF0)
;-------------------------------------------------------------------------
        setloc	0FFF0h			; Power-On Entry Point, macro fills space from last line with FF
start:
        jmp     0F000h:test_start
        setloc	0FFFFh			; Pad remainder of ROM
	      db	0ffh
