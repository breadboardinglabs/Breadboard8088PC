;=========================================================================
; floppy1.inc - BIOS floppy disk services (part 1 of 2)
;	INT 13h, function AH=00h
;	INT 13h, function AH=01h
;	INT 13h, function AH=08h
;	INT 13h, function AH=15h
;	INT 13h, function AH=16h
;	INT 13h, function AH=17h
;	INT 13h, function AH=18h
;	INT 13h, function AH=08h
;	- see floppy2.inc for other INT 13h functions
;-------------------------------------------------------------------------
;
; Compiles with NASM 2.13.02, might work with other versions
;
; Copyright (C) 2010 - 2023 Sergey Kiselev.
; Provided for hobbyist use on the Xi 8088 and Micro 8088 boards.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Modifications by DH for DMA controller issues with register writes and byte order
; Updated 18/07/2024 to disable DMA and use Programmed IO based on PCJr BIOS
;
;=========================================================================

;-------------------------------------------------------------------------
; FDC registers

fdc_dor_reg	equ	3F2h		; FDC digital output register (W)
fdc_status_reg	equ	3F4h		; FDC main status register (R)
fdc_data_reg	equ	3F5h		; FDC data register (R/W)
fdc_dir_reg	equ	3F7h		; FDC digital input register (W) AKA LDCR
fdc_ccr_reg	equ	3F7h		; FDC control configuration register (R) AKA LDCR

; FDC status register bits
fdc_stat_ready	equ	80h		; 1 = FDC is ready to send / receive
fdc_stat_dir	equ	40h		; 0 = CPU -> FDC; 1 = FDC -> CPU
fdc_stat_dma	equ	20h		; 1 = DMA and IRQ enabled
fdc_stat_busy	equ	10h		; 1 = FDC is busy
fdc_exec_mode	equ	20h		; 1 = BBPC Execution Mode when DMA disabled 0 = Results Mode 

;-------------------------------------------------------------------------
; DMAC registers

dmac_ch0_addr_reg	equ	00h	; DMAC channel 0 base addres (W)
dmac_ch0_count_reg	equ	01h	; DMAC channel 0 word count (W)
dmac_ch1_addr_reg	equ	02h	; DMAC channel 1 base addres (W)
dmac_ch1_count_reg	equ	03h	; DMAC channel 1 word count (W)
dmac_ch2_addr_reg	equ	04h	; DMAC channel 2 base addres (W)
dmac_ch2_count_reg	equ	05h	; DMAC channel 2 word count (W)
dmac_mask_reg		equ	0Ah	; DMAC single mask bit register (W)
dmac_mode_reg		equ	0Bh	; DMAC mode register (R/W)
dmac_ff_reg		equ	0Ch	; DMAC set (R) or clear (W)
					; first / last flip-flop
dmac_all_mask_reg	equ	0Fh	; DMAC single mask bit register (W)

dmapage_ch2_reg		equ	81h	; DMA page channel 2 register

;-------------------------------------------------------------------------
; Floppy disk services - error codes

fdc_e_success	equ	00h		; successful completion
fdc_e_invalid	equ	01h		; invalid function or parameter
fdc_e_address	equ	02h		; address mark not found
fdc_e_wprotect	equ	03h		; disk write-protected
fdc_e_notfound	equ	04h		; sector not found
fdc_e_changed	equ	06h		; disk changed
fdc_e_dma	equ	08h		; DMA overrun
fdc_e_boundary	equ	09h		; attempted DMA across 64K boundary
fdc_e_format	equ	0Ch		; not supported or drive type unknown
fdc_e_crc	equ	10h		; uncorrectable CRC error on read
fdc_e_failure	equ	20h		; controller failure
fdc_e_seek	equ	40h		; seek failed
fdc_e_timeout	equ	80h		; timeout / device not ready

;-------------------------------------------------------------------------
; Drive media state for fdc_media_state

fdc_m_rate_bits		equ	0C0h	; bits 7-6: data transfer rate
					; 00b = 500 Kbit/sec
					; 01b = 300 Kbit/sec
					; 10b = 250 Kbit/sec
					; 11b = 1 Mbit/sec
fdc_m_double_step	equ	20h	; bit 5:    1 = double stepping required
fdc_m_established	equ	10h	; bit 4:    1 = media/drive established
fdc_m_state_bits	equ	07h	; bits 2-0: media state bits
					; 000b = trying 360K in 360K drive
					; 001b = trying 360K in 1.2M drive
					; 010b = trying 1.2M in 1.2M drive
					; 011b = known 360K in 360K drive
					; 100b = known 360K in 1.2M drive
					; 101b = known 1.2M in 1.2M drive
					; 110b = not used
					; 111b = 720K in 720K or 1.44M in 1.44M
fdc_m_360in360		equ	93h	; 250 Kbps, established, 360K in 360K
fdc_m_720		equ	97h	; 250 Kbps, established, other drive
fdc_m_try_360in1200	equ	61h	; 300 Kbps, not established, 360K in 1.2
fdc_m_360in1200		equ	74h	; 300 Kbps, established, 360K in 1.2M
fdc_m_try_1200in1200	equ	02h	; 500 Kbps, not established, 1.2M in 1.2
fdc_m_1200in1200	equ	15h	; 500 Kbps, established, 1.2M in 1.2M
fdc_m_try_1440		equ	07h	; 500 Kbps, not established, other drive
fdc_m_1440		equ	17h	; 500 Kbps, established, other drive
fdc_m_try_2880		equ	0C7h	; 1 Mbps, not established, other drive
fdc_m_2880		equ	0D7h	; 1 Mbps, established, other drive

;-------------------------------------------------------------------------
; IRQ flag - fdc_calib_state, bit 7. Set by INT 0Eh (IRQ6) interrupt handler
;	     when IRQ6 happens to indicate completion of an I/O operation

fdc_irq_flag	equ	80h		; IRQ6 had occurred

; write flag - fdc_motor_state, bit 7. Set for write or format,
;	       unset for read or verify

fdc_write_flag	equ	80h		; write or format operation

;-------------------------------------------------------------------------
; floppy drive type definitions

type_none	equ	00h
type_360	equ	01h
type_1200	equ	02h
type_720	equ	03h
type_1440	equ	04h
type_2880	equ	06h

;=========================================================================
; int_13_fn00: Reset disk system
; Input:
;	AH = 00h
;	DL = drive number (bit 7 not set - floppy drive)
; Output:
;	CF clear if successful
;		AH = 00h - successful completion
;	CF set on error
;		AH = 20h - controller failure
;		AH = 80h - timeout / device not ready
; Note:
;	This function doesn't check drive number, but HDD BIOS (if installed)
;	normally will call this functions if bit 7 of DL is not set
;-------------------------------------------------------------------------
int_13_fn00:
	call	fdc_init
	mov	byte [fdc_last_error],ah  ; save the error code
	jmp	int_13_exit

;=========================================================================
; fdc_init - Initialize floppy disk controller
; Input:
;	none
; Output:
;	CF clear on success
;		AH = 00h - successful completion
;	CF set if error
;		AH = 20h - controller failure
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
fdc_init:
	and	byte [fdc_calib_state],0F0h ; require recalibration
	call	fdc_reset		; reset FDC
	jc	fdc_init_error
	mov	dx,fdc_status_reg	; get status register address
	in	al,dx
	test	al,80h
	jz	.try_again		; try again if FDC not ready
	test	al,40h
	jz	.fdc_ready		; FDC ready to recieve data
.try_again:
	call	fdc_reset
	mov	dx,fdc_status_reg	; get status register address
	in	al,dx
	test	al,80h
	jz	fdc_init_error
	test	al,40h
	jnz	fdc_init_error
.fdc_ready:

;	mov	al,08h			; FDC Sense Interrupt Status command, this returns C0 under DMA and causes issues with NO DMA status polling as reset does not raise IRQ!!
;	call	fdc_write		; send the command
;	jc	fdc_init_error
;	call	fdc_read		; read ST0
;	jc	fdc_init_error
;	mov	byte [fdc_ctrl_status],al  ; save ST0
;	mov	ah,al			; save AL to AH
;	call	fdc_read		; read current cylinder
;	jc	fdc_init_error
;	mov	byte [fdc_ctrl_status+1],al  ; save
;	and	ah,0C0h
;	cmp	ah,0C0h			; abnormal termination?
;	jne	fdc_init_error	; not an abnormal termination

; fall through to fdc_send_specify

;=========================================================================
; fdc_send_specify - Send specify command to FDC
; Input:
;	none
; Output:
;	CF clear on success
;		AH = 00 - successful completion
;	CF set if error
;		AH = 20h - controller failure
;	AX,CL,SI trashed
; Note:
;	Specify command parameters are obtained from table at INT 1Eh vector
;       BBPC Modified to set Byte 1 bit 0 ND=1 to disable DMA
;-------------------------------------------------------------------------
fdc_send_specify:
	push	ds
        push    bx
	xor	si,si
	mov	ds,si
                                        ; BBPC Changed to disable DMA by setting ND bit, no need to change media bytes
	lds	si,[1Eh*4]		; DS:SI -> INT 1Eh
	mov	al,3		 	; FDC Specify command
	mov	ah,byte [si]		; specify command - byte 0
	mov	bx,word [si+1]		; specify command - byte 1, do this first so we can disable DMA
        or      bl, 0x01                ; Bit 0 on Byte 1 is ND Non-DMA =1 for disable DMA
        mov     si, word bx             ; Copy bl to si (bh don't care)

	mov	cl,3			; command length 3 bytes (AX, SI - low, SI - High don't care)
        pop     bx
	pop	ds
	call	fdc_send_cmd		; send specify command to FDC
	jc	fdc_init_error
	mov	ah,fdc_e_success	; successful completion
	ret

fdc_init_error:
	mov	ah,fdc_e_failure
	stc
	ret

;=========================================================================
; int_13_fn01 - Get status of last operation
; Input:
;	AH = 01h
;	DL = drive number (bit 7 not set - floppy drive)
; Output:
;	CF clear if last operation was successful
;		AH = 00h - successful completion
;	CF set on error
;		AH - error code of the last operation
; Note:
;	This function doesn't check drive number, but HDD BIOS (if installed)
;	normally will call this functions if bit 7 of DL is not set
;-------------------------------------------------------------------------
int_13_fn01:
	mov	ah,byte [fdc_last_error]
	or	ah,ah
	jz	.no_error
	stc
.no_error:
	jmp	int_13_exit

;=========================================================================
; int_13_fn08: Get drive parameters
; Input:
;	AH = 08h
;	DL = drive number (0 - 7Fh)
; Output:
;	CF clear if successful
;		AX = 0000h
;		BH = 00h
;		BL = drive type
;		CH = maximal cylinder number - 1
;		CL = maximal sector number
;		DH = maximal head number
;		DL = number of drives
;		ES:DI -> diskette parameter table
;	CF set on error
;		AH = 01h - drive number is greater than 7Fh
; Notes:
;	- If non-existent drive number is specified, function returns zeros in
;	AX,BX,CX,DH,ES,DI and number of floppy drives in DL (0 if no floppies)
;	- If media type is not established function sets media parameters 
;	based on drive type
;-------------------------------------------------------------------------
int_13_fn08:

	cmp	dl,80h			; valid floppy drive number?
	jae	.invalid_drive		; hard drive number specified
	mov	al,byte [equipment_list]
	and	al,equip_floppies|equip_floppy2
	mov	ah,2			; assume two floppy drives
	cmp	al,equip_floppies|equip_floppy2 ; two floppy drives?
	je	.check_drive_number
	dec	ah			; assume one floppy drive
	cmp	al,equip_floppies	; one floppy drive?
	jne	.no_drives

.check_drive_number:
	mov	byte [bp+int_13_dl],ah	; pass number of drives to caller
	cmp	dl,ah			; requested drive number is larger
					; than number of disks?
	jnb	.non_existent_drive	; jump if non existent drive requested
	call	get_drive_type		; returns drive type in AL
	jc	.non_existent_drive	; return no drive if there is an error

	mov	byte [bp+int_13_dh],1	; maximal head number is 1 for floppy
	mov	byte [bp+int_13_bl],al	; pass drive type to caller
	mov	cx,cs
	mov	es,cx			; diskette parameter table segment

	cmp	al,type_360
	je	.set_360
	cmp	al,type_720
	je	.set_720
	cmp	al,type_1200
	je	.set_1200
	cmp	al,type_1440
	je	.set_1440

.set_2880:
	mov	al,fdc_m_try_2880	; try 2.88M in 2.88M drive
	lea	di,[media_2880]		; only 2.88M uses 1 Mbps rate
	mov	cx,4F24h		; 2.88M - 80 cylinders, 36 sectors
	jmp	.set_media_type

.set_360:
	mov	al,fdc_m_360in360
	lea	di,[media_360_in_360]
	mov	cx,2709h		; 360K - 40 cylinders, 9 sectors
	jmp	.set_media_type

.set_720:
	mov	al,fdc_m_720
	lea	di,[media_720]
	mov	cx,4F09h		; 720K - 80 cylinders, 9 sectors
	jmp	.set_media_type

.set_1200:
	mov	al,fdc_m_try_1200in1200
	lea	di,[media_1200]
	mov	cx,4F0Fh		; 1.2M - 80 cylinders, 15 sectors
	jmp	.set_media_type

.set_1440:
	mov	al,fdc_m_try_1440
	lea	di,[media_1440]
	mov	cx,4F12h		; 1.44M - 80 cylinders, 18 sectors

.set_media_type:
	call	get_media_state
	test	bl,fdc_m_established	; media type established?
	jnz	.set_parameters		; no need to update if established
	call	set_media_state

.set_parameters:
	xor	ax,ax			; AH = 00h - successful completion
	mov	byte [bp+int_13_al],al	; successful completion
	mov	byte [fdc_last_error],al
	mov	byte [bp+int_13_bh],al	; clear BH just in case
	mov	word [bp+int_13_cx],cx	; cylinders / sectors
	mov	word [bp+int_13_di],di	; diskette parameter table pointer
	jmp	int_13_exit

.no_drives:
	mov	byte [bp+int_13_dl],0	; zero drives

.non_existent_drive:
	xor	cx,cx
	xor	di,di
	mov	byte [bp+int_13_bl],cl	; drive type is zero
	mov	byte [bp+int_13_dh],cl	; maximal head number is zero
	mov	es,cx			; disk parameter table segment = 0000h
	jmp	.set_parameters

.invalid_drive:
	mov	ah,fdc_e_invalid
	stc
	jmp	int_13_exit

;=========================================================================
; int_13_fn15: Get disk type
; Input:
;	AH = 15h
;	DL = drive number (0 or 1)
; Output:
;	AH = type code:
;		00h - no such drive (invalid drive specified)
;		01h - floppy without change-line support
;		02h - floppy with change-line support
;-------------------------------------------------------------------------
int_13_fn15:
	mov	ah,00h			; assume no drive
	cmp	al,type_none
	je	.exit
	mov	ah,01h			; assume no change-line support
	cmp	al,type_360
	je	.exit
	cmp	al,type_720		; no change-line support on 720K drives?
	je	.exit
	mov	ah,02h			; otherwise it supports change-line

.exit:
	clc
	mov	byte [fdc_last_error],0
	jmp	int_13_exit

;=========================================================================
; int_13_fn16 - Detect disk change
; Input:
;	AH = 16h
;	DL = drive number (0 or 1)
; Output:
;	CF clear if change line inactive
;		AH = 00h - disk not changed
;	CF set if change line active or error
;		AH = 01h - invalid drive number
;		AH = 06h - disk changed or change line not supported
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
int_13_fn16:
	cmp	al,type_360
	je	.no_change_line		; report disk changed for 360K drives
	cmp	al,type_720
	je	.no_change_line		; report disk changed for 720K drives

	call	fdc_motor_on		; turn motor on
	xor	ah,ah			; assume disk not changed
	mov	dx,fdc_dir_reg		; get digital input register address
	in	al,dx			; read disk change line
  ; WD37C65C in DIP form does not support disk change, fake no change!
  mov al, 0x7f
	shl	al,1			; bit 7 to CF
	jnc	.exit
	mov	ah,fdc_e_changed
	stc

.exit:
	mov	byte [fdc_last_error],ah  ; save the error code
	pushf
	push	ds
	xor	si,si
	mov	ds,si
	lds	si,[1Eh*4]		; DS:SI -> INT 1Eh
	mov	cl,byte [si+2]
	pop	ds
	mov	byte [fdc_motor_tout],cl ; ticks before turning off the motor
	popf
	jmp	int_13_exit

.no_change_line:
	mov	ah,fdc_e_changed	; can't detect - report disk changed
	stc
	mov	byte [fdc_last_error],ah
	jmp	int_13_exit

.invalid_drive:
	mov	ah,fdc_e_invalid
	stc
	jmp	int_13_exit

;=========================================================================
; int_13_fn17 - Set disk type for format
; Input:
;	AH = 17h
;	AL = format type
;		01h - 320K / 360K disk in 360K drive
;		02h - 320K / 360K disk in 1.2M drive
;		03h - 1.2M disk in 1.2M drive
;		04h - 720K disk in 720K or 1.2M drive
;	DL = drive number (0 or 1)
; Output:
;	CF clear if successful
;		AH = 00h - successful completion
;	CF set on error
;		AH = 01h - invalid drive number
;		AH = 06h - disk changed
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
int_13_fn17:
	mov	cl,byte [bp+int_13_al]	; get original AL value to CL
	cmp	cl,0			; validate parameters
	je	.invalid_parameters
	cmp	cl,4
	ja	.invalid_parameters
	cmp	cl,1			; 360K disk in 360K drive?
	jne	.not_360in360
	mov	ah,0			; no error
	mov	al,fdc_m_360in360	; 360K disk in 360K drive
	jmp	.exit_check_error

.not_360in360:
	call	fdc_motor_on
	call	fdc_disk_change		; check if disk has been changed
	cmp	ah,fdc_e_changed
	jbe	.set_type		; disk changed (no errors other than
					; "disk changed" from fdc_disk_change)
	cmp	ah,fdc_e_timeout
	jne	.set_type		; floppy disk is installed
	call	get_media_state		; BL = media state
	cmp	bl,fdc_m_720		; 250 Kbps and not 5.25?
	je	.exit_check_error
	mov	al,fdc_m_try_360in1200	; 300 Kpbs, try 360 in 1.2M
	jmp	.exit_check_error

.set_type:
	cmp	cl,4			; 720K in 720K?
	jne	.check_360in1200	; jump if not 720K in 720K
	mov	al,fdc_m_720
	jmp	.exit_check_error

.check_360in1200:
	cmp	cl,2			; 360K in 1.2M?
	jne	.set_1200in1200		; jump if not 360K in 1.2M
	mov	al,fdc_m_360in1200
	jmp	.exit_check_error

.set_1200in1200:
	mov	al,fdc_m_1200in1200	; 1.2M in 1.2M

.exit_check_error:
	call	set_media_state		; set media state to value in AL
	mov	byte [fdc_last_error],ah
	or	ah,ah
	jz	.exit			; jump if no error
	stc				; indicate error

.exit:
	jmp	int_13_upd_exit

.invalid_parameters:
	mov	ah,fdc_e_invalid
	jmp	.exit

;=========================================================================
; int_13_fn18 - Set media type for format
; Input:
;	AH = 18h
;	DL = drive number (0 or 1)
;	CH = number of cylinders - 1
;	CL = sectors per track
; Output:
;	CF = clear if successful
;		AH = 00h - requested format is supported
;		ES:DI -> diskette parameter table
;	CF = set on error
;		AH = 01h - invalid drive number specified
;		AH = 0Ch - format is not supported or drive type is unknown
;-------------------------------------------------------------------------
int_13_fn18:
	cmp	al,type_360		; 360K drive?
	jne	.try_drive_1200
	cmp	cx,2709h	 	; 40 tracks 9 sectors?
	jnz	.unsupported_format
	mov	al,fdc_m_360in360	; 360K in 360K established, 250 Kbps
	lea	di,[media_360_in_360]
	jmp	.set_media

.try_drive_1200:
	cmp	al,type_1200		; 1.2M drive?
	jne	.try_drive_2880
	cmp	cx,4F0Fh		; 80 tracks 15 sectors?
	jne	.try_media_360_in_1200
	mov	al,fdc_m_1200in1200	; 1.2M in 1.2M established, 500Kbps
	lea	di,[media_1200]		; 1.2M
	jmp	.set_media

.try_media_360_in_1200:
	cmp	cx,2709h		; 80 tracks 9 sectors?
	jne	.unsupported_format
	mov	al,fdc_m_360in1200	; 360K in 1.2M established, 300Kbps
	lea	di,[media_360_in_1200]	; 360K in 1.2M
	jmp	.set_media

.try_drive_2880:
	cmp	al,type_2880		; 2.88M drive?
	jne	.try_drive_1440
	cmp	cx,4F24h		; 80 tracks 36 sectors?
	jne	.try_media_1440
	mov	al,fdc_m_2880		; indicate 2.88M
	lea	di,[media_2880]
	jmp	.set_media

.try_drive_1440:
	cmp	al,type_1440		; 1.44M drive?
	jne	.try_drive_720

.try_media_1440:
	cmp	cx,4F12h		; 80 tracks 18 sectors?
	jne	.try_media_720
	mov	al,fdc_m_1440		; indicate 1.44M
	lea	di,[media_1440]
	jmp	.set_media

.try_drive_720:
	cmp	al,type_720		; 720K drive?
	jne	.unsupported_format	; should never happen...

.try_media_720:	
	cmp	cx,4F09h		; 80 tracks 9 sectors?
	jne	.unsupported_format
	mov	al,fdc_m_720		; other established, 250Kbps
	lea	di,[media_720]		; 720K

.set_media:
	call	set_media_state		; set drive media state (in AL)
	call	fdc_set_rate		; transfer rate in AL (bits 7-6)
	mov	word [bp+int_13_di],di	; return parameters table - offset
	mov	cx,cs
	mov	es,cx			; return parameters table - segment
	mov	byte [fdc_last_error],0	; no errors
	xor	ah,ah
	jmp	int_13_upd_exit

.unsupported_format:
	mov	ah,fdc_e_format		; not supported or drive type unknown
	stc
	jmp	int_13_upd_exit

;=========================================================================
; fdc_recalibrate - Recalibrate disk drive, seek to cylinder 0
; Input:
;	[BP+int_13_dl] = drive number (0 or 1)
; Output:
;	CF clear if successful
;		AH = 00h - successful completion
;	CF set on error
;		AH = 20h - controller failure
;		AH = 20h - timeout
;	AH trashed
;-------------------------------------------------------------------------
fdc_recalibrate:
	push	si
	push	cx
	push	dx
	mov	al,07h			; FDC Recalibrate command
	mov	ah,byte [bp+int_13_dl]	; drive number - 2nd byte of command
	mov	cl,2			; 2 bytes command
	and	byte [fdc_calib_state],~fdc_irq_flag ; clear IRQ flag
	call	fdc_send_cmd
	jc	recal_end		; failure
	;call	fdc_wait_irq		; wait for IRQ
    call    chk_stat_2              ; Wait for Seek End in ST0 bit 5, already has Sense code in it
	jc	recal_end		; timeout waiting for interrupt
	;mov	al,08h			; FDC Sense Interrupt Status command
	;mov	cl,1			; 1 byte command
	;call	fdc_send_cmd
	;jc	recal_end		; failure
	;mov	cx,2			; 2 bytes result
	;call	fdc_get_result		; store result
	;jc	recal_end		; failure
	;mov	bx,fdc_ctrl_status
	;mov	ah,fdc_e_seek
	;mov	dl,byte [bx]			; ST0
	;and	dl,60h
	;cmp	dl,60h			; abnormal termination + seek end
	;stc				; indicate error
	;je	recal_end		; failure

	mov	ch,0
	call	set_cylinder		; set current cylinder to 0
	call	set_drive_calibrated	; set drive calibrated in data area
	mov	cx,43h
	call	delay_15us		; 1 ms delay
	xor	ah,ah			; AH = 0, CF = 0 - no error

recal_end:
	mov	byte [fdc_last_error],ah
	pop	dx
	pop	cx
	pop	si
	ret

;=========================================================================
; fdc_seek - Move floppy drive head to the specified cylinder
; Input:
;	DL = drive number
;	DH = head number
;	CH = cylinder
; Output:
;	CF clear if successful
;		AH = 00h - successful completion
;	CF set on error
;		AH = 20h - controller failure
;		AH = 40h - seek failed
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
fdc_seek:
	push	bx
	push	cx
	push	dx
	call	check_drive_calibrated
	jc	.skip_recalibrate	; jump if drive is already calibrated
	call	fdc_recalibrate
	jnc	.skip_recalibrate	; jump if calibration successful
	call	fdc_recalibrate		; try recalibrating again
	jc	.exit			; no luck...

.skip_recalibrate:
	call	get_media_state		; BL = media state
	test	bl,fdc_m_double_step 	; check double stepping bit
	jz	.no_double_stepping
	shl	ch,1			; CH = CH * 2 (double cylinder number)

.no_double_stepping:
	call	check_cylinder		; already at the right cylinder?
	jnz	.do_seek		; jump if seek is required
	cmp	byte [fdc_last_error],fdc_e_seek
	je	.do_seek		; jump if it was a seek error
	xor	ah,ah			; AH = 0, CF = 0 - success
	jmp	.exit

.do_seek:
	mov	al, 0xD5		; Seek DL/CH/DH Will output Track/Cylinder to Port 80 with D5
	out	post_reg,al
	mov	al,ch			; cylinder
	out	post_reg,al
	mov	si,ax			; SI - low = cylinder number
	mov	al,0Fh			; FDC Seek command
	mov	ah,dh			; head
	shl	ah,1			; move head to bit 2
	shl	ah,1
	or	ah,byte [bp+int_13_dl]	; seek - byte 1 (head / drive)
	mov	cl,3			; 3 bytes command
	and	byte [fdc_calib_state],~fdc_irq_flag ; clear IRQ flag
	call	fdc_send_cmd
	jc	.set_result		; seek error
;	call	fdc_wait_irq		; wait for IRQ
    call    chk_stat_2              ; Wait for Seek End in ST0 bit 5
	jc	.set_result		; timeout waiting for interrupt

	mov	al,08h			; FDC Sense Interrupt Status command
	mov	cl,1			; 1 byte command
	call	fdc_send_cmd
	jc	.set_result		; failure
	mov	cl,2
	push	bx
	call	fdc_get_result		; read result bytes
	pop	bx
	jc	.set_result		; error
	mov	ah,fdc_e_seek
	mov	al,byte [fdc_ctrl_status] ; ST0
	and	al,60h
	cmp	al,60h			; abnormal termination + seek end
	stc
	je	.set_result		; seek error
	call	set_cylinder		; save new cylinder number

	push	ds
	xor	si,si
	mov	ds,si
	lds	si,[1Eh*4]		; DS:SI -> INT 1Eh
	mov	al,byte [si+9]		; AL = head settle time in ms
	pop	ds

	or	al,al			; head settle time is zero?!
	jz	.get_settle_time

	mov	cl,67			; 15 us * 67 ~= 1 ms
	mul	cl			; AX = delay in 15 us intervals
	mov	cx,ax

.wait:
	call	delay_15us

.wait_end:
	xor	ah,ah			; AH = 0, CF = 0 - no error

.set_result:
	mov	byte [fdc_last_error],ah

.exit:
	pop	dx
	pop	cx
	pop	bx
	ret

.get_settle_time:
	test	byte [fdc_motor_state],fdc_write_flag ; test the write bit
	jz	.wait_end		; jump if read / verify - no wait

	call	get_media_state	
	mov	ah,bl			; AH = media state
	and	ah,fdc_m_state_bits	; leave only drive media state bits
	mov	cx,1325			; 20 ms delay for 360K drives
	jz	.wait			; jump if 360K, media not established
	cmp	ah,fdc_m_360in360 & fdc_m_state_bits
	je	.wait			; jump if 360K, media established
	mov	cx,995			; 15 ms delay for other drives
	jmp	.wait

;=========================================================================
; fdc_motor_on - Turn motor on (if it is not on yet)
; Input:
;	DL = drive number (0 or 1) -- FIXME: Does it receive DL?
; Output:
;	none
;-------------------------------------------------------------------------
fdc_motor_on:
	push	ax
	push	cx
	push	dx
	cli				; entering critical section
	mov	byte [fdc_motor_tout],0FFh  ; set timeout to maximum
	mov	dl,byte [bp+int_13_dl]	; DL = drive number
	mov	cl,dl			; CL = drive number
	inc	cl			; CL = drive number + 1
	mov	al,byte [fdc_motor_state] ; AL = motor state byte
	mov	dh,al			; keep a copy in motor state in DH
	and	al,0CFh			; clear drive select bits (5-4)

	shl	dl,1			; shift drive number to bits (5-4)
	shl	dl,1
	shl	dl,1
	shl	dl,1
	or	al,dl			; select drive
	shr	dh,cl			; set CF if motor for is on
	jc	.already_on
	mov	dl,1
	dec	cl
	shl	dl,cl
	or	al,dl			; indicate that motor is on
	mov	byte [fdc_motor_state],al ; update motor state in data area
	sti
	or	al,40h			; no reset,  note set_fdc_dor rotates right 4 bits 40h=04h!
	call	set_fdc_dor
	mov	ax,90FDh
	int	15h			; call OS hook
	jc	.exit
	push	ds
	push	si
	xor	si,si
	mov	ds,si
	lds	si,[1Eh*4]		; DS:SI -> INT 1Eh
	mov	al,byte [si+0Ah]	; AL = motor start time in 1/8 second
	pop	si
	pop	ds

	test	byte [fdc_motor_state],fdc_write_flag ; test the write bit
	jz	.read_verify		; jump if not write operation
	cmp	al,8
	jae	.wait_loop		; jump if at least 1 second start time
	mov	al,8			; wait at least 1 second for write
	jmp	.wait_loop

.read_verify:
	cmp	al,5
	jae	.wait_loop		; jump if at least 625 ms start time
	mov	al,5			; wait at least 625 ms for read / verify

.wait_loop:
	mov	cx,8287			; 8287 * 15.09us ~= 125ms
	call	delay_15us		; wait 125 ms
	dec	al
	jnz	.wait_loop		; repeat until AL = 0

.exit:
	pop	dx
	pop	cx
	pop	ax
	ret

.already_on:
	mov	byte [fdc_motor_state],al ; update motor state (AL) in data area
	sti
	or	al,40h			; no reset,  note set_fdc_dor rotates right 4 bits 40h=04h!
	call	set_fdc_dor
	jmp	.exit

;=========================================================================
; fdc_end_io - Set motor timeout, return next sector to be transferred
; Input:
;	CH = cylinder
;	DH = head
; Output:
;	BL = next sector to be transferred
;-------------------------------------------------------------------------	
fdc_end_io:
	push	ax
	push	ds
	xor	bx,bx
	mov	ds,bx
	lds	bx,[1Eh*4]		; DS:BX -> INT 1Eh
	mov	ah,byte [bx+2]		; motor timeout (ticks)
	mov	al,byte [bx+4]		; sectors per track
	inc	al
	pop	ds
	mov	bx,fdc_ctrl_status
	cmp	ch,byte [bx+3]		; same result cylinder?
	jne	.exit
	cmp	dh,byte [bx+4]		; same result head?
	jne	.exit
	mov	al,byte [bx+5]		; result sector number
.exit:
	mov	byte [fdc_motor_tout],ah  ; motor timeout
	mov	bl,al			; next sector to be transferred
	pop	ax
	ret

;========================================================================
; fdc_disk_change - Read disk change line, reset it if active
; Input:
;	AL = drive type
; Output:
;	CF clear if disk not changed
;		AH = 00h - disk not changed
;	CF set if disk changed or on error
;		AH = 06h - disk changed
;		AH = error code
;		AL - trashed
; Note:
;	Motor needs to be turned on before calling this function
;------------------------------------------------------------------------
fdc_disk_change:
	push	bx
	push	cx
	mov	ah,0
	cmp	al,type_360
	je	.exit			; jump if 360K drive (no change line)
	cmp	al,type_720
	je	.exit			; jump if 720K drive (no change line)

	call	get_media_state
	mov	al,bl			; AL = drive media state
	and	al,fdc_m_state_bits
	jz	.exit			; jump if 360K drive (no change line)
	cmp	al,3
	je	.exit			; jump if 360K drive (no change line)
	mov	dx,fdc_dir_reg		; get digital input register address
	in	al,dx			; read disk change line
  ; WD37C65C in DIP form does not support disk change, fake no change!
  mov al, 0x7f
	shl	al,1
	jnc	.exit			; no disk change
	mov	al,bl			; AL = drive media state
	and	al,~fdc_m_established	; media not detected
	call	set_media_state
	call	fdc_init		; full initialization
	jc	.exit
	mov	dx,word [bp+int_13_dx]	; restore DX
	mov	ch,1
	call	fdc_seek		; seek to cylinder 1
	jc	.exit
	mov	ch,0
	call	fdc_seek		; seek to cylinder 0
	jc	.exit
	mov	ah,fdc_e_changed
	mov	dx,fdc_dir_reg		; get digital input register address
	in	al,dx			; read disk change line
  ; WD37C65C in DIP form does not support disk change, fake no change!
  mov al, 0x7f
	shl	al,1
	jnc	.changed_or_error	; jump if disk change line was reset
	mov	ah,fdc_e_timeout	; failed: no floppy

.changed_or_error:
	stc

.exit:
	mov	dx,word [bp+int_13_dx]	; restore dx
	pop	cx
	pop	bx
	ret

;=========================================================================
; fdc_configure_dma - Configure DMA controller for FDC operation (channel 2)
; Input:
;	AL = DMA mode byte
;		42h - verify (single mode, addr increment, verify, channel 2)
;		46h - read   (single mode, addr increment, write, channel 2) These are from RAM perspective, read disk via IO, write RAM via DMA
;		4Ah - write  (single mode, addr increment, read, channel 2) These are from RAM perspective, write disk via IO, read RAM via DMA
;	CX = byte count (minus 1)
;	ES:BX -> buffer address for DMA operation (BX is from [BP+int_13_bx])
; Output:
;	CF clear on success
;	CF set if error
;		AH = 08h - DMA overrun
;	AX,BX trashed
; Note:
;	Translates ES:BX to DMA page and base address and configures DMAC
;-------------------------------------------------------------------------
; fdc_configure_dma:
	; push	dx
	; mov	bx,word [bp+int_13_bx]	; get BX from the stack
	; mov	dx,es			; user's buffer segment
	; rol	dx,1
	; rol	dx,1
	; rol	dx,1
	; rol	dx,1
	; mov	ah,dl			; calculate DMA page number
	; and	ah,0Fh			; AH = page number: bits 19 - 16 of ES
	; and	dl,0F0h			; DX = DMA offset: bits 15 - 0 of ES
	; add	dx,bx			; add user's buffer offset
	; adc	ah,0			; increment page number on overflow

	; mov	bx,dx			; check if crossing DMA page boundary:
	; add	bx,cx			;   add DMA buffer address to byte count
	; jc	.dma_boundary		; jump if crossing DMA boundary (64 KiB)

	; cli
        ; push ax

	; mov	al,6
	; out	dmac_mask_reg,al	; disable DMA channel 2 via mask
        ; pop ax
	; jmp	$+2
	; jmp	$+2
	; out	dmac_mode_reg,al	; send DMA mode byte
	; jmp	$+2
	; jmp	$+2
        ; ; The Intersil chip still seems to reverse the MSB and LSB of the DMA address despite everything attempted
        ; ; If we read from the same port then it will reverse the order MSB first then LSB, see if this works
        ; ; 1 Put read in as last commands to see if this leaves the DMAC in the correct order!
        ; ; 2 Just change Flip Flip write to Read
        ; ; 3 Change Flip Flip write to Read AND reverse MSB and LSB writes
        ; ; Following DMATEST4 nothing much seems to change the behavior with curent physical design 
        ; ; BUT re-reading registers and repeating appears to work eventually!

; .retry_dma:                             ; When we read registers after setting and not right will return here!

	; out	dmac_ff_reg,al		; clear first/last flip-flop
	; jmp	$+2
	; jmp	$+2
	; mov	al,cl
	; out	dmac_ch2_count_reg,al	; send word count - low byte
	; jmp	$+2
	; jmp	$+2
	; mov	al,ch
	; out	dmac_ch2_count_reg,al	; send word count - high byte
	; jmp	$+2
	; jmp	$+2

        ; ; Read count back. If different jump to .retry_dma
        
	; in	al,dmac_ch2_count_reg	; read count - low byte
        ; cmp     al, cl
        ; jne      .retry_dma
	; jmp	$+2
	; jmp	$+2
	; in	al,dmac_ch2_count_reg	; read count - high byte
        ; cmp     al, ch
        ; jne      .retry_dma

; .setdmaaddress:
	; out	dmac_ff_reg,al		; clear first/last flip-flop
	; jmp	$+2
	; jmp	$+2
	; mov	al,dl
	; out	dmac_ch2_addr_reg,al	; send base address - low byte
	; jmp	$+2
	; jmp	$+2
	; mov	al,dh
	; out	dmac_ch2_addr_reg,al	; send base address - high byte
	; jmp	$+2
	; jmp	$+2

        ; ; Read address back. If different jump to .retry_dma
        
	; in	al,dmac_ch2_addr_reg	; read base address - low byte
        ; cmp     al, dl
        ; jne      .retry_dma
	; jmp	$+2
	; jmp	$+2
	; in	al,dmac_ch2_addr_reg	; read base address - high byte
        ; cmp     al, dh
        ; jne      .retry_dma

	; mov	al,ah
	; out	dmapage_ch2_reg,al	; channel 2 page register
	; jmp	$+2
	; jmp	$+2
	; mov	al,2
	; out	dmac_mask_reg,al	; enable DMA channel 2

	; jmp	$+2
	; jmp	$+2
        ; ; 1 Put read in as last commands to see if this leaves the DMAC in the correct order!
; ;        in 	al, dmac_ff_reg         ; This sets the address register order to MSB then LSB when writing and reading - perhaps bug in chip when generating DMA addresses
; ;       xor     al,al
; ; 	out	dmac_ff_reg,al		; clear first/last flip-flop - leave in cleared state as seems to get MSB and LSB confused
; ;	jmp	$+2
; ;	jmp	$+2
  
	; sti
	; clc				; CF = 0 - no error 

; .exit:
	; pop	dx
	; ret

; .dma_boundary:				; Note: CF is already set
	; mov	ah,fdc_e_boundary
	; jmp	.exit

;=========================================================================
; fdc_send_cmd - Send a command to FDC
; Input:
;	AL = 1st byte of the command
;	AH = 2nd byte of the command
;	SI = 3rd and 4th bytes of the command
;	DI = 5th and 6th bytes of the command
;	BL = 7th byte of the command
;	BH = 8th byte of the command
;	CH = 9th byte of the command
;	CL = command length (number of bytes)
; Output:
;	CF clear if successful
;		AH = 00h - successful completion
;	CF set on error
;		AH = 20h - controller failure
;		AH = 80h - timeout / device not ready
;	AX trashed
;-------------------------------------------------------------------------
fdc_send_cmd:
	call	fdc_write		; send AL (1st byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,ah
	call	fdc_write		; send AH (2nd byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	ax,si
	call	fdc_write		; send SI / low byte (3th byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,ah
	call	fdc_write		; send SI / high byte (4th byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	ax,di
	call	fdc_write		; send DI / low byte (5th byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,ah
	call	fdc_write		; send DI / high byte (6th byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,bl
	call	fdc_write		; send BL (7rd byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,bh
	call	fdc_write		; send BH (8th byte)
	dec	cl
	jbe	.send_cmd_exit		; jump if done (ZF=1) or on error (CF=1)
	mov	al,ch
	call	fdc_write		; send CH (9th byte)

.send_cmd_exit:
	ret

;=========================================================================
; fdc_write - Send byte to FDC
; Input:
;	AL = byte to send
; Output:
;	CF clear if successful
;	CF set if timeout
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
fdc_write:
	push	cx
	push	dx
	push	ax
	mov	dx,fdc_status_reg	; get status register address
	xor	cx,cx			; busy wait counter

.wait_ready:
	in	al,dx			; read fdc_status_reg
	test	al,fdc_stat_ready	; exit loop if FDC is ready
	jnz	 .fdc_ready
	loop	.wait_ready         ; retry until timeout
	pop	ax
	mov	ah,fdc_e_timeout	; timeout
	stc				; indicate error
	jmp	.exit

.fdc_ready:
	test	al,fdc_stat_dir|fdc_stat_dma ; check that DIR and DMA flags are 0
	jnz	.failure		; FDC failure if they are set
	pop	ax
	inc	dx			; DX = fdc_data_reg = fdc_status_reg + 1
	out	dx,al			; write byte
	clc				; no error

.exit:
	pop	dx
	pop	cx
	ret

.failure:
	pop	ax
	mov	ah,fdc_e_failure	; FDC failure
	stc
	jmp	.exit

;=========================================================================
; fdc_get_result - Read FDC result
; Input:
;	none
; Output:
;	CF clear if successful
;		AH = 00h - operation successful
;		Result is returned in fdc_ctrl_status BIOS area
;	CF set on error
;		AH = 20h - controller failure
;		AH = 80h - timeout / device not ready
;	AL,BX - trashed
;-------------------------------------------------------------------------
fdc_get_result:
	push	cx
	push	dx
	mov	bx,fdc_ctrl_status
	mov	dx,fdc_status_reg	; get status register address

.next_byte:
        xor     cx,cx			; busy wait counter

.wait_ready:
	in	al,dx			; read fdc_status_reg
	test	al,fdc_stat_ready	; exit loop if FDC is ready
	jnz	.fdc_ready
	loop	.wait_ready
	mov	ah,fdc_e_timeout	; timeout
	stc				; indicate error
	jmp	.exit

.fdc_ready:
	and	al,fdc_stat_dir|fdc_stat_dma|fdc_stat_busy
;	test	al,~fdc_stat_busy	; controller finished sending result?
	test	al, fdc_stat_busy	; controller finished sending result? This was wrong when stat_busy = 0 then done
	; stat_busy will be 0 when all results have been returned
	je	.done
	cmp	al,fdc_stat_dir|fdc_stat_busy
	jne	.failure		; invalid FDC status, indicate FDC failure
	cmp	bx,fdc_ctrl_status+7	; already read 7 bytes? 
	jnb	.failure		; controller error if so
	inc	dx			; DX = fdc_data_reg = fdc_status_reg + 1
	in	al,dx
	mov	byte [bx],al
	inc	bx
	dec	dx			; DX = fdc_status_reg = fdc_data_reg - 1
	jmp	.next_byte

.done:
	xor	ah,ah			; operation successful

.exit:
	pop	dx
	pop	cx
	ret

.failure:
	mov	ah,fdc_e_failure
	stc
	jmp	.exit

;=========================================================================
; fdc_read - Read byte from FDC
; Input:
;	none
; Output:
;	CF clear if successful
;		AL = byte read from FDC
;		AH - unchanged
;	CF set on error
;		AH = 20h - controller failure
;		AH = 80h - timeout / device not ready
;-------------------------------------------------------------------------
fdc_read:
	push	cx
	push	dx
	mov	dx,fdc_status_reg	; get status register address
        xor     cx,cx			; busy wait counter

.wait_ready:
	in	al,dx			; read fdc_status_reg
	test	al,fdc_stat_ready	; exit loop if FDC is ready
	jnz	.fdc_ready
	loop	.wait_ready
	mov	ah,fdc_e_timeout	; timeout
	stc				; indicate error
	jmp	.exit

.fdc_ready:
	in	al,dx
	test	al,fdc_stat_dir		; FDC is ready to send a byte? 
	jz	.failure		; jump if not ready
;	jmp	short $+2		; I/O delay
;	jmp	short $+2
	inc	dx			; DX = fdc_data_reg = fdc_status_reg + 1
	in	al,dx			; read the byte
	clc				; no error

.exit:
	pop	dx
	pop	cx
	ret

.failure:
	mov	ah,fdc_e_failure
	stc
	jmp	.exit

;=========================================================================
; fdc_get_error - Translate FDC status to BIOS error number
; Input:
;	DS:[fdc_ctrl_status] - FDC status bytes
; Output:
;	AH = error code
;-------------------------------------------------------------------------
fdc_get_error:
	mov	bx,word [fdc_ctrl_status]
	test	bl,0C0h			; BL = ST1 (Actually ST0 D7 & D6 = 0 Normal Termination)
	mov	ah,fdc_e_success
	jz	.exit			; jump if successful completion
	test	bl,40h			; abnormal termination? Equipment Check = 1
	mov	ah,fdc_e_failure
	jz	.exit
	test	bh,1			; address mark not found?
	mov	ah,fdc_e_address
	jnz	.exit
	test	bh,2			; BH=ST1 disk write protected?
	mov	ah,fdc_e_wprotect
	jnz	.exit
	test	bh,4			; sector not found?
	mov	ah,fdc_e_notfound
	jnz	.exit
	test	bh,10H			; DMA overrun?
	mov	ah,fdc_e_dma
	jnz	.exit
	test	bh,20H			; CRC error?
	mov	ah,fdc_e_crc
	jnz	.exit
	test	bh,80h			; access after last sector? End of Track
	mov	ah,fdc_e_notfound
	jnz	.exit
	mov	ah,fdc_e_failure	; return FDC failure for other errors

.exit:
	ret

;=========================================================================
; fdc_reset - Reset FDC
; Input:
;	none
; Output:
;	Resets FDC flags in BIOS area
;	AX,CX,DX - trashed 
;-------------------------------------------------------------------------
fdc_reset:
	cli
	and	byte [fdc_calib_state],~fdc_irq_flag ; clear IRQ flag
	and	byte [fdc_motor_state],~fdc_write_flag ; clear the write bit
	mov	al,byte [fdc_motor_state] ; AL = motor state byte
	and	al,0BFh			; clear bit 6 - reset,  note set_fdc_dor rotates right 4 bits 40h=04h!
	push	ax			; set_fdc_dor trashes AX, save it
	call	set_fdc_dor
	pop	ax
	mov	cx,3
	call	delay_15us		; 30-45 us delay
	or	al,40h			; set bit 6 - no reset,  note set_fdc_dor rotates right 4 bits 40h=04h!
	call	set_fdc_dor             ; Note set_fdc_dor now clears DMAIRQ bit
	sti
;	call	fdc_wait_irq		; wait for IRQ
    call    chk_stat_results    ; Check for busy flag to be clear or timeout, getting D0 which show RQM, DIO and CB high

	jc	.exit
	and	byte [fdc_calib_state],~fdc_irq_flag ; clear IRQ flag
	xor	ah,ah			; no errors
.exit:
	ret

;=========================================================================
; fdc_detect_media - Detect media type, update it in fdc_media_state[drive]
; Input:
;	[BP+int_13_dl] = drive number
; Output:
;	CF clear if successful
;		AH = 00h
;	CF set on error
;		AH = 20h - invalid drive type
;-------------------------------------------------------------------------
fdc_detect_media:
	push	dx
	push	cx

	mov	ah,fdc_e_failure	; in case the get_drive_type fails
	mov	dl,byte [bp+int_13_dl]	; restore DL in case it was trashed
	call	get_drive_type		; get drive type to AL
	jc	.exit			; drive doesn't exist or invalid type

	cmp	al,type_720
	je	.set_720
	cmp	al,type_1200
	je	.detect_1200
	cmp	al,type_1440
	je	.detect_1440
	cmp	al,type_2880
	je	.detect_2880

	mov	al,fdc_m_360in360	; set 360K disk in 360K drive
	jmp	.set_rate

.set_720:
	mov	al,fdc_m_720

.set_rate:
	push	ax
	call	fdc_set_rate		; transfer rate in AL
	pop	ax
	jmp	.exit_set_media

.detect_1200:
	mov	al,0			; try 500 Kbps
	call	fdc_read_id
	mov	al,fdc_m_1200in1200
	jnc	.exit_set_media		; jump if successful
	mov	al,40h			; try 300 Kbps
	call	fdc_read_id
	mov	al,fdc_m_360in1200
	jnc	.exit_set_media		; jump if successful
	mov	al,fdc_m_try_1200in1200
	jmp	.exit_set_media

.detect_1440:
	mov	al, 0xD3		; Detect 1.44MB Media Type
	out	post_reg,al

	mov	al,0			; try 500 Kbps
	call	fdc_read_id
	mov	al,fdc_m_1440
	jnc	.exit_set_media		; jump if successful
	mov	al,80h			; try 250 Kbps
	call	fdc_read_id
	mov	al,fdc_m_720
	jnc	.exit_set_media		; jump if successful
	mov	al,fdc_m_try_1440
	jmp	.exit_set_media

.detect_2880:
	mov	al,0C0h			; try 1 Mbps
	call	fdc_read_id
	mov	al,fdc_m_2880
	jnc	.exit_set_media		; jump if successful
	mov	al,0			; try 500 Kbps
	call	fdc_read_id
	mov	al,fdc_m_1440
	jnc	.exit_set_media		; jump if successful
	mov	al,80h			; try 250 Kbps
	call	fdc_read_id
	mov	al,fdc_m_720
	jnc	.exit_set_media		; jump_if successful
	mov	al,fdc_m_try_2880

.exit_set_media:
 	out	post_reg,al       ; Log Media State to POST should be 0x17 for 1.44MB HD
	call	set_media_state		; set media state
	xor	ah,ah			; AH = 0, CF = 0 - no error
.exit:
	mov	byte [fdc_last_error],ah
	pop	cx
	pop	dx
	ret

;=========================================================================
; fdc_read_id - Read ID
; Input:
;	[BP+int_13_dl] = drive number
; Output:
;	CF clear if successful
;		AH = 0 - successful completion
;		AL = ID (bits 7-6)
;	CF set on error
;		AH = error code
;	AX,CX - trashed
;-------------------------------------------------------------------------
fdc_read_id:
	push	bx
	mov	byte [fdc_motor_tout],0FFh ; set timeout to maximum
	call	fdc_set_rate		; transfer rate in AL
	mov	dl,byte [bp+int_13_dl]
	call	fdc_recalibrate		; recalibrate
	jnc	.do_read_id
	call	fdc_recalibrate		; second attempt
	jc	.error

.do_read_id:
	mov	cx,3			; 3 attempts

.read_id_loop:
	push	cx
	mov	al,4Ah			; FDC Read ID command
; FIXME - does drive number in DL gets to here?
;	mov	ah,dl			; read id - byte 1 (head = 0 / drive)
	mov	ah,byte [bp+int_13_dl]	; read id - byte 1 (head = 0 / drive)
	mov	cl,2			; 2 byte commands
	and	byte [fdc_calib_state],~fdc_irq_flag ; clear IRQ6 flag
	call	fdc_send_cmd
	jc	.error_cmd
;	call	fdc_wait_irq		; wait for IRQ6
    call    chk_stat_results    ; Wait for Sense Interrupt Status to return in results phases

	jc	.error_cmd		; timeout waiting for interrupt
	mov	cl,7
	call	fdc_get_result		; read result bytes
	jc	.error_cmd
	call	fdc_get_error		; get error code
	pop	cx
	or	ah,ah
	jz	.exit			; if no errors
	loop	.read_id_loop		; retry
	jmp	.error

.error_cmd:
	pop	cx

.error:
	stc

.exit:
	pop	bx
	ret

;=========================================================================
; fdc_set_rate - Set transfer rate
; Input:
;	AL = transfer rate (bits 7 and 6)
;		00h - 500 Kbps (1.2M and 1.44M disks)
;		40h - 300 Kbps (360K disk in 1.2M drive)
;		80h - 250 Kbps (360K disk in 360K drive, or 720K disk)
;		0C0h - 1 Mbps (2.88M disks)
; Output:
;	none
;-------------------------------------------------------------------------
fdc_set_rate:
	push	ax
	and	al,fdc_m_rate_bits	; get the data rate bits only
	and	byte [fdc_last_rate],~fdc_m_rate_bits ; clear rate bits
	or	byte [fdc_last_rate],al	; set new rate bits
	rol	al,1
	rol	al,1
	mov	dx,fdc_ccr_reg		; get control configuration register address
	out	dx,al
	mov	dx,word [bp+int_13_dx]	; restore DX
	pop	ax
	ret

;=========================================================================
; fdc_wait_irq - Wait for FDC interrupt for 2 seconds
; Input:
;	none
; Output:
;	CF clear if interrupt had occurred
;		AH = 00h - successful completion
;	CF set if no interrupt
;		AH = 80h - timeout
;	BX = fdc_calib_state
;	AL - trashed
;-------------------------------------------------------------------------
; fdc_wait_irq:
	; push	cx
	; sti
	; clc
	; mov	ax,9001h
	; int	15h			; call OS hook
	; jc	.timeout
	; mov	bx,fdc_calib_state	; contains IRQ flag
	; xor	cx,cx

; %ifdef AT_DELAY

; .zero_loop:
	; test	byte [bx],fdc_irq_flag	; test IRQ flag
	; jnz	.exit			; exit loop if interrupt had occurred
	; in	al,ppi_pb_reg
	; test	al,refresh_flag
	; jz	.zero_loop		; wait 15.09 us
	; loop	.one_loop		; DEC CX; JNZ .one_loop
	; jmp	.timeout

; .one_loop:
	; test	byte [bx],fdc_irq_flag	; test IRQ flag
	; jnz	.exit			; exit loop if interrupt had occurred
	; in	al,ppi_pb_reg
	; test	al,refresh_flag
	; jnz	.one_loop		; wait 15.09 us
	; loop	.zero_loop		; DEC CX; JNZ .zero_loop

; %else ; AT_DELAY

; .1:
	; ; wait 38.13 us (Intel 8088 CPU running on 4.77 MHz)
	; mov	al,10
; .2:
	; test	byte [bx],fdc_irq_flag	; test IRQ flag
	; jnz	.exit			; exit loop if interrupt had occurred
	; dec	al
	; jnz	.2
	; loop	.1

; %endif ; AT_DELAY

; .timeout:
	; mov	ah,fdc_e_timeout	; timeout
	; mov	al, 0xDD		; IRQ timeout
	; out	post_reg,al
	; mov	al,fdc_e_timeout	; timeout 80
	; out	post_reg,al
	; stc
	; pop	cx
	; ret

; .exit:
	; mov	al, 0xDA		; IRQ Exit
	; out	post_reg,al
	; and	byte [bx],~fdc_irq_flag	; clear IRQ flag
	; xor	ah,ah			; AH = 0, CF = 0 - no error
	; pop	cx
	; ret

;=========================================================================
; set_fdc_dor - set FDC Digital Output Register - DOR
; Input:
;	AL = new motor state
; Output:
;	AL, DX trashed
;-------------------------------------------------------------------------
set_fdc_dor:
	ror	al,1
	ror	al,1
	ror	al,1
	ror	al,1
;	or	al,08h			; DMA+IRQ enabled, now DMA Disabled for BBPC
    and     al,77h                  ; Clear Mode Bit 7 and Bit 3 DMA+IRQ on WD37C65C
	mov	dx,fdc_dor_reg		; get digital output register address
	out	dx,al			; send the state to FDC
	ret

;=========================================================================
; get_media_state - Get drive's media state from the data area
; Input:
;	[BP+int_13_dl] = drive number
; Output:
;	BL = drive's media state
;	BH = 0 (destroyed)
;-------------------------------------------------------------------------
get_media_state:
	mov	bl,byte [bp+int_13_dl]
	mov	bh,0			; BX = drive number
	mov	bl,byte [fdc_media_state+bx]
	ret

;=========================================================================
; set_media_state - Store drive's media state in the data area
; Input:
;	AL = media state
;	[BP+int_13_dl] = drive number
; Output:
;	none
;-------------------------------------------------------------------------
set_media_state:
	push	bx
	mov	bl,byte [bp+int_13_dl]
	mov	bh,0			; BX = drive number
	mov	byte [fdc_media_state+bx],al
	pop	bx
	ret

;=========================================================================
; check_cylinder - Compare specified cylinder with value in the BIOS data area
; Input:
;	CH = current cylinder
;	[BP+int_13_dl] = drive number
; Output:
;	ZF = 1 - cylinder matches
;-------------------------------------------------------------------------
check_cylinder:
	push	bx
	mov	bl,byte [bp+int_13_dl]
	mov	bh,0			; BX = drive number
	cmp	byte [fdc_cylinder+bx],ch
	pop	bx
	ret

;=========================================================================
; set_cylinder - Store drive's current cylinder into the BIOS data area
; Input:
;	CH = current cylinder
;	[BP+int_13_dl] = drive number
; Output:
;	none
;-------------------------------------------------------------------------
set_cylinder:
	push	bx
	mov	bl,byte [bp+int_13_dl]
	mov	bh,0			; BX = drive number
	mov	byte [fdc_cylinder+bx],ch
	pop	bx
	ret

;=========================================================================
; check_drive_calibrated - Check if the drive calibrated bit is set
;			   in the BIOS data area
; Input:
;	[BP+int_13_dl] = drive number
; Output:
;	CF = 1 - bit set (drive calibrated)
;-------------------------------------------------------------------------
check_drive_calibrated:
	push	cx
	mov	ch,byte [fdc_calib_state] ; calibration state for primary FDC
	mov	cl,byte [bp+int_13_dl]	; CL = drive number
	inc	cl
	shr	ch,cl			; set CF if drive is calibrated
	pop	cx
	ret

;=========================================================================
; set_drive_calibrated - Set drive calibrated bit in the BIOS data area
; Input:
;	[BP+int_13_dl] = drive number
; Output:
;	none
;-------------------------------------------------------------------------
set_drive_calibrated:
	push	cx
	mov	cl,byte [bp+int_13_dl]	; CL = drive number
	mov	ch,1			; bit 0 set
	shl	ch,cl			; move it into the right position
	or	byte [fdc_calib_state],ch ; set the bit for the primary FDC
	pop	cx
	ret

;=========================================================================
; detect_floppy - Detect floppy configuration
;                 set equipment bits accordingly
; Input:
;	none
; Ouput:
;	AL = NVRAM floppy configuration byte
;-------------------------------------------------------------------------
detect_floppy:
%ifdef BIOS_SETUP
	call	get_floppy
%else ; BIOS_SETUP
	mov	al,DEFAULT_FLOPPIES	
%endif ; BIOS_SETUP
	cmp	al,00h			; No floppy drives?
	je	.exit
	or	byte [equipment_list],equip_floppies
	test	al,0Fh			; Second floppy drive installed?
	jz	.exit
	or	byte [equipment_list],equip_floppy2
.exit:
	ret

;=========================================================================
; get_drive_type - Read drive type from NVRAM or Flash ROM configuration
; Input:
;	DL = drive number (0 or 1)
; Output:
;	CF clear if successful
;		AL = drive type
;	CF set on error (invalid drive type)
;=========================================================================

get_drive_type:
%ifdef BIOS_SETUP
	call	get_floppy
%else ; BIOS_SETUP
	mov	al,DEFAULT_FLOPPIES
%endif ; BIOS_SETUP
	or	dl,dl			; drive 0?
	jnz	.drive_1		; jump if drive 1 - type in bits 3-0
	shr	al,1			; shift drive 0 bits 7-4 to 3-0
	shr	al,1
	shr	al,1
	shr	al,1
.drive_1:
	and	al,0Fh			; mask drive bits
	cmp	al,type_none
	je	.error
	cmp	al,5			; invalid value
	je	.error
	cmp	al,type_2880
	ja	.error
	clc
	ret

.error:
	stc
	ret

;=========================================================================
; print_floppy - Print floppy configuration
; Input:
;	AL = NVRAM floppy configuration byte
; Ouput:
;	none
;-------------------------------------------------------------------------
print_floppy:
	push	ax
	push	si
	mov	si,msg_floppy
	call	print
	ror	al,1
	ror	al,1
	ror	al,1
	ror	al,1
	mov	si,ax
	and	si,0007h
	shl	si,1
    cs	mov	si,word [tbl_floppy+si]
	call	print
	mov	si,msg_floppy_2
	call	print
	ror	al,1
	ror	al,1
	ror	al,1
	ror	al,1
	mov	si,ax
	and	si,0007h
	shl	si,1
    cs	mov	si,word [tbl_floppy+si]
	call	print
	mov	si,msg_crlf
	call	print
	pop	si
	pop	ax
	ret
;=========================================================================
; CHK_STAT_RESULTS based on _2 from PC-Jr BIOS with mods for Xi-8088 for handling interrupt
; process without IRQ6 being enabled and DMA disabled
; THIS ROUTINE WAITS FOR BUSY FLAG TO BE ZERO BIT 4 OF STATUS
; THIS MEANS EXECUTION MODE HAS FINISHED AND NOW IN RESULTS MODE
; NOTE THAT WHEN REPLACING INTERRUPTS THAT RQM and DIO checks do not include FDC Busy
; THE INTERRUPT IS WAITED FOR, THE INTERRUPT STATUS SENSED,
; AND THE RESULT RETURNED TO THE CALLER
; NONE
; CY = 0 SUCCESS
; CY = 1 FAILURE -- ERROR IS IN DISKETTE-STATUS
; (AX) DESTROYED
; INPUT
; OUTPUT
;-------------------------------------------------------------------------


 chk_stat_results:
		  push	  bx                     ; SAVE REGISTERS
		  push    dx
		  xor	  bx, bx                 ; NUMBER OF STATUS POLLS TO ISSUE BEFORE TIMEOUT
          mov	  dx,fdc_status_reg
 
 .check_status:				  
		  in	  al,dx		
		  test	  al, fdc_exec_mode
		  jz	  .exec_clear            ; Execution Mode flag clear so return 

 .dec_counter:				  
		  dec	  bx                     ; DEC LOOP COUNTER
		  jnz	  short	.check_status    ; DO ANOTHER LOOP
		  or	  byte [fdc_last_error], fdc_e_timeout ; Add the timeout bit to FDC Error 
 .seterror:					  
		  stc                            ; RETURN ERROR INDICATION FOR CALLER
 
 .exec_clear:					  
		  pop	  dx					  
		  pop	  bx                     ; RESTORE REGISTERS
		  ret



;=========================================================================
; CHK_STAT_2 from PC-Jr BIOS with mods for Xi-8088 for handling interrupt
; process without IRQ6 being enabled and DMA disabled
; THIS ROUTINE HANDLES THE INTERRUPT RECEIVED AFTER
; A RECALIBRATE, SEEK, OR RESET TO THE ADAPTER
; THE INTERRUPT IS WAITED FOR, THE INTERRUPT STATUS SENSED,
; AND THE RESULT RETURNED TO THE CALLER
; NONE
; CY = 0 SUCCESS
; CY = 1 FAILURE -- ERROR IS IN DISKETTE-STATUS
; (AX) DESTROYED
; INPUT
; OUTPUT
;-------------------------------------------------------------------------


 chk_stat_2:
		  push	  bx                     ; SAVE REGISTERS
		  xor	  bx, bx                 ; NUMBER OF SENSE INTERRUPTS TO ISSUE

 j33_2:				  
		  mov	  al, 08h                ; SENSE INTERUPT STATUS
		  call	  fdc_write              ; ISSUE SENSE INTERUPT STATUS
		  call	  fdc_get_result
		  jc	  short	j35              ; NEC TIME OUT, FLAGS SET IN RESULTS
		  mov	  al, [fdc_ctrl_status]  ; GET STATUS ST0
		  test	  al, 20h                ; IS SEEK OR RECAL OPERATION DONE ST0 Seek End D5 0x20
		  jnz	  short	j35_1            ; JUMP IF EXECUTION OF SEEK OR RECAL DONE
 j33_3:				  
		  dec	  bx                     ; DEC LOOP COUNTER
		  jnz	  short	j33_2            ; DO ANOTHER LOOP
		  or	  byte [fdc_last_error], fdc_e_timeout ; Add the time out bit to FDC Error 
 j34:					  
		  stc                            ; RETURN ERROR INDICATION FOR CALLER
 j35:					  
					  
		  pop	  bx                     ; RESTORE REGISTERS
		  ret
 j35_1:				                 ; SEEK END HAS OCCURED, CHECK FOR NORMAL TERMINATION
		  and	  al, 0C0h               ; MASK NORMAL TERMINATION BITS ST0 D7 & D6 dont have constants yet!
		  jz	  short j35              ; JUMP IF NORMAL TERMINATION
		  or	  byte [fdc_last_error], fdc_e_seek ;  Add the seek error bit to FDC Error
		  jmp	  short	j34
 
    ; fdc_last_error	equ	41h	; DISKETTE-STATUS byte - status of last diskette operation
    ; fdc_ctrl_status	equ	42h	; NEC-STATUS byte[7] - FDC status bytes

    ; FDC status register bits
    ; fdc_stat_ready	equ	80h		; 1 = FDC is ready to send / receive
    ; fdc_stat_dir	equ	40h		; 0 = CPU -> FDC; 1 = FDC -> CPU
    ; fdc_stat_dma	equ	20h		; 1 = DMA and IRQ enabled
    ; fdc_stat_busy	equ	10h		; 1 = FDC is busy

;=========================================================================
; BOUND_SETUP
; THIS ROUTINE SETS UP BUFFER ADDRESSING FOR READ/WRITE/VERIFY
; OPERATIONS
; INPUT
;   ES HAS ORIGINAL BUFFER SEGMENT VALUE
;   BP POINTS AT BASE OF SAVED PARMETERS ON STACK
;   ES HAS SEGMENT WHICH WILL ALLOW 64K ACCESS THE
;   COMBINATION ES:DI AND DS:SI POINT TO THE BUFFER THIS
;   CALCULATED ADDRESS WILL ALWAYS ACCESS 64K OF MEMORY
;   BX DESTOYED (or you could say adjusted for new ES value!)
; OUTPUT
;-------------------------------------------------------------------------

bound_setup:
		  push	  cx                 ; SAVE REGISTERS
		  mov	  bx, [bp+int_13_bx] ; GET OFFSET OF BUFFER FROM STACK
		  push	  bx                 ; SAVE OFFSET TEMPORARILY
		  mov	  cl, 4              ; SHIFT COUNT
		  shr	  bx, cl             ; SHIFT OFFSET FOR NEW SEGMENT VALUE
		  mov	  cx, es             ; PUT ES IN REGISTER SUITABLE FOR ADDING TO
		  add	  cx, bx             ; GET NEW VALUE FOR ES
		  mov	  es, cx             ; UPDATE THE ES REGISTER
		  pop	  bx                 ; RECOVER ORIGINAL OFFSET
		  and	  bx, 0Fh            ; NEW OFFSET
		  mov	  si, bx             ; DS:SI POINT AT BUFFER
		  mov	  di, bx             ; ES:DI POINT AT BUFFER
		  pop	  cx                 ; 
		  ret                        ;
;=========================================================================
; THIS SUBROUTINE SETS DS TO POINT TO THE BIOS DATA AREA
; INPUT NONE , OUTPUT DS IS SET
;-------------------------------------------------------------------------

dss:
		  push	  ax
		  mov	  ax, 40h ; BIOS Data aread {0x0040:0000] or 0x00400
		  mov	  ds, ax
		  pop	  ax
		  ret

;=========================================================================
; NUM_TRANS
; THIS ROUTINE CALCULATES THE NUMBER OF SECTORS THAT
; WERE ACTUALLY TRANSFERRED TO/FROM THE DISKETTE
; INPUT
;    (CH) = CYLINDER OF OPERATION
;    (CL) = START SECTOR OF OPERATION
; OUTPUT
;    (AL) = NUMBER ACTUALLY TRANSFERRED
; NO OTHER REGISTERS MODIFIED
;-------------------------------------------------------------------------

num_trans:
          push    ds
		  push    si
		  mov	  al, ds:fdc_ctrl_status+3  ; GET CYLINDER ENDED UP ON
		  cmp	  al, [bp+int_13_ch]        ; SAME CYLINDER/TRACK COUNT AS WE STARTED? NOTE PCJR BP Offsets are different from XI BIOS
		  mov	  al, ds:fdc_ctrl_status+5  ; GET ENDING SECTOR
		  jz	  short	j45                 ; IF ON SAME CYL, THEN NO ADJUST
		  ;mov	  bl, 8                     ; Get EOT Tracks per sector, byte 4 in parameter table in ah
		  ;call	  get_parm???               ; The PC Jr doubles the parameter and if bit 1 is set sends to the FDC
		                                    ; We just need sectors per track from the INT 1E Disk Parameter table
		  lds	  si,[1Eh*4]		        ; DS:SI -> INT 1Eh Disk Parameter Table Vector now in DS and si
		  mov	  ah,byte [si+4]		    ; Get sectors per track
		  mov	  al, ah                    ; Into al
j45:
		  inc	  al                        ; USE EOT+l FOR CALCULATION
		  sub	  al, [bp+int_13_cl]        ; SUBTRACT START SECTOR FROM END SECTOR
		  ;mov	  [bp+0Eh], al              ; Check what this is??? Is used as temporary variable on stack, value is in AL anyway
		  pop     si
		  pop     ds
		  ret

; ------------------------------------------------------
; rw_opn
;	 this routine performs read/write/verify operation
;    Based on PC-Jr BIOS with all steps already taken
;    care of removed and simplified.
; Note jnn_m: labels match PC Jr BIOS for cross reference
; ------------------------------------------------------
rw_opn:

;********* BBPC This is where we need to start Non DMA code in BBPC - uses CX for address to jump to handle FDC data *****************
;----- initialize cx for jump	after last parameter is	passed to nec
		  push	  es					  ; Need to save this as seems to break checksum check and perhaps DOS
		  mov	  al, [bp+int_13_ah]	  ; retreive command parameter
		  test	  al, 1		              ; is this and	odd numbered function write=3, read=2, verify=4
		  jz	  short	j16_1	          ; jump if not odd numbered
		  mov	  cx, write_loop
		  jmp	  short	j16_3
j16_1:
		  cmp	  al, 2		              ; is this a read?
		  jnz	  short	j16_2	          ; jump if verify
		  mov	  cx, read_loop
		  jmp	  short	j16_3
j16_2:
		  mov	  cx, verify_loop
j16_3:		  
          mov     dx, fdc_status_reg ; Load IO Port for FDC Status port (0x03F4)
		  cld

		  ; call	  disable	  ; disable all	interrupts
		  cli                 ; For now we don't need to worry about watch dog timer IRQ6 or Timer 0
          call    bound_setup ; This will adjust es:bx to give a 64K page, was called by disable
		  push	  es          ; es:bx will have modified values for 64K range in bx
		  pop	  ds		  ; initialize ds for write
		  jmp	  cx		  ; jump to appropriate	r/w/v loop

;----------------------------------------------------------------
;***note***
;	   data	is transferred using polling algorithms. these loops
;	   transfer a data byte	at a time while	polling	FDC for
;	   next	data byte and completion status
;----------------------------------------------------------------
verify_loop:
          ; cx was used for address to jmp to so use ch as fdc_exec_mode and cl as fdc_stat_ready
		  ; register/register tests are 3 clocks, register immediate are 5 clocks!
		  mov	  cx, fdc_exec_mode * 256 + fdc_stat_ready
		                             ; 0x80 less than 80 then will read status as not ready, Equal will be exec_mode=0
		  mov	  bx, j22_2          ; jmp to register is a few clock cycles faster
verify_loop1:
		  in	  al, dx	         ; read master status register
		  test	  al, ch		     ; has FDC entered execution phase yet?
		  jz	  short	verify_loop1 ; no, continue sampling
j22_2:
		  test	  al, cl			 ; fdc_stat_ready is data ready? (RQM)
		  jnz	  short	j22_4	     ; jump if data transfer is ready
		  in	  al, dx	         ; read status	port
		  test	  al, ch			 ; are we done? In results mode if = 0
		  jnz	  short	j22_2	     ; jump if more transfers = 1
		  jmp	  short	op_end	     ; transfer done
j22_4:
		  inc	  dx		         ; point at FDC data register (0x03F5)
		  in	  al, dx	         ; read data
		  dec	  dx		         ; point at FDC status register (0x03F4)
		  in	  al, dx	         ; read status	port
		  test	  al, ch  ; are we done? In results mode if = 0
		  jnz	  short	j22_2	     ; continue
		  jmp	  short	op_end	     ; we are done
;------ read operation
read_loop:
          ; cx was used for address to jmp to so use ch as fdc_exec_mode and cl as fdc_stat_ready
		  ; register/register tests are 3 clocks, register immediate are 5 clocks!
		  mov	  cx, fdc_exec_mode * 256 + fdc_stat_ready
		                             ; 0x80 less than 80 then will read status as not ready, Equal will be exec_mode=0
		  mov	  bx, read_loop2     ; jmp to register is a few clock cycles faster
read_loop1:
		  in	  al, dx	         ; read master status register
		  test	  al, ch             ; fdc_exec_mode has FDC entered execution phase yet?
		  jz	  short	read_loop1   ; has not started execution phase yet check again
		  jmp     short exec_start   ; Don't re-read status when exec mode starts otherwise we miss firstbyte
j22_5:
		  in	  al, dx	         ; read status port
exec_start:
		  test	  al, cl             ; fdc_stat_ready is data ready? (RQM)
		  jz	  short j22_5 	     ; re-test status if not ready
first_read:                          ; For first read we won't check completion of exec mode		  
		  inc	  dx		         ; point at FDC data register (0x03F5)
		  in	  al, dx	         ; read data from FDC
		  stosb			             ; transfer data to memory using ES:DI (set in bound_setup)
		  dec	  dx		         ; point at FDC status register (0x03F4)
		  add	 ch, cl				 ; Add the flags together in ch for TEST using Sign flag on D7

read_loop2:                          ; Second byte onwards check 
		  in	  al, dx	         ; read status port
		  test	  al, ch             ; Test with A0 Ready + Exec Mode if zero then finished reading requested sectors
		  jz	  short	op_end       ; Ready=0 AND Exec_Mode=0 so results mode
		  jns	  short read_loop2   ; Jump Not Sign as if D7 (ready) is 1 then SF=1, SF=0 re-test status if not ready

		  inc	  dx		         ; point at FDC data register (0x03F5)
		  in	  al, dx	         ; read data from FDC
		  stosb			             ; transfer data to memory using ES:DI (set in bound_setup)
		  dec	  dx		         ; point at FDC status register (0x03F4)
		  jmp	  bx                 ; bx=read_loop2 continue with read operation



;------ write	and format operation
write_loop:
          ; cx was used for address to jmp to so use ch as fdc_exec_mode and cl as fdc_stat_ready
		  ; register/register tests are 3 clocks, register immediate are 5 clocks!
		  mov	  ch, fdc_exec_mode
		  mov	  cl, fdc_exec_mode + fdc_stat_ready
		                             ; 0x80 less than 80 then will read status as not ready, Equal will be exec_mode=0
		  mov	  bx, j22_7          ; jmp to register is a few clock cycles faster

write_loop1:
		  in	  al, dx	         ; read master status register
		  test	  al, ch             ; fdc_exec_mode has FDC entered execution phase yet?
		  jz	  short	write_loop1  ; no, continue looping
j22_7:
		  in	  al, dx	         ; read status port
		  test	  al, cl	         ; is the FDC still in execution phase or ready sign not set if not ready?
		  jz	  short	op_end	     ; jump if execution phase is done
		  jns	  short	j22_7	     ; Jump Not Sign as if D7 (ready) is 1 then SF=1, SF=0 re-test status if not ready
		  inc	  dx		         ; point at data register
		  lodsb			             ; transfer byte to FDC from memory using DS:SI (set in bound_setup)
		  out	  dx, al	         ; write the byte on the diskette
		  dec	  dx		         ; point at the status register (0x03F4)
		  jmp	  bx	             ; continue with write (or format??)
;------ transfer process is over
op_end:
		  call	  dss
		  ;call	  enable
		  pop	  es			     ; We have finished with ES now
		  sti
j17:
		  jb	  short	j20_rw_err
		  call	  fdc_get_result
		  jb	  short	j20_rw_err
;------ check	the results returned by	the controller
		  cld
		  mov	  si, fdc_ctrl_status ; Point to FDC Status bytes set by results
		  lodsb                       ; Load Status ST0
		  and	  al, 0C0h            ; Test for Normal Termination D7=0 & D6=0
		  jz	  short	j22
		  cmp	  al, 40h             ; D6=1 Indicates command not completed sucessfully
		  jnz	  short	j18_rw_nec_fail
;----------------------------------------------------------------
;***note***
;	  the current system configuration has no DMA. in order	to
;	  stop the FDC an eot must be passed to	force the FDC to halt.
;	  therefore, the status	returned by the	FDC will always	show
;	  an eot error.	If this	is the only error returned and the
;	  number of sectors transferred	equals the number sectors
;	  requested in this interrupt call then	the operatlon has
;	  completed successfully. If an	eot error is returned and the
;	  requested number of sectors is not the number	of sectors
;	  transferred then the error is	legitimate. When the eot error
;	  is invalid the status	bytes returned are updated to
;	  reflect the status of	the operation as if DMA had been present
;	  Not EOT is represented by End of Cylinder D7 in ST1
;----------------------------------------------------------------

		  lodsb			             ; get Status ST1
		  cmp	  al, 80h            ; D7 End of Cylinder - is this the only error?
		  jz	  short	j21_1	     ; normal termination,	no error
		  shl	  al, 1		         ; not eot error, bypass error bits
		  shl	  al, 1
		  shl	  al, 1		         ; test for crc error D5
		  mov	  ah, fdc_e_crc
		  jb	  short	j19_rw_fail  ; rw_fail
		  shl	  al, 1		         ; test for DMA overrun D4
		  mov	  ah, fdc_e_dma
		  jb	  short	j19_rw_fail  ; rw_fail
		  shl	  al, 1
		  shl	  al, 1		         ; test for record not found D2
		  mov	  ah, fdc_e_notfound
		  jb	  short	j19_rw_fail  ; rw_fail
		  shl	  al, 1
		  shl	  al, 1		         ; test missing address mark D0
		  mov	  ah, fdc_e_address
		  jb	  short	j19_rw_fail  ; rw_fail
;------ FDC must have failed
j18_rw_nec_fail:
		  mov	  ah, fdc_e_failure  ; rw-nec-fail
j19_rw_fail:

		  or	  [ds:fdc_last_error], ah ; Set error bits in FDC error status
		  call	  num_trans	         ; how many were really transferred
j20_rw_err:
          stc                        ; Need to indicate error, ah has error No.
		  ret			             ; return to caller
;------ operation was	successful
j21_1:
		  mov	  bl, [bp+int_13_cl]	     ; get number of sectors pass from stack  - int_13_cl PC Jr has different offsets to BP!
		  call	  num_trans	         ; how many got moved, al contains number of sectors
		  cmp	  bl, al             ; number requested == number of actually transferred ?
		  jz	  short	j21_2	     ; transfer successful
;------ operation attempted to access data past eot. this is a real error

		  or	  byte [ds:fdc_last_error], fdc_e_notfound ; record	not found
		  mov	  byte [ds:fdc_ctrl_status+1], fdc_e_timeout ; ST1 gets correct value
		  stc
		  ret
j21_2:
		  xor	  ah, ah	         ; clear ah for FDC_status update (was ax but al has sector count in it)
		  xor	  si, si             ; index to FDC status array
		  mov	  [si+fdc_ctrl_status], ah ; zero out byte, ST0
		  inc	  si		         ; point index at second byte
		  mov	  [si+fdc_ctrl_status], ah ; zero out byte, ST1
		  jmp	  short	j21_3_opn_ok
j22:
		  call	  num_trans
j21_3_opn_ok:
		  xor	  ah, ah	  ; no errors
		  ret
