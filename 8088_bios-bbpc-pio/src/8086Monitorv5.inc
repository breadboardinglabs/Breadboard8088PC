;===========================================================================
; 8086monitor.asm - Nanocomp 8086 Monitor migrated from Motorola MC6809 code
; Note this is a literal translation of the 6802/6809 code and is not efficient or best practice
; The Nanocomp 6809 monitor was a quick translation of the 6802 version in the first place!
; nasm -f bin -o 8086monitor.bin -l 8086monitor.lst 8086monitorv4.asm
; 
; Version          Date            Description
;  1.0             6/1/24          Initial migration from 6809 to 8088, minus Save & Load SREC
;  2.0             8/1/24          Add Save and Load SREC for serial port, remove test RAM bytes, 57.6Kbps /64 Enabled
;  3.0             19/1/24         Convert to use IO Ports and add setting Extra Segment for Mem access and Code Segment for Go
;                                  Bug fixes for Go and added stack pointer to Register Display
;  4.0             10/4/24         Mask PIC interupt flags on power up. Add dummy interrupt function including EOI notification to PICs
;                                  Populate interupt vectors with dummy interupt function, leave video and floppy parameters int_1D & int_1E
;                                  Add far function wrappers for 7 segment display and key pad functions to allow calling from another code segment
;  5.0             07/05/24        Merged Monitor into BPC Bios as V5 at F000:B000. Use B to boot BIOS calls cold_start
;
; Conversion of 6809 Nanocomp monitor to 8086 Breadboard PC
;=========================================================================
; Note that due to there being no Monitor commands (yet) to set Segments then SS=7000, DS=7000 and ES=D000 (PIA etc). GO will use [7000:xxxx]
; Now E and C can be used to set the Extra and Code segments used by Memdisp, Save, Load and Go
; INT 3 is equivelevent of SWI used to do Register Display. 8088 Pushes Flags on Stack with CS and IP
; Need PUSHA to have most state (no DS, SS or ES),  AX, CX, DX, BX, SP, BP, SI, DI. SP+6 will be value before INT
; INT decrements the stack pointer by two, pushes the flags onto the stack, and clears the trap (TF) and interrupt-enable (IF) flags to
; disable single-step and maskable interrupts. The flags are stored in the format used by the PUSHF instruction. SP is decremented again
; by two, and the CS register is pushed onto the stack. The address of the interrupt pointer is calculated by multiplying interrupt-type by
; four, the second word of the interrupt pointer replaces CS. SP again is decremented by two and IP is pushed onto the stack and is 
; replaced by the first word of the interrupt pointer.

                      ; Flags Register
                      ; CC  15 14 13 12 11 10 9  8  7  6  5  4  3  2  1  0
                      ;     -  -  -  -  OF DF IF TF SF ZF -  AF -  PF -  CF
                      ;
                      ; 0 CF Carry Flag
                      ; 1 -
                      ; 2 PF Parity Flag
                      ; 3 -
                      ; 4 AF Aux. Carry Flag
                      ; 5 -
                      ; 6 ZF Zero Flag
                      ; 7 SF Sign Flag
                      ; 8 TF Trap Flag
                      ; 9 IF Interrupt Flag
                      ;10 DF Direction Flag
                      ;11 OF Overflow Flag
                      ;12-15 - 
                      
;-           After power up, display - wait for command
;RST Reset   Is hard reset bringing /RST line low via power supervisor debounce circuit
;AB  Abort   Is Non Maskable Interupt (NMI), vector at [0000:0008] IP:CS
;G  Go       Displays G to acknowledge command, enter 4 digit hex address and will run on 4th digit
;CN Continue After SWI or Abort (NMI) press CN to continue, values on the stack will be pulled including instruction pointer
;M  Memory   Display M in far right display (upside down U),
;            4 digit Hex entered in left 4 digits, 
;            displays memory contents in right two digits
;            change by typing in new digits enter in from right shift to left
;            Press I to save and move to next 
;            No changes press I to advance or AB to abort
;            Invalid address or hex bytes will return to monitor - prompt
;S  Save     SAVE range to serial port in V2+ in S19/SREC format 7DCB 
;            Display S to prompt for Start address
;            Display F to prompt for for finish address
;            Displays F when finished
;L  Load     LOAD from serial port in V2+ 7DC4
;            When finished Display F Finished or E for Hex error or C for Checksum error
;AB Abort    Return to monitor start via NMI if not changed NMI vector
;R  Register Display register values via the pushed values on the stack (automatic after INT3 vector at [0000:000C] IP:CS)
;            Right digits show 
;            	FL Flags register
;            	CS Code Segment
;            	IP Instruction Pointer
;             AX AX register CX, DX, BX, SP, BP, SI, DI
;            	CX CX Register
;             DX DX Register
;             BX BX Register
;             SP SP Register
;             BP BP Register
;             SI SI Register
;             DI DI Register
;            Press I between values
;            After DI displayed returns to Monitor, AB to Abort to monitor
;E Extra Segment Enter 4 chracter Hex address to set the ES register for use with Mem, Save and Load
;C Code Segment Enter 4 chracter Hex address to set the CS register for use with Go
;cpu	8086

; This macro includes the setloc macro to pad code with FF to a given address
;%include "macro.inc"

; Define 7 Segment display character codes
seg0                          EQU 0x7E
seg1                          EQU 0x06
seg2                          EQU 0x5B
seg3                          EQU 0x1F
seg4                          EQU 0x27
seg5                          EQU 0x3D
seg6                          EQU 0x7D
seg7                          EQU 0x0E
seg8                          EQU 0x7F
seg9                          EQU 0x3F
segA                          EQU 0x6F
segB                          EQU 0x75
segC                          EQU 0x78
segD                          EQU 0x57
segE                          EQU 0x79
segF                          EQU 0x69
segG                          EQU 0x7C
segH                          EQU 0x67
segI                          EQU 0x60
segL                          EQU 0x70
segM                          EQU 0x6E ; (upside down U)
segN                          EQU 0x6E ; (upside down U)
segO                          EQU 0x7E
segP                          EQU 0x6B
segS                          EQU 0x3D ; (Same as 5)
segU                          EQU 0x76
segX                          EQU 0x67
segY                          EQU 0x37
segDash                       EQU 0x01
segSpace                      EQU 0x00

; Define Nanocomp Keypad key codes
keyCN                         EQU 0x25
keyG                          EQU 0x35
keyI                          EQU 0x32
keyL                          EQU 0x05
keyM                          EQU 0x31
keyP                          EQU 0x15 ; P Punch replaced with S Save
keyS                          EQU 0x15
keyR                          EQU 0x30
key0                          EQU 0x22
key1                          EQU 0x24
key2                          EQU 0x02
key3                          EQU 0x12
key4                          EQU 0x14
key5                          EQU 0x00
key6                          EQU 0x10
key7                          EQU 0x04
key8                          EQU 0x01
key9                          EQU 0x11
keyA                          EQU 0x03
keyB                          EQU 0x13
keyC                          EQU 0x23
keyD                          EQU 0x33
keyE                          EQU 0x21
keyF                          EQU 0x20

asciiNull                     EQU 0x00
asciiBS                       EQU 0x08
asciiLF                       EQU 0x0A
asciiFF                       EQU 0x0C
asciiCR                       EQU 0x0D

ascii0                        EQU 0x30
ascii1                        EQU 0x31
ascii3                        EQU 0x33
ascii9                        EQU 0x39
asciiA                        EQU 0x41
asciiC                        EQU 0x43
asciiF                        EQU 0x46
asciiS                        EQU 0x53

; Serial Port Constants
SERIALCTRL                    EQU  0xE000           ; Use IO Port E000
SERIALSTATUS                  EQU  0xE000           ; Use IO Port E000
SERIALDATA                    EQU  0xE001           ; Use IO Port E001

CTRLRESET                     EQU  0b00000011       ; CR1, CR0 1,1 for Master Reset
CTRLDIVIDE16                  EQU  0b00000001       ; CR1, CR0 0,1 for divide by 16
CTRLDIVIDE64                  EQU  0b00000010       ; CR1, CR0 1,0 for divide by 64
CTRLWORD8N1S                  EQU  0b00010100       ; CR4, CR3, CR2 1, 0, 1 for 8 Bits, 1 Stop No Parity
CTRLRTSLOW                    EQU  0b00000000       ; CR6, CR5, 0,0 RTS low IRQ disabled, requests data
CTRLRTSHIGH                   EQU  0b01000000       ; CR6, CR5, 1,0 RTS high IRQ disabled, prevents receiving data
CTRLIRQENABLED                EQU  0b10000000       ; CR7, 1 IRQ enabled

STATUSRDRF                    EQU  0b00000001       ; Receive Data Register Full Bit 0
STATUSTDRE                    EQU  0b00000010       ; Transmit Data Register Empty Bit 1
STATUSDCD                     EQU  0b00000100       ; Data Carrier Detect Bit 2
STATUSCTS                     EQU  0b00001000       ; Clear To Send Bit 3
STATUSFE                      EQU  0b00010000       ; Framing Error Bit 4
STATUSOVRN                    EQU  0b00100000       ; Receiver Overrun Bit 5
STATUSPE                      EQU  0b01000000       ; Parity Error Bit 6
STATUSIRQREQ                  EQU  0b10000000       ; Interupt Request /IRQ Bit 7

EOS                           EQU  0x00             ; end of string

; Now Monitor is part of BIOS then disable
;%define	START		0x8000		; BIOS/ROM starts at offset 8000h Physical Address is F8000 [F000:8000]
%define	STACKSTART 0xFFEC ; Saved User Stack Pointer (and possibly Stack Segment) after INT 3
%define	GO_CS 0xFFEA ; Code Segment for Go
%define	GO_IP 0xFFE8 ; Instruction Pointer for Go
%define SAVE_START 0xFFE6 ; Save Start Addres 
%define SAVE_FINISH 0xFFE4 ; Save Finish Address
%define EXTRA_SEGMENT 0xFFE2 ; E command data segment used by Memdisp, Save, Load


%define MONSTACK 0xFEFE   ; Monitor default stack starting point, on less FEFF is where it starts
%define USERSTACK 0xFDFE   ; User program default stack starting point, on less FEFF is where it starts

%define	DBUF		0xFFF0		; Display Buffer DBUF starts at offset FFF0 Physical Address is 7FFF0 [7000:FFF0] at top of 512K RAM
; Note es set to D000 for PIA

%define DATASEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define USERCODESEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define STACKSEG 0x7000   ; Stack Segment is current top of 512K RAM minus 256 bytes for Monitor use
%define	PIASEG  0xD000		; PIA Segment A D0000    [D000:0000]
%define MONCODESEG 0xF000 ; Monitor Code Segment

%define DIVIDE0VECTOR 0x0000 ; IP and CS for divide by 0
%define SINGLESTEPVECTOR 0x0004 ; IP and CS for Single Step
%define NMIVECTOR 0x0008  ; IP and CS for Non Maskable Interrupt
%define INT3VECTOR 0x000c ; IP and CS for INT 3 Software Interrupt
%define OVERFLOWVECTOR 0x0010 ; IP and CS for Overflow

%define	PORTA		0xD000		; PIA Port A IO Port D000
%define	CTRLA		0xD001		; PIA Control A IO Port D001
%define	PORTB		0xD002		; PIA Port B IO Port D002
%define	CTRLB		0xD003		; PIA Control B IO Port D003


pic1_reg0	equ	0x20        ; PC 8259 Interrupt controllers for EOI in irq_ignore
pic1_reg1	equ	21h
pic2_reg0	equ	0xA0        ; PC 8259 Interrupt controllers for EOI in irq_ignore
pic2_reg1	equ	0A1h

pit_ch0_reg	equ	40h       ; PC 8252/8253 Programmable Interval Timer ports
pit_ch1_reg	equ	41h
pit_ch2_reg	equ	42h
pit_ctl_reg	equ	43h
    

;org	START		; Use only upper 32 KiB of ROM, now at 0xB000 rather than 0x8000

;   Reset Handler
reset:
                     ;RESET               lds      #MONSTACK          ; Load System Stack Pointer with Monitor default  
                     ;                    jsr      DISPRESET          ; Reset VGA Display CRTC etc
    ; Clear Interrupts until vectors defined
    cli
    xor ax,ax
    mov ds,ax  ; Interupt Vectors start at [0000:0000] and are 4 bytes each IP then CS

    ; Define Divide by 0 error vector (Display Registers now Ignore)
    mov [ds:DIVIDE0VECTOR], word sw_int_ignore
    mov [ds:DIVIDE0VECTOR+2],word  MONCODESEG
    ; Define Single Step vector (Display Registers  now Ignore)
    mov [ds:SINGLESTEPVECTOR], word sw_int_ignore
    mov [ds:SINGLESTEPVECTOR+2], word MONCODESEG
    ; Define NMI Vector and Code Segment
;   Set up NMI to point here
                     
;    mov [ds:NMIVECTOR], word regdispswi        ;                     ldx      #NMISR            ; Save NMI Service Routine address in NMI Vector  
    mov [ds:NMIVECTOR], word nmisr        ;                     ldx      #NMISR            ; Save NMI Service Routine address in NMI Vector  
    mov [ds:NMIVECTOR+2], word MONCODESEG ;                     stx      NMIVEC
    ; Define Software Interrupt INT 3 Vector and Code Segment
    mov [ds:INT3VECTOR], word regdispswi
    mov [ds:INT3VECTOR+2], word MONCODESEG
    ; Define Overflow Error Vector and Code Segment (Display Registers  now Ignore)
    mov [ds:OVERFLOWVECTOR], word sw_int_ignore
    mov [ds:OVERFLOWVECTOR+2], word MONCODESEG

    ; Use hw_int_ignore for Hardware IRQ 0-7 mapped to IRQ 8-15 starting at 0x20-0x3F and IRQ 8-15 to IRQ 70-77 0x1C0-0x1DF rest can be sw_int_ignore
    ; iterate over 0x014 to 0x3ff with sw_int_ignore then populate 0x20-0x3F and IRQ 8-15 to IRQ 70-77 0x1C0-0x1DF rest can be sw_int_ignore
    ; Ignore 1D and 1E for Video and Floppy Parameters 0x74 - 0x7b, 0x7c onwards
;    mov cx, 0x00FA
    mov cx, 0x0018
    mov di, 0x0014
vectorLoop:
    mov [ds:di], word sw_int_ignore
    add di, 2
    mov [ds:di], word MONCODESEG
    add di, 2
    loop vectorLoop

    mov cx, 0x00E0
    mov di, 0x007c
vectorLoop1:
    mov [ds:di], word sw_int_ignore
    add di, 2
    mov [ds:di], word MONCODESEG
    add di, 2
    loop vectorLoop1

    ; Now do IRQ0-IRQ7
    mov cx, 0x0008
    mov di, 0x0020
vectorLoop2:
    mov [ds:di], word hw_int_ignore
    add di, 2
    mov [ds:di], word MONCODESEG
    add di, 2
    loop vectorLoop2

    ; Now do IRQ8-IRQ15
    mov cx, 0x0008
    mov di, 0x01c0
vectorLoop3:
    mov [ds:di], word hw_int_ignore
    add di, 2
    mov [ds:di], word MONCODESEG
    add di, 2
    loop vectorLoop3

    ; Initialise PIC0 and PIC1 as per BIOS except for Mask all IRQ
    mov	al,11h			; ICW1 - edge triggered, cascade, ICW4
    out	pic1_reg0,al
    out	pic2_reg0,al
    mov	al,8			; ICW2 - interrupt vector offset = 8
    out	pic1_reg1,al
    mov	al,70h			; ICW2 - interrupt vector offset = 70h
    out	pic2_reg1,al
    mov	al,4			; ICW3 - slave is connected to IR2
    out	pic1_reg1,al
    mov	al,2			; ICW3 - slave ID = 2 (IR2)
    out	pic2_reg1,al
    mov	al,1			; ICW4 - 8086/8088
    out	pic1_reg1,al
    out	pic2_reg1,al

    mov	al,0xff		; OCW1 - Mask all interrupts
    out	pic1_reg1,al
    out	pic2_reg1,al

    ; Initialize PIT (8254 timer) and disable timers 0,1,2 mode 0
    mov	al,30h			; channel 0, LSB & MSB, mode 0, binary
    out	pit_ctl_reg,al
    mov	al,0
    out	pit_ch0_reg,al
    out	pit_ch0_reg,al
    mov	al,70h			; channel 1, LSB & MSB, mode 0, binary
    out	pit_ctl_reg,al
    mov	al,0h			; 
    out	pit_ch1_reg,al
    out	pit_ch1_reg,al

    mov	al,0xb0			; channel 3, LSB & MSB, mode 0, binary
    out	pit_ctl_reg,al
    mov	al,0h			; 
    out	pit_ch2_reg,al
    out	pit_ch2_reg,al

    ; DMA should be disabled from RESET - check this
    
    
    ; Don't want to set stack with segments incase we need to refresh segments later
    mov sp, MONSTACK ; Stack starts at 0x7FEFE downwards [7000:FEFE] leaving 256 bytes for Monitor working memory from (7)FF00-(7)FFFF
    mov [ds:STACKSTART], word USERSTACK; Set the default userstack value on reset    
    
monitor_segments:    
    ; Set up the segment registers and stack
    mov ax, STACKSEG
    mov ss, ax     ; Set Stack Segment at 0x7000
    mov ax, DATASEG
    mov ds, ax     ; Set ds to 0x7000
    mov es, ax     ; Set es to 0x7000
    ;mov ax, PIASEG PIA now uses IO Port don't need es anymore, use for memdisp
    ;mov es, ax     ; Set es to 0xD000 for PIA (at 0xD0000)  PIA now uses IO Port don't need es anymore, use for memdisp
    mov [ds:GO_CS], word USERCODESEG; Store default User Code Segment for Go now can be set with C code segment command
    mov [ds:EXTRA_SEGMENT], word DATASEG; Store default Data Segment for Go now can be set with E extra segment command     
;    xor ax, ax
;    xor bx, bx
;    xor cx, cx
;    xor dx, dx
;    xor di, di
;    xor si, si
    
    ; Re-enable interrupts
    sti
    ; Write ascending byte values to ds
;    xor di, di
 ;   xor bx, bx

;fill_memory:
    ; Write the incrementing bx to memory for testing Memory access
;    mov [ds:di], bl

    ; Increment bl and the index register
;    inc bl
;    inc di
    ;inc di when using bx for word at time
    ; If di is 0, increase the data segment register to next 64K block
;    jnz fill_memory
    ;mov [ds:0x0000], byte 0xcc 
    
    ; Add Flags, IP and CS to stack after reset as DisplayRegisters may get confused before INT3 or NMI?
    xor ax, ax  ; Put flags in known state
    pushf
    mov ax, MONCODESEG
    push ax     ; Monitor CS
    mov ax, 0x1234
    push ax     ; Test IP
    
fill_disp_buff:
    ; Write NC 8088 version# 
    mov di, DBUF
    mov byte [ds:di], segN
    inc di
    mov byte [ds:di], segC
    inc di
    mov byte [ds:di], seg8
    inc di
    mov byte [ds:di], seg0
    inc di
    mov byte [ds:di], seg8
    inc di
    mov byte [ds:di], seg8
    inc di
    mov byte [ds:di], segDash
    inc di
    mov byte [ds:di], seg5 ; Version 5
    call getkey

    ; Fill registers with test data for testing Reg Display
    mov ax, 0x2233
    mov cx, 0x4455
    mov dx, 0x6677
    mov bx, 0x8899
    mov bp, 0xaabb
    mov si, 0xccdd
    mov di, 0xeeff

nmisr:
   ; Put rest of registers on the stack (now has Flags and dummy IP and CS if after reset)
   ; After INT 3 or NMI then Flags, CS and IP are on stack already
   ; pusha does not exist on 8088/8086!

   push sp  ; Need sp on stack to use in Regdisp
   push ax
   push cx
   push dx
   push bx
   push bp
   push si
   push di

   ; Save User Program Stack pointer current value, is restored on Continue or set on GO   default FDFE
   mov [ds:STACKSTART], sp; NMISR               sts      STACKSTART         ; Save current stack pointer at STACKSTART $13F8 (after NMI or RESET)


resume:
    ; Set up the segment registers and stack
    mov ax, STACKSEG ;RESUME               lds      #MONSTACK         ; Resume monitor and reinitialise stack pointer   
    mov ss, ax     ; Set Stack Segment at 0x7000
    mov sp, MONSTACK ; Reset Stack from user to monitor which is allocated 250+ bytes FEFE - FE00
    mov ax, DATASEG
    mov ds, ax     ; Set ds to 0x7000
    mov ax, [ds:EXTRA_SEGMENT] ; Get saved extra segment value, defaults to 7000 
    mov es, ax     ; PIA now uses IO Port for es used for Memdisp, Save & Load
    ; Leave stack pointer for Register Display, reset will fix broken Stack
    
                   ;                     lda      #dpRam            ; Initialise direct page register 
                   ;                     tfr      a,dp
   call cleardisp  ;                     bsr      CLEARDISP         ; Clear 7 segment display

;   Show '-' at left
   mov ah, segDash ;                     lda      #segDash          ; Display - on left hand digit 1   
   mov [ds:DBUF],ah;                     sta      <dbuf+0 
   call getkey     ;                     lbsr     GETKEY            ; Wait for a key

;   If key is 'M'
   cmp ah, keyM    ;                     cmpa     #keyM             ; If M Display Memory
   je memdisp      ;                     lbeq     MEMDISP

;   If key is 'R'
   cmp ah, keyR    ;                     cmpa     #keyR             ; If R display Registers
   je regdisp      ;                     beq      REGDISP

;   If key is 'CN'
   cmp ah, keyCN   ;                     cmpa     #keyCN            ; If Continue then pull CPU state from stack and continue 
   je cont         ;                     beq      CONT

;   If key is 'L'
   cmp ah, keyL    ;                     cmpa     #keyL             ; If L then Load from Serial Port in S-REC
   je load         ;                     lbeq     LOAD

;   If key is 'S' (was P for punch)
   cmp ah, keyS    ;                     cmpa     #keyS             ; If S then Save to Serial Port in S-REC
   je save         ;                     lbeq     SAVE

;   If key is 'E' Extra Segment
   cmp ah, keyE    ;
   je setes        ;

;   If key is 'C' Code Segment
   cmp ah, keyC    ;
   je setcs        ;

;   If key is 'B' Boot BIOS via cold_start
   cmp ah, keyB    ;
   mov	word [warm_boot],0	; indicate cold boot
   je cold_start; Start the PC BIOS

;   If key is 'G'
   cmp ah, keyG    ;                     cmpa     #keyG             ; If G then prompt for Address and execute starting at that address
   jne resume      ;                     bne      RESUME            ; Otherwise wait for another key

;   Prompt and accept start address
go:
   mov ah, segG    ;GO                   lda      #segG             ; Display G on right hand data segment 6
   mov [ds:DBUF+5],ah;                   sta      <dbuf+5
   call baddr      ;                     lbsr     BADDR             ; Get 16 bit address in bx and display on segments 1-4
   ; Temporary measure until sorted segments and stack from INT3
   mov [ds:GO_IP], bx ;               Store desired IP
   ; mov [ds:GO_CS], word USERCODESEG;Store User Code Segment Now done by reset and C Command
   mov sp, [ds:STACKSTART] ; Restore the user stack pointer from FDFE downwards, saved on NMI and INT 3
  ; Need FAR in call to use Code Segment at GO_IP/GO_IP_CS
   call FAR [ds:GO_IP]    ; Now use IP and CS from GO_IP_CS
                   ;                     ldy      <STACKSTART       ; Get previous saved stack pointer before NMI or SWI
                   ;                     stx      $0A,y             ; Overwrite PC saved on stack
                   ;                     lda      #$80              ; Set Entire bit in Condition Code register on stack
                   ;                     ora      ,y                ; OR with existing CC value
                   ;                     sta      ,y                ; Save back CC with Entire bit set
   jmp resume      ; Return to monitor if return from Go
   
cont:
                   ;CONT                 lds      <STACKSTART       ; Load stack pointer from saved value
   ;popa 8088/8086 does not support popa!            ; Get saved registers from stack

   mov sp, [ds:STACKSTART] ; Restore sp from end of push on NMI/INT 3   
   pop di
   pop si
   pop bp
   pop bx
   pop dx
   pop cx
   pop ax
   pop sp ; This should result in the same sp value before iret as is in STACKSTART

   iret            ;                  rti                        ; Return from interupt, which will load program counter from X value returned from BADDR


;   Prompt with E and accept extra segment address
setes:
   mov ah, segE    ; Display E on right hand data segment 6
   mov [ds:DBUF+5],ah;                   sta      <dbuf+5
   call baddr      ;                     lbsr     BADDR             ; Get 16 bit address in bx and display on segments 1-4
   mov [ds:EXTRA_SEGMENT], bx ; Store desired extra segment
   mov es, bx      ; Change es as well, shouldn't be changed else where other than reset and resume
   jmp resume 

;   Prompt with C and accept code segment address
setcs:
   mov ah, segC    ; Display C on right hand data segment 6
   mov [ds:DBUF+5],ah;                   sta      <dbuf+5
   call baddr      ;                     lbsr     BADDR             ; Get 16 bit address in bx and display on segments 1-4
   mov [ds:GO_CS], bx ; Store desired code segment, used by Go
   jmp resume 

    ; Note es should be set to PIA map D000, ds to 7000 before calling
dispresh:	
    push ax	           ;dispresh             pshs     x,b,a
    push dx            ; dx=x as destination
    push si            ; si=x as source
                       ; ah=a al=b (6809 D=AB)
	  ;   set output ports using IO Ports now, change mov to in or out using dx and al, need to reverse 6809 A & B so now A=al and B=ah
    mov dx, CTRLA	     ;                     ldx      #PORTA            ; This routine uses a different approach from GETKEY using index X and offsets
    xor ax,ax	         ;                     clra                       ; note 6809 offsets changed for RS0=A0 & RS1=A1
    out dx, al      	 ;                     sta      $01,x             ; Set CTRLA Port A to data direction register b2=0 DDR
    mov dx, CTRLB	     ;    
    out dx, al      	 ;                     sta      $03,x             ; Set CTRLB Port B to data direction register
    mov al, 0x7f       ;                     lda      #$7F              ; 
    mov dx, PORTA
    out dx, al	       ;                     sta      ,x                ; Set Port A to output bits 0-6
    mov al, 0x0f       ;                     lda      #$0F              ; Set bits 0-3 for output
    mov dx, PORTB
    out dx, al      	 ;                     sta      $02,x             ; Set Port B to output for bits 0-3
    mov al, 0x04       ;                     lda      #$04              ; Set control register b2=1 Output Register
    mov dx, CTRLA
    out dx, al      	 ;                     sta      $01,x             ; Set CTRLA Port A control register for Port Output, will be keypad switch input
    mov dx, CTRLB
    out dx, al      	 ;                     sta      $03,x             ; Set CTRLB Port B control register for Port Output, will be keypad row
	
    ; initialise loop over digits
    mov si, DBUF	    ;                      ldx      #dbuf             ; Segments are numbered 4..9 to correspond to outputs from the 74LS145/7442 decoder.
    mov ah, 0x03      ;	                     ldb      #$03
disp1:
    inc ah	          ;disp1                 incb                       ; Start at 04
    cmp ah, 0x0c      ;	                     cmpb     #$0C              ; until done 11/0B
    jne disp2 	      ;                      bne      DISP2             ; Display next digit
	
	  ; finished
    pop si
    pop dx	          ;                      puls     a,b,x,pc          ;(pul? pc=rts) Done all 6 segments 04-09
    pop ax	
    ret	

    ; light up the next digit                     ; Changes to reduce Ghosting of 7 Segment displays
disp2:
    mov al, 0x7f      ;disp2                lda      #$7F              ; This turns off all 7 Segments
    mov dx, PORTA
	  out dx, al        ;                     sta      PORTA             ; Turn off current segments before changing PORTB
    ; Moved PORTB code from below as need to copy ah to al for out
    mov dx, PORTB
    mov al, ah
	  out dx, al        ;                     stb      PORTB             ; Select segment on Port B (04-09)            

	  mov al, [ds:si]   ;                     lda      ,x+               ; Get segment value from Display buffer first
    inc si
    not al	          ;                     coma                       ; Complement 7 Segment values as 1 turns off segment
    mov dx, PORTA
	  out dx, al        ;                     sta      PORTA               ; Save segment bits to Port A
	
	  ;   delay loop
    mov al, 0xa0	    ;                     lda      #$A0              ; Delay loop for 160 iterations
disp3:
    dec al            ;disp3                deca                       
    jne disp3	        ;                     bne      DISP3             ; Delay until A is zero
    jmp disp1	        ;                     bra      DISP1             ; Process next segment


; Key codes
;  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  L SP CN G  M  R  I
; 22 24 02 12 14 00 10 04 01 11 03 13 23 33 21 20 05 15 25 35 31 30 32

; Now reverses ah and al due to OUT and IN Uses a=al, b=ah returns key code in ah

getkey:
    push bx           ;GETKEY               pshs     y,dp,b
    push dx           ;                     lda      #dpPIA            ; Set Direct Page register to PIA page ($40), PIA code uses Direct addressing 
                      ;                     tfr      a,dp

;   Set up I/O port for Port A 0-7 input and Port B 0-3 output, for IO Ports need to swap use of al and ah
getkey1:
    call dispresh     ;GETKEY1              bsr      DISPRESH          ; Refresh 7 Segment display while waiting for key
    xor ax, ax        ;                     clra     
    mov dx, CTRLA
    out dx, al        ;                     sta      <CTRLA            ; Set Port A to data direction register b2=0 DDR
    mov dx, CTRLB
    out dx, al        ;                     sta      <CTRLB            ; Set Port B to data direction register
    mov dx, PORTA
    out dx, al        ;                     sta      <PORTA            ; Set Port A to input bits 0-7
    mov al, 0x0f      ;                     lda      #$0F              ; set bits 0-3 for output
    mov dx, PORTB
    out dx, al        ;                     sta      <PORTB            ; Set Port B to output for bits 0-3
    mov al, 0x04      ;                     lda      #$04              ; Set control register b2=1 Output Register
    mov dx, CTRLA
    out dx, al        ;                     sta      <CTRLA            ; Set Port A control register for Port Output, will be keypad switch input
    mov dx, CTRLB
    out dx, al        ;                     sta      <CTRLB            ; Set Port B control register for Port Output, will be keypad row
    mov ah, 0xff      ;                     lda(b)      #$FF           ; *** This should probably set ldb to $FF, bug in original Monitor Code
getkey2:              ; B register is used to strobe keypad output PB0, PB1 to drive 74LS145 decoder (7442 in original)
    inc ah            ;GETKEY2              incb                       ; Read next keypad row (row will change to 0, key pressed changes from 1 to 0)
    cmp ah,0x04       ;                     cmpb     #$04              ; Done rows 0,1,2,3?
    je getkey1        ;                     beq      GETKEY1           ; Scanned all 4 keypad lines, update display and start again
    mov dx, PORTB
    mov al, ah
    out dx, al        ;                     stb      <PORTB            ; Output current keypad row to Port B, selected row output will be set to 0 (default to 1)
    mov dx, PORTA
    in al, dx         ;                     lda      <PORTA            ; Read keypad keys from Port A (note Port A is pulled high, pressed key sets bit to 0)
    not al            ;                     coma                       ; Complement keypad input so pressed key is indicated by 1
    cmp al, 0x00
    je getkey2        ;                     beq      GETKEY2           ; If all zero, no key pressed, try next keypad row

;   Found a key: decode it, use bh for tmp3 (PA0-5) and bl for tmp2 (Q0-3)
    mov bh, ah        ;                     stb      tmp2              ; Save the keypad row in B to tmp2 (better name ****)
    mov bl, al        ;                     sta      tmp3              ; Save input key to tmp3 (was chksum which maps to same address $13ed)
    xor al, al        ;                     clra                       ; A maintains the bit count of which bit is set
    mov ah, 0x01      ;                     ldb      #$01              ; Set b0 as test bit
getkey3:
    cmp ah,bl         ;GETKEY3              cmpb     tmp3              ; Is bit A set?, what happens if two keys pressed, just the lowest?
    je getkey4        ;                     beq      GETKEY4           ; If bit is set calculate key code GETKEY4
    inc al            ;                     inca                       ; Next bit count                       
    shl ah,0x01       ;                     aslb                       ; Rotate bit to next positon, fill b0 with 0, when all 0 set Z (eq)
    cmp ah,0x00
    je getkey2        ;                     beq      GETKEY2           ; When done 0-7 then process next keypad row
    jmp getkey3       ;                     bra      GETKEY3           ; Check next bit

getkey4:
    mov ah,bh         ;GETKEY4              ldb      tmp2              ; Get the keypad row (Q0-3)
    shl ah,0x01       ;                     aslb                       ; Shift to the top nibble
    shl ah,0x01       ;                     aslb     
    shl ah,0x01       ;                     aslb     
    shl ah,0x01       ;                     aslb     
    add ah,al         ; We want to return key in ah after pop                     pshs     b                 ; Add B to A via stack
                      ;                     adda     ,s+               ; Add value of B on stack to A to calculate keycode

;   Wait for the key to be released
    push ax           ;                     pshs     a                 ; Save keycode
                      ; Use bx for Y and B
    mov bx, 0x0800    ;                     ldy      #$08              ; Keybounce delay will be 8 (Y) x 256 (B), 2048 loops
getkey5:
                      ;GETKEY5              clrb                       ; Initialise B as inner loop counter
getkey6:
    mov dx, PORTA                      
    in al, dx         ;GETKEY6              lda      <PORTA            ; Check port to see if key still pressed (bit=0 key pressed)
    not al            ;                     coma                       ; Complement A, non zero (NE) means a key still held down
    cmp al,0x00
    jne getkey5       ;                     bne      GETKEY5           ; If key still pressed, zero count and check again 
    dec bx            ;                     decb                       ; Key released, decrement count
    jne getkey6       ;                     bne      GETKEY6           ; If still not zero check port again (this will de-bounce keys)
                      ;                     leay     -$01,y            ; Counted down B to 0 from 255, decrement Y
                      ;                     bne      GETKEY6           ; Check port again, until Y is zero
    pop ax            ;                     puls     a,b,dp,y,pc       ; (pul? pc=rts) Keycode returned in A 
    pop dx
    pop bx
    ret


; 7CB5 BADDR Build 4 digit hex address from keyboard, refreshing display and return address in bx register
baddr:
    push ax           ;BADDR                pshs     b,a               ; Preserve A and B
    push di
    xor ax,ax         ;                     clra                       ; Clears D (A+B)
                      ;                     clrb                       ;
    mov di, DBUF      
    mov [ds:di],ax    ;                     std      dbuf              ; Clear left 4 digits in display buffer
    mov [ds:di+2],ax  ;                     std      dbuf+2           
                      ;                     ldx      #dbuf             ; X is left hand digit of display buffer
    call hexin        ;                     bsr      HEXIN             ; Get two Hexadecimal keys, and display at di, returns value in ah
    mov bh,ah         ;                     pshs     a                 ; Save most significant byte on stack
    call hexin        ;                     bsr      HEXIN             ; Get two Hexadecimal keys, and display at X, returns in A
                      ;                     puls     b                 ; Now get most significant byte from stack (now in bh)
                      ;                     bra      BADDR1            ; B=MSB, A=LSB which is wrong way around for D
                      ; Assume this code is before BADDR to keep the subroutine addresses consistent with 6802 code
    mov bl,ah         ;BADDR1               exg      a,b               ; Swap A and B
    ; bx now has MSB and LSB for return     tfr      d,x               ; Return address in X
    pop di            ;                     puls     a,b,pc ;(pul? pc=rts)
    pop ax
    ret
    
; 7CCC HEXIN Use KEYHEX to accept two hex key entries and combines two hex digits in one byte in A and updates display at X and X+1
hexin:
    call keyhex     ;HEXIN                bsr      KEYHEX            ; Get most significant byte (left hand segment)
    push bx
    shl ah,0x01     ;                     asla                       ; Shift hex digit value to upper nibble
    shl ah,0x01     ;                     asla     
    shl ah,0x01     ;                     asla     
    shl ah,0x01     ;                     asla     
    mov bh, ah      ;                     pshs     a                 ; Save for return value
    call l7seg      ;                     bsr      L7SEG             ; Convert MSB (Left hand segment) to segment value
    mov [ds:di], ah ;                     sta      ,x+               ; Display segment at X and increment to next segment
    inc di
    call keyhex     ;                     bsr      KEYHEX            ; Get least significant byte
    add bh, ah      ;                     adda     ,s+               ; Add to saved most significant byte and remove from stack
    ; bh has sum                          pshs     a                 ; Save sum of MSB and LSB
    call r7seg      ;                     bsr      R7SEG             ; Convert LSB (right hand segment) to segment
    mov [ds:di], ah ;                     sta      ,x+               ; Display segment at X and increment to next segment
    inc di
    mov ah, bh      ;(pul? pc=rts) return saved Hex two digit value in A 
    pop bx
    ret
    
; 7CE4 KEYHEX Combines GETKEY and HEXCON
; Gets a single key digit 0-F, converts to 7 Segment, return to monitor if invalid key
keyhex:
    call getkey       ;KEYHEX               lbsr     GETKEY            ; Get a single key

;   Fall-thru
; 7CE7 HEXCON converts key code in ah into Hex equivalent for the key and returns in ah, al is not preserved. 
; If non Hex command key entered returns to monitor
hexcon:
    push bx          ;HEXCON                pshs     x,b
    mov bx,KEYCODE-1 ;                      ldx      #KEYCODE          ; Lookup key in KEYCODE table
    mov al, 0xff     ;                      ldb      #$FF
hexcon1:
    inc al           ;HEXCON1               incb                       ; B will have hex value of KEYCODE
    inc bx
    cmp al,0x10      ;                      cmpx     #SVNSEG           ; Check if X is at end of KEYCODE table (or al < 0x10)
    je resume        ;                      beq      TORESUME2         ; If it is at end without finding key, the resume to monitor **** (may not need toresume2 perhaps to avoid lbeq)
    cmp ah, [cs:bx]  ;                      cmpa     ,x+               ; Compare KEYCODE at X with current key value
    jne hexcon1      ;                      bne      HEXCON1           ; If no match then move to next KEYCODE value
    mov ah,al        ;                      tfr      b,a               ; Hex digit value returned in A
    pop bx           ;                      puls     b,x,pc ;(pul? pc=rts)
    ret 
    
; 7CFF L7SEG converts left hex digit of ah into 7 segment code for display and returns in ah, ax/al is not preserved
l7seg:
    ; we could use cl=4
    shr ah,0x01       ;L7SEG                asra                       ; Shift MSB right 4 bits so it is LSB
    shr ah,0x01       ;                     asra     
    shr ah,0x01       ;                     asra     
    shr ah,0x01       ;                     asra     

; 7D03 R7SEG converts right hex digit of ah into 7 segment code for display and returns in ah, ax/al is not preserved
;   Convert low-order 4 bits of ah to 7seg
r7seg:
    push si           ;R7SEG                pshs     x                 ; Save X, used as index to lookup 7 segment values
    push bx
    mov bx, SVNSEG    ;                     ldx      #SVNSEG           ; X is 7 segment lookup table start
    and ah, 0x0f      ;                     anda     #$0F              ; mask MSB
    mov al, ah
    xor ah, ah
    mov si, ax        ;R71                  beq      R72               ; A=0 we have found value already
                      ;                     leax     $01,x             ; Increment X
                      ;                     deca                       ; Decrease A (note that leax A,X would avoid the need for the loop)
                      ;                     bra      R71               ; Check next value
    mov ah, [cs:bx+si];R72                  lda      ,x                ; Get 7 segment value for A
    pop bx            ;puls     x,pc              ;(pul? pc=rts) return A and restore X 
    pop si
    ret

; 7D15 SVNHEX Converts 7 segment code in ah to a hex value returned in ah, default to monitor if code not hex, , ax/al is not preserved
svnhex:
    push bx          ;SVNHEX               pshs     x,b
    mov bx, SVNSEG-1 ;                     ldx      #SVNSEG           ; Lookup value in A in SVNSEG table
    mov al, ah       ;
    xor ah, ah       ;                     clra                       ; Initialise A
svnhex1:
    inc bx
    cmp al,  [cs:bx] ;SVNHEX1              cmpb     ,x+               ; Does 7 segment value match?
    je svnhex2       ;                     beq      SVNHEX2           ; If match return value in A
    inc ah           ;                     inca                       ; Next segment value
    jmp svnhex1       ;                     bra      SVNHEX1           ; Lookup next code value in SVNSEG 
svnhex2:
    pop bx           ;SVNHEX2              puls     b,x,pc ;(pul? pc=rts)
    ret

;   Prompt for address with 'M' and display memory contents
;   Changed memory access statements to use es rather than ds as getkey, dispresh etc need ds set to 7000 too
memdisp:
    xor ax,ax         ;MEMDISP              clra                       ;
    mov [ds:DBUF+4],ah;                     sta      <dbuf+4           ; Clear Left Hand Data segment (digit 5)
    mov ah, segM      ;                     lda      #segM             ; Display M in right hand data segment (digit 6)
    mov [ds:DBUF+5],ah;                     sta      <dbuf+5
    call baddr        ;                     bsr      BADDR             ; Gets 4 digit address then displays in Digits 1-4, then returns address in bx
mem0:
    mov ah, [es:bx]   ;es should be been set by reset, resume or setes ;MEM0                 lda      ,x                ; Get current memory value at X
    mov dh, ah        ; Make a copy so we don't use SVNHEX!
    push ax           ;                     pshs     a                 ; Save memory value
    call l7seg        ;                     bsr      L7SEG             ; Convert Most Significant Byte / Left Hand digit to segment
    mov [ds:DBUF+4],ah;                     sta      <dbuf+4           ; Display MSB on left data segment
    pop ax            ;                     puls     a                 ; Get saved memory value
    call r7seg        ;                     bsr      R7SEG             ; Convert Least Significant Byte / Right hand digit to segment
    mov [ds:DBUF+5],ah;                     sta      <dbuf+5           ; Display LSB on right data segment

;   Check for I key
mem1:
    call getkey       ;MEM1                 lbsr     GETKEY            ; Check for key press
    cmp ah, keyI      ;                     cmpa     #keyI             ;  
    je mem2           ;                     beq      MEM2              ; If I pressed then save and then get next memory value
    call hexcon       ;                     bsr      HEXCON            ; Not I so convert key to hex value in A, returns to monitor if not 0-F
    and dh, 0x0F      ; mask top nibble
    shl dh,0x01       ; Move lower nibble to upper 
    shl dh,0x01       ;  
    shl dh,0x01       ;  
    shl dh,0x01       ;  
    add dh, ah        ; Add the new digit, when saving we can use dh
    mov al,[ds:DBUF+5];                     ldb      <dbuf+5           ; Move LSB to MSB (move right to left data segment)
    mov [ds:DBUF+4],al;                     stb      <dbuf+4
    call r7seg        ;                     bsr      R7SEG             ; Convert A LSB / right hand digit into 7 segment value
    mov [ds:DBUF+5],ah;                     sta      <dbuf+5           ; Display LSB in right hand data segment
    jmp mem1          ;                     bra      MEM1              ; Get next key (hex values will continue to rotate LSB to MSB until I pressed or Abort


;   Store and increment
mem2:
    ; Now dh includes the new contents, no need to use SVNHEX - was using jp NOT jmp!!
    ;mov ah,[ds:DBUF+4];MEM2                 lda      <dbuf+4           ; Get MSB 7 Segment value
    ;call svnhex       ;                     bsr      SVNHEX            ; Convert A from 7 segment to Hex MSB value
    ;shl ah,0x01       ;                     asla                       ; Shift left 4 bits to become MSB nibble     
    ;shl ah,0x01       ;                     asla     
    ;shl ah,0x01       ;                     asla     
    ;shl ah,0x01       ;                     asla     
    ;mov dl, ah        ;                     pshs     a                 ; Save MSB value
    ;mov ah,[ds:DBUF+5];                     lda      <dbuf+5           ; Get LSB 7 segment value
    ;call svnhex       ;                     bsr      SVNHEX            ; Convert A from 7 segment to Hex LSB value
    ;add ah, dl        ;                     adda     ,s+               ; Add LSB to MSB nibble
    ;mov dl, ah        ;                     tfr      a,b               ; Save A in B
    mov [es:bx], dh   ;                      sta      ,x                ; Store new value in A at X
    mov ah, [es:bx]   ;                      lda      ,x+               ; Re-read memory value from X then increment X for next byte
; This won't handle next segment, will wrap around to start of segment
    inc bx            ;                     pshs     b                 ; Push new value onto stack
    cmp ah, dh        ;                     cmpa     ,s+               ; Compare new value and current value at X
    ;int3 debug new code
    jne resume        ;                     bne      RESUME            ; If not equal then tried to update invalid RAM (eg ROM etc), return to monitor
    mov ax, bx        ;                     tfr      x,d               ; X is now next address, copy to D (A=MSB, B=LSB)
    call l7seg        ;                     bsr      L7SEG             ; Convert MSB left digit to 7 segment code
    mov [ds:DBUF+0],ah;                     sta      <dbuf+0           ; display MSB left digit
    mov ax, bx        ;                     tfr      x,d               ; Copy X to D again
    call r7seg        ;                     bsr      R7SEG             ; Convert MSB right digit to 7 segment code
    mov [ds:DBUF+1],ah;                     sta      <dbuf+1           ; display MSB right digit
    mov ax, bx        ;                     tfr      b,a               ; Copy LSB to A
    mov ah, al        ; r7seg/l7seg does not preserve al
    call l7seg        ;                                        bsr      L7SEG             ; Convert LSB left digit to 7 segment code
    mov [ds:DBUF+2],ah;                     sta      <dbuf+2           ; display LSB left digit
    mov ax, bx        ;                     tfr      b,a               ; Copy B to A again
    mov ah, al        ; r7seg/l7seg does not preserve al
    call r7seg        ;                     lbsr     R7SEG             ; Convert LSB right digit to 7 segment code
    mov [ds:DBUF+3],ah;                     sta      <dbuf+3           ; display LSB right digit
    jmp mem0          ;                 bra      MEM0              ; Get memory value from new X address now on display




;   Clear display
cleardisp:
    push ax          ;CLEARDISP            pshs     x,b,a
    push di
    xor ax, ax       ;                     clra                       ; Blank segment value
    mov al, 0x08     ;                     ldb      #$06              ; Iterate over 6 digits 
    mov di, DBUF     ;                     ldx      #dbuf 
cleardisp1:
   mov [ds:di], ah   ;CLEARDISP1           sta      ,x+               ; Clear display buffer
   inc di
   dec al            ;                     decb                       ; Next segment (1-6)
   jne cleardisp1    ;                     bne      CLEARDISP1        ; Next segment
   pop di            ;                     puls     a,b,x,pc ;(pul? pc=rts)
   pop ax
   ret

;R  Register Display register values via the pushed values on the stack (automatic after INT3 vector at [0000:000C] IP:CS)
;            Right digits show 
;            	FL Flags register
;            	CS Code Segment
;            	IP Instruction Pointer
;             AX AX register CX, DX, BX, (SP), BP, SI, DI
;            	CX CX Register
;             DX DX Register
;             BX BX Register
;             (SP SP Register exclude)
;             BP BP Register
;             SI SI Register
;             DI DI Register
;            Press I between values
;            After DI displayed returns to Monitor, AB to Abort to monitor

;   Characters for Register Display  FL CS IP AX CX DX BX SP BP SI DI
;   On INT 3, 8088 Pushes Flags on Stack with CS and IP
;   Need PUSHA to have most state (no DS, SS or ES),  AX, CX, DX, BX, SP, BP, SI, DI. SP+6 will be value before INT
REGDISPCHAR          db segF,segL, segC,segS, segI,segP, segS,segP, segA,segX
                     db segC,segX, segD,segX, segB,segX 
                     db segB,segP, segS,segI, segD,segI

regdispswi:
    ; Save the remaining registers to the current stack
    ; pusha does not exist on 8088/8086!
    push sp
    push ax
    push cx
    push dx
    push bx
    push bp
    push si
    push di   

    mov [ds:STACKSTART],sp; REGDISPSWI           lda      #dpRam             ; Initialise direct page register
                          ;                      tfr      a,dp
                          ;                      sts      <STACKSTART        ; Save stackpointer from running code on SWI
    ; mov sp, MONSTACK this is done in Resume     ;                      lds      #MONSTACK          ; Set stackpointer to monitor default  
regdisp:
    call cleardisp        ; REGDISP              bsr      CLEARDISP          ; Clear display
    mov ax, [ds:STACKSTART]
    add ax, 0x15          ; 10 Registers 22 dec bytes 0x15 (including 0) 
    mov si,ax             ;                      ldx      <STACKSTART        ; Get running stack pointer from saved value into X

;   Read double characters from REGDISPCHAR
    mov bx,REGDISPCHAR  ;                      leay     -$1B,pc            ; Loads Y with REGDISPCHAR register table start address ($7DEB) 
regdisp1:
    mov ah, [cs:bx]       ;REGDISP1             lda      ,y+                ; Get register prefix character
    mov [ds:DBUF+6], ah   ;                     sta      <dbuf+5            ; Display at segment 6
    inc bx
    mov ah, [cs:bx]       ;                     
    mov [ds:DBUF+7], ah   ;                     
    inc bx
                          ;                     cmpy     #REGDISPSWI        ; Are we on the last register DI?       
                          ;                     ldx      #STACKSTART
                          ;SHOWLEFT             cmpy     #REGDISPCHAR2      ; Are we on single byte values still? X is first 2 byte
                          ;                     bls      SHOWBYTE           ; Display single byte value
    mov ah,[ss:si]        ; This will be LSB    lda      ,x                 ; Get most significant byte of two byte value
    call l7seg            ;                     lbsr     L7SEG              ; Get left hand digit segment code
    mov [ds:DBUF+0], ah   ;                     sta      <dbuf+0            ; Display at segment 1
    mov ah,[ss:si]        ;                     lda      ,x+                ; Get same byte for register and increment X
    call r7seg            ;                     lbsr     R7SEG              ; Get right hand digit segment code
    mov [ds:DBUF+1], ah   ;                     sta      <dbuf+1            ; Display at segment 2
    dec si
    mov ah,[ss:si]        ;    SHOWBYTE         lda      ,x                 ; Get byte value for register from stack
    call l7seg            ;                     lbsr     L7SEG              ; Get left hand segment code
    mov [ds:DBUF+2], ah   ;                     sta      <dbuf+2            ; Display at segment 3
    mov ah,[ss:si]        ;                     lda      ,x+                ; Get same byte value for register and increment X
    dec si
    call r7seg            ;                     lbsr     R7SEG              ; Get right hand segment code
    mov [ds:DBUF+3], ah   ;                     sta      <dbuf+3            ; Display at segment 4
    call getkey           ;                     lbsr     GETKEY             ; Get a key
    cmp ah, keyI          ;                     cmpa     #keyI              ; Has I been pressed
    jne resume            ;                     lbne     RESUME             ; No then return to monitor 
    cmp bx, regdispswi    ;                     cmpy     #REGDISPSWI        ; Are we at end of register table?       
    jne regdisp1          ;                     bne      REGDISP1           ; No so display next register
    jmp resume            ;                     lbra     RESUME             ; Yes so return to monitor


;   Set up serial port, now uses IO Ports not memory mapped IO, need to use al for A and ah for B
load:
    mov al, CTRLRESET      ;LOAD                 lda      #CTRLRESET  ; Master Reset bits
    mov dx, SERIALCTRL
    out dx, al             ;                     sta      SERIALCTRL  ; Serial Port Control Master Reset
    mov al, CTRLDIVIDE64 | CTRLWORD8N1S | CTRLRTSLOW ;
;    mov al, CTRLDIVIDE16 | CTRLWORD8N1S | CTRLRTSLOW ;
    out dx, al             ;                     sta      SERIALCTRL  ; Serial Port Divide by 64 (3.6864Mhz Xtal /64 =57,600 ), 8 Bits No Parity 1 Stop, /RTS Low - receive data, DCD wired low

reload:
    call recvbyte           ;RELOAD               bsr      RECVBYTE    ; Get a byte without Hex conversion, should be S1 or S9 combination

;   Wait for $53 = S
    cmp al, asciiS         ;                     cmpa     #asciiS    
    jne reload             ;                     bne      RELOAD      ; Not S then get next byte
    call recvbyte          ;                     bsr      RECVBYTE    ; We have S so get next byte, should be S record type 1 or 9

;   Code S1 begins a record
    cmp al, ascii1         ;                     cmpa     #ascii1    
    je load2               ;                     beq      LOAD2       ; Parse the bytecount and start address, the data and checksum

;   Code S9 means EOF
    cmp al, ascii9         ;                     cmpa     #ascii9   
    jne reload             ;                     bne      RELOAD      ; If not S1 or S9 keep reading otherwise display F for finished

;   Show F for Finished in display forever
messagef:
    mov al, segF           ;MESSAGEF             lda      #segF
    jmp errstop            ;                     bra      ERRSTOP


;   Get byte count and initialise checksum, use cl for byte count and ch for checksum once calculated in ah, tmp=bx, x=di
load2:
    call recvhexbyte       ; LOAD2                bsr      RECVHEXBYTE ; Read the Byte Count for Srec format (note NOT Hex character count)
    mov cl, ah             ;                      stb      bytecount (cl) 
    mov ch, ah             ;                      lda      bytecount  ; Initialise checksum in A with byte count for new record 
    dec cl                 ;                      dec      bytecount (cl)

;   Get two-byte address where to load bytes, accumulating checksum in A
    call recvhexbyte       ;                      bsr      RECVHEXBYTE  ; Get high address byte in B
    mov bh, ah             ;                      stb      tmpX (bh)  
                           ;                      pshs     b
    add ch, ah             ;                      adda     ,s+          ; add high address to checksum in A
    dec cl                 ;                      dec      bytecount   ; reduce byte count by 1

    call recvhexbyte       ;                      bsr      RECVHEXBYTE  ; Get low address byte in B
    mov bl, ah             ;                      stb      tmpX+1 (bl)     ; Save low address byte in tmpX
                           ;                      pshs     b
    add ch, ah             ;                      adda     ,s+          ; add low address to checksum in A
    mov di, bx             ;                      ldx      tmpX        ; Load address in X for use row data bytes storage  
ldloop:
    dec cl                 ;LDLOOP                dec      bytecount (cl)  ; reduce byte count by 1

    je lddone              ;                      beq      LDDONE       ; No more bytes? just checksum left
    call recvhexbyte       ;                      bsr      RECVHEXBYTE  ; Get next hex characters and convert to bytes in B
    ; Now uses extra segment for load
    mov [es:di], ah        ;                      stb      ,x+          ; Save to memory at X
    inc di;                ;
                           ;                      pshs     b
    add ch, ah             ;                      adda     ,s+          ; Add B to checksum held in A     
    jmp ldloop             ;                      bra      LDLOOP       ; Next byte


;   Verify checksum
lddone:
    call recvhexbyte       ;LDDONE                bsr      RECVHEXBYTE  ; Get checksum from end of line in B
    not ah                 ;                      coma                  ; Since complement not twos complement (NEG) as S19 format checksum would not work

                           ;                      pshs     b            ; Compare B to checksum (which is complement of sum)
    cmp ah, ch             ;                      cmpa     ,s+          ; to sum of bytes
    je reload              ;                      beq      RELOAD       ; If zero then checksum and complement of sum of bytes are equal, get next row in file
; Display C for Checksum Error
    mov al, segC           ;                      lda      #segC        ; Checksum not equal so display C error and stop

;   Fall-thru
errstop:
    call cleardisp         ;ERRSTOP              lbsr     CLEARDISP    ; Clear display then show charcters in A (F Finished, E error in Hex, C Checksum error)
    mov [ds:DBUF], al      ;                     sta      dbuf+0  
dead:
    call dispresh          ;DEAD                 lbsr     DISPRESH
    jmp dead               ;                     bra      DEAD

;   Receive one byte from serial port in A, now returns in al
recvbyte:
                           ;RECVBYTE             lda     #STATUSRDRF   ; Mask Receive Data Register Full
recvbyte1:
    mov dx, SERIALSTATUS
    in al, dx              ;RECVBYTE1        bita    SERIALSTATUS  ; Check RDRF, 1 means has data, 0 loop back
    test al, STATUSRDRF
    ;int3
    jz recvbyte1            ;                    beq     RECVBYTE1     ; Keep checking until read for more data
    mov dx, SERIALDATA      ;
    in al, dx               ;                    lda     SERIALDATA    ; Read data from serial port, should clear RDRF
    ret                     ;                    rts

;   Receive two ASCII characters, convert to a byte and return in B (now ah for IO changes), invalid Hex display E and stop 
recvhexbyte:
    push bx                 ; RECVHEXBYTE        pshs    a             ; Save A to stack as will have checksum
    push ax ; We need to return B/al so use bl 
    call recvbyte           ;                    bsr     RECVBYTE
    call hextobin           ;                    bsr     HEXTOBIN      ; Convert ASCII Hex to Low Nibble
    shl al,0x01             ;                    lsla                  ; Shift to high nibble
    shl al,0x01             ;                    lsla
    shl al,0x01             ;                    lsla
    shl al,0x01             ;                    lsla
    mov ah, al              ;                    tfr     a,b           ; Save high nibble to B
    call recvbyte           ;                    bsr     RECVBYTE
    call hextobin           ;                    bsr     HEXTOBIN      ; Convert ASCII Hex to Low Nibble
    add ah, al              ;                    addb    ,s+               
    mov bh, ah              ;
    pop ax                  ;    
    mov ah, bh              ;                    puls    a,pc           ; B will have returned two chracter Hex Byte value
    pop bx                  ;
    ret
    
; Take ASCII character in A (now al with IO changes) and convert to 4 bit Low nibble in A
hextobin:
    
    cmp al, ascii0           ;HEXTOBIN            cmpa     #'0          ; Compare with ASCII 0
    js hexerr                ;                    bmi      HEXERR       ; Less than 0 then error
    cmp al, ascii9           ;                    cmpa     #'9          ; Compare with ASCII 9
    jbe hexrts               ;                    ble      HEXRTS       ; Less than or equal then numeric, return value
    cmp al, asciiA           ;                    cmpa     #'A          ; Compare with ASCII A
    js hexerr                ;                    bmi      HEXERR       ; Less then A then error
    cmp al, asciiF           ;                    cmpa     #'F          ; Compare with ASCII F
    jg hexerr                ;                    bgt      HEXERR       ; Greater than F then error
    sub al, 0x07             ;                    suba     #7           ; A=$41/65 becomes $3A/58 F=$46/70 becomes $3F/63
hexrts:
    and al, 0x0f             ;HEXRTS              anda     #$0F         ; Mask high nibble to convert from ASCII to binary 4 low bits      
    ret                      ;                    rts

hexerr:
; Display E for Error in Hexadecimal conversion 
   mov al, segE             ;HEXERR               lda      #segE         ; 7 Segment E
   jmp errstop              ;                     bra      ERRSTOP      ; Will display E - error as invalid Hex character


;   Save memory to serial port Prompt S and get start address, ptr SAVE_START, addr SAVE_FINISH 6809 A now = al, B=ah IO Port changes
save:
    xor ax, ax
    xor cx, cx              ; use cl for checksum as dl now used for IO Port as par of dx!
    mov al, segS            ;SAVE                 ldx      #segS
    mov [ds:DBUF+4], al     ;                     stx      <dbuf+4 Space & S
    call baddr              ;                     lbsr     BADDR
    mov [ds:SAVE_START], bx ;                     stx      <ptr
    ;int3
;   Prompt F and get finish address
    mov al, segF            ;                     lda      #segF
    mov [ds:DBUF+4], al     ;                     sta      <dbuf+5
    call baddr              ;                     lbsr     BADDR
    mov [ds:SAVE_FINISH], bx;                     stx      <addr
    ;int3
;   Set up serial output port - RESET should go in main reset code
    mov al, CTRLRESET       ;                     lda      #CTRLRESET  ; Master Reset bits
    mov dx, SERIALCTRL
    out dx, al              ;                     sta      SERIALCTRL ; Serial Port Control Master Reset
    mov al, CTRLDIVIDE64 | CTRLWORD8N1S | CTRLRTSLOW; lda      #CTRLDIVIDE64 | CTRLWORD8N1S | CTRLRTSLOW
;    mov al, CTRLDIVIDE16 | CTRLWORD8N1S | CTRLRTSLOW; lda      #CTRLDIVIDE16 | CTRLWORD8N1S | CTRLRTSLOW
    out dx, al              ;                     sta      SERIALCTRL ; Serial Port Divide by 64 (3.6864Mhz Xtal /6 /64 =9600 ), 8 Bits No Parity 1 Stop, /RTS Low - send data

save1:                      ;SAVE1 
;   Send block start
save4:
    call sendstart          ;SAVE4                lbsr     SENDSTART
    mov al, 0xFF            ;                     lda      #$FF          ; This is effectively -1
    mov ah, [ds:SAVE_START] ;                     ldb      <ptr+1
                             ;                    pshs     b
    sub al, ah               ;                    suba     ,s+           ; Subtract -1 from b gives +1 - not sure why do it this way 

;   Send length code and init checksum, dh for checksum
save2:
    and al, 0x0f             ;SAVE2               anda     #$0F          ; Do maximum of 16 bytes (0-F), but need to allow for data byte 0 in length calculation
    add al, 0x04             ;                    adda     #$04          ; Length+2, Checksum+1 data0+1 total 4
    ;int3
    call sendhexa            ;                    lbsr     SENDHEXA      ; Send the length in Hex
    mov cl, al               ;                    sta      <chksum       ; Start the checksum with the length value
    ;int3
;   Send (half of) address and add to checksum
    mov bx, SAVE_START+1     ;                    ldx      #ptr          ; Get most significant byte of start address
    ; sendbytex loads al from [bx]
    ;int3
    call sendbytex           ;                    lbsr     SENDBYTEX     ; Send MSB as Hex
    add cl, al               ;                    adda     <chksum       ; Add byte value to checksum
                             ;                    sta      <chksum 

;   Send other half of address and add to checksum
    mov bx, SAVE_START  ; LSB in little endian leax     $01,x         ; Get the least significant byte of the start address
    ;int3
    call sendbytex           ;                    lbsr     SENDBYTEX     ; Send LSB as Hex
    add cl, al               ;                    adda     <chksum       ; Add byte value to checksum 
                             ;                    sta      <chksum 
    mov bx, [ds:SAVE_START]  ;                    ldx      <ptr

;   Send a data byte and add to checksum
save3:
    call sendbytex           ;SAVE3                bsr      SENDBYTEX     ; Send the byte pointed to by X a Hex
    add cl, al               ;                     adda     <chksum       ; Add data byte to checksum
                             ;                     sta      <chksum 
    cmp [ds:SAVE_FINISH], bx ;                     cmpx     <addr
    je endblock              ;                     beq      ENDBLOCK      ; Has X reached the Finish Address
    inc bx                   ;                     leax     $01,x         ; Increment X to the next byte
    mov [ds:SAVE_START], bx  ;                     stx      <ptr          ; Update ptr to current X value
    mov al, [ds:SAVE_START]  ;                     lda      <ptr+1        ; Get the LSB value
    test al, 0x0f            ;                     bita     #$0F          ; Only check the least significant nibble
    jne save3                ;                     bne      SAVE3         ; Keep sending until 0 (we have done xF byte)

;   Send checksum
endblock:
    mov al, cl               ;ENDBLOCK             lda      <chksum       ; Done block of 16 bytes (other than 1st or last block)
    not al                   ;                     coma                   ; Complement checksum (reverse all bits)
    call sendhexa            ;                     bsr      SENDHEXA      ; Send checksum as 2 Hex characters
    mov al, asciiCR          ;                     lda      #asciiCR      ; Send CR LF to end line
    call send                ;                     lbsr     SEND     
    mov al, asciiLF          ;                     lda      #asciiLF    
    call send                ;                     bsr      SEND

;   Test if all finished
    cmp bx, [ds:SAVE_FINISH] ;                     cmpx     <addr         ; Check X against Finish Address
    je sendeof               ;                     beq      SENDEOF       ; Send S9 End of File if X=Finish Address

;   Test if another full block to send
    mov al,[ds:SAVE_START+1] ;                     lda      <ptr          ; Compare current byte MSB with finish MSB
    cmp al,[ds:SAVE_FINISH+1];                     cmpa     <addr
    js save4                 ;                     bmi      SAVE4
    mov ah,[ds:SAVE_START]   ;                     ldb      <ptr+1
    mov al,[ds:SAVE_FINISH]  ;                     lda      <addr+1
    and al, 0xF0             ;                     anda     #$F0
                             ;                     pshs     b
    cmp al, ah               ;                     cmpa     ,s+
    jne save4                ;                     bne      SAVE4

;   Last block may be short
    call sendstart           ;                     bsr      SENDSTART
    mov al, [ds:SAVE_FINISH] ;                     lda      <addr+1
    jmp save2                ;                     bra      SAVE2


;   Send S19 End of file 53 39 30 33 30 30 30 30 42 46 = 'S9030000FC'
sendeof:
    mov si, SENDEOFMSG       ;SENDEOF              ldy      #SENDEOFMSG
    call sendmsg             ;                     bsr      SENDMSG
    jmp messagef             ;                     lbra     MESSAGEF 


;   Send S19 file format start line 'S1'
sendstart:
    mov si, SENDSTARTMSG     ;SENDSTART            ldy      #SENDSTARTMSG
    call sendmsg             ;                     bsr      SENDMSG
    ret                      ;                     rts


;   Transmit byte from (X) and convert from 8 bits to two ASCII HEX characters and send over serial port
sendbytex:
    mov al, [es:bx]             ;SENDBYTEX         lda      ,x       Now uses al=A for IO changes and es for extra segment
; Convert byte in A (al) into Hex as two ASCII characters and send over serial port
sendhexa:
    push ax                     ;                  pshs     a            ; Save byte value as need to return since used for checksum calc
    push ax                     ;                  pshs     a            ; Save again so we can mask top and lower nibbles
    ; Must use al as daa only works with this register
    ; mov al, ah already using al with IO changes
    and al, 0b11110000          ;                  anda     #%11110000   ; Mask High nibble
    shr al,0x01                 ;                  lsra                  ; Shift to Low nibble
    shr al,0x01                 ;                  lsra 
    shr al,0x01                 ;                  lsra
    shr al,0x01                 ;                  lsra

    add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal 0-9  $30-$39 A-F $41-$46
    daa                         ;                  daa                    ; DAA on adding $90 sets carry which Makes 0-9 3x and A-F be 4x in adca
    adc al, 0x40                ;                  adca     #$40          ; Add $40 makes 0-9 D0-D9  in Decimal 130-139, A-F A0-A5 +$40 + Carry in decimal 141-146 = $41-$46
    daa                         ;                  daa                    ; Strips 100 from result to give 30-39 and 41-46

    ; mov ah, al now using al for A

    call send                   ;                  bsr      SEND

    pop ax                      ;                  puls     a 
    ; mov al, ah already using al with IO changes
    and al, 0b00001111          ;                  anda     #%00001111    ; Mask Low nibble

    add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal, same as above
    daa                         ;                  daa
    adc al, 0x40                ;                  adca     #$40
    daa                         ;                  daa
    ; mov ah, al now using al for A

    call send                   ;                  bsr      SEND
    pop ax                      ;                  puls     a
    ret                         ;                  rts

;   Transmit byte from A now A=al for IO Changes
send:
    push ax                     ;SEND              pshs    a
    ;mov al, STATUSTDRE          ;                  lda     #STATUSTDRE   ; Mask Transmit Data Register Empty
send1:
    mov dx, SERIALSTATUS
    in al, dx    
    test al, STATUSTDRE         ;SEND1             bita    SERIALSTATUS  ; Check TDRE, 1 means ready for more data, 0 loop back
    je send1                    ;                  beq     SEND1         ; Keep checking until ready for more data
    pop ax                      ;                  puls    a             ; 
    mov dx, SERIALDATA 
    out dx, al                  ;                  sta     SERIALDATA
    ret                         ;                  rts                   ; Return


; Send NULL terminated list of BYTES pointed to by Y (si) to serial port
sendmsg:
    mov al, [cs:si]             ;SENDMSG           lda      ,y+
    inc si
    cmp al, 0x00
    je sendmsg2                 ;                  beq      SENDMSG2
    call send                   ;                  bsr      SEND
    nop                         ;                  nop                 ; Allow SWI to be entered and continue
    jmp sendmsg                 ;                  bra      SENDMSG

sendmsg2:
    ret                         ;SENDMSG2             rts

;   Send S19 Start of data S1                         
SENDSTARTMSG:        db      asciiS, ascii1, asciiNull    

;   Send S19 End of file 53 39 30 33 30 30 30 30 46 43 = 'S9030000FC'
SENDEOFMSG:          db      asciiS, ascii9, ascii0, ascii3, ascii0, ascii0, ascii0, ascii0, asciiF, asciiC, asciiCR, asciiLF, asciiNull    

    
; Keypad keycode lookup table to convert key to nibble values, used by KEYHEX and HEXCON
KEYCODE:             db  key0, key1, key2, key3, key4, key5, key6, key7
                     db  key8, key9, keyA, keyB, keyC, keyD, keyE, keyF 

; Seven Segment display lookup table, used to convert 7 Seg value to nibble and nibble to 7 Seg value
; Used by HEXCON, R7SEG and SVNHEX subroutines
SVNSEG:              db  seg0, seg1, seg2, seg3, seg4, seg5, seg6, seg7
                     db  seg8, seg9, segA, segB, segC, segD, segE, segF

;        setloc	0FF00h			; Make Far functions fairly fixed in memory map
  
; Use this function to avoid hardware interrupts messing up the Nanocomp monitor
far_dispresh:
  call dispresh
  retf
  
far_cleardisp:
  call cleardisp
  retf

far_getkey:
   call getkey
   retf

far_send:
   call send
   retf

; Send byte in al as 2 hex characters to serial port
far_sendbytex:
   call sendbytex
   retf

hw_int_ignore:
	push	ax
	mov	al,20h
	out	pic2_reg0,al	; signal EOI to the slave PIC OCW2
	out	pic1_reg0,al	; signal EOI to the master PIC OCW2
	pop	ax
	iret
   
; Use this function to avoid software interrupts messing up the Nanocomp monitor
sw_int_ignore:
	iret

; BIOS will now call reset rather than cols_start, then use B to boot BIOS
;=========================================================================
; start - at power up or reset execution starts here (F000:FFF0)
;-------------------------------------------------------------------------
;        setloc	0FFF0h			; Power-On Entry Point, macro fills space from last line with FF
;start:
;        jmp     0F000h:reset
;        setloc	0FFFFh			; Pad remainder of ROM
;	      db	0ffh
