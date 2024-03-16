;===========================================================================
; MDAtest3.asm - Nanocomp 8086 MDA Video Controller Test 3 CGA/BIOS
; nasm -f bin -o MDAtest3.bin -l MDAtest3.lst MDAtest3.asm
; nasm -f srec -o MDAtest3.srec -l MDAtest3.lst MDAtest3.asm

; Version          Date            Description
;  1.0             31/1/24         MDATest2 with White, Green & CGA/BIOS
;
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
                      
cpu	8086

; This macro includes the setloc macro to pad code with FF to a given address
%include "macro.inc"


; Serial Port Constants
SERIALCTRL                    EQU  0xE000           ; Use IO Port E000
SERIALSTATUS                  EQU  0xE000           ; Use IO Port E000
SERIALDATA                    EQU  0xE001           ; Use IO Port E001

CTRLRESET                     EQU  0b00000011       ; CR1, CR0 1,1 for Master Reset
CTRLDIVIDE16                  EQU  0b00000001       ; CR1, CR0 0,1 for divide by 16


%define	START		0x0000		; Default Physical Address is 70000 [7000:0000]

%define DATASEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define USERCODESEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define STACKSEG 0x7000   ; Stack Segment is current top of 512K RAM minus 256 bytes for Monitor use
%define MONCODESEG 0xF000 ; Monitor Code Segment

org	START		; Use only upper 32 KiB of ROM

;   Reset Handler
teststart:
CRTCAR              EQU  0x03b4
CRTCDR              EQU  0x03b5
VRAMSEG             EQU  0x8000
VRAMTOP             EQU  0x0FFF

CRTC_DAC_ADD_WR     EQU  0x03b0
CRTC_DAC_COL_VALUE  EQU  0x03b1
CRTC_DAC_PIXEL_MASK EQU  0x03b2
CRTC_DAC_ADD_RD     EQU  0x03b3

ASCII0              EQU  0x30
ASCIISPACE          EQU  0x20
COLSPERROW          EQU  0x50
ROWSPERPAGE         EQU  0x19
ROWSPERPAGEBCD      EQU  25
BYTESPERCHAR        EQU  02
CHARATTRIB          EQU  0b00001111 ; Black background, White/Green foreground, 

EOS                 EQU  0x00 ;      end of string

                    ORG  0x0000
                    
CRTStart:           mov ax, VRAMSEG
                    mov es, ax     ; Set es to MDA frame buffer 80000 [8000:0000]
                    xor ax, ax     ; A=al, B=ah
                    mov si, CRTCTAB 
                    
CRTCLOOP:           mov al, ah     ;stb      CRTCAR      
                    mov dx, CRTCAR
                    out dx, al     ; Save Rn in CRTC Address Register
                    mov al, [cs:si]; lda      ,X+        ; Get Rn value from CRTC Table single + for +1, ++ is +2 and wrong!
                    mov dx, CRTCDR
                    out dx, al     ;  Save Rn data value in CRTC Data Register
                    inc si 
                    inc ah 
                    cmp ah, 0x10   ; Have we processed all register values                 
                    jne CRTCLOOP
;  Colour for Monochrome Green R, G, B
; 00  0x0, 0x0, 0x0  Black/Background
; 01  0x0, 0x2A, 0x0 Green Foreground 66%
; 10  0x0, 0x15, 0x0 Bright Background 33%
; 11  0x0, 0x3f, 0x0 Bright Green Foreground 100% 0-63d, 6 bits per colour

;  Colour for Monochrome White R, G, B
; 00  0x0, 0x0, 0x0  Black/Background
; 01  0x2A, 0x2A, 0x2A White Foreground 66%
; 10  0x15, 0x15, 0x15 Bright Background 33%
; 11  0x3f, 0x3f, 0x3f Bright White Foreground 100% 0-63d, 6 bits per colour

                  mov dx, CRTC_DAC_PIXEL_MASK 
                  mov al, 0xFF   
                  out dx, al                 ; Pixel mask set so disabled

                  xor bx, bx
                  mov si, PALETTE
                  
                  mov cx, 24                 ; 24 Palette entries 16 CGA, 4 MDA Green, 4 MDA White
NEXTPALETTE:      mov dx, CRTC_DAC_ADD_WR 
                  mov al, bl                  
                  out dx, al                 ; Output Palette address No.
                  mov dx, CRTC_DAC_COL_VALUE 
                  mov al, [cs:si]
                  inc si
                  out dx, al                 ; Output Red 
                  mov al, [cs:si]
                  inc si
                  out dx, al                 ; Output Green 
                  mov al, [cs:si]
                  inc si
                  out dx, al                 ; Output Blue 
                  inc bl
                  LOOP NEXTPALETTE
                  
                  int3
                  
;CLRSCREEN:          xor di, di     ;ldx      #VRAM
;                    mov al, ASCIISPACE ;lda      #ASCIISPACE
;                    mov ah, CHARATTRIB ;ldb      #CHARCOLOUR
;CLEARSCREEN1        stb      ,X+
;                    sta      ,X+         ; Could Use D and X++
;                    cmpx     #VRAMTOP
;                    ble      CLEARSCREEN1

CRTCTAB             db      0x63         ; R0 H 62 to 64 Total 99
                    db      0x50         ; R1 H Displayed 80
                    db      0x53         ; R2 H from x53 to x55 Sync Position 83
                    db      0x0C         ; R3 H Sync Width 12
                    db      0x1B         ; R4 V Total 27
                    db      0x03         ; R5 V Total Adjust (was 13/$0D)
                    db      0x19         ; R6 V Displayed 25
                    db      0x1A         ; R7 V Sync Position 26
                    db      0x00         ; R8 Interlace mode - Non Interlaced
                    db      0x0F         ; R9 Maximum Scan Line Address 
                    db      0x0d         ; R10 Cursor Start - No Blink + Line 13 Start  Cursor off $0x20
                    db      0x0f         ; R11 Cursor End - No Blink + Line 15 Finish (Blinking coordinated with char blink by PLD)
                    db      0x00,0x00    ; R12,R13 Start Address
                    db      0x00,0x00    ; R14,R15 Cursor Address

; Palette colors are 0-63 not 0-255 so FF=3F, AA=2A, 55=15
; First 16 are CGA, then 4 MDA with green, 4 MDA with White   P4=0 CGA, P4=1 MDA, P2=0 Green P2=1 White
PALETTE             db      0x00, 0x00, 0x00  ; Black (#000000)
                    db      0x00, 0x00, 0x2A  ; Blue (#0000AA)
                    db      0x00, 0x2A, 0x00  ; Green (#00AA00)
                    db      0x00, 0x2A, 0x2A  ; Cyan (#00AAAA)
                    db      0x2A, 0x00, 0x00  ; Red (#AA0000)
                    db      0x2A, 0x00, 0x2A  ; Magenta (#AA00AA)
                    db      0x2A, 0x15, 0x00  ; Brown (#AA5500)
                    db      0x2A, 0x2A, 0x2A  ; Light Gray (#AAAAAA)
                    db      0x15, 0x15, 0x15  ; Dark Gray (#555555)
                    db      0x15, 0x15, 0x3F  ; Light Blue (#5555FF)
                    db      0x15, 0x3F, 0x15  ; Light Green (#55FF55)
                    db      0x15, 0x3F, 0x3F  ; Light Cyan (#55FFFF)
                    db      0x3F, 0x15, 0x15  ; Light Red (#FF5555)
                    db      0x3F, 0x15, 0x3F  ; Light Magenta (#FF55FF)
                    db      0x3F, 0x3F, 0x15  ; Yellow (#FFFF55)
                    db      0x3F, 0x3F, 0x3F  ; White (#FFFFFF)

                    db      0x00, 0x00, 0x00  ; MDA Black (#000000) background
                    db      0x00, 0x2A, 0x00  ; Green foreground
                    db      0x00, 0x15, 0x00  ; Dark Green - bright background
                    db      0x00, 0x3F, 0x00  ; Bright Green - bright foreground

                    db      0x00, 0x00, 0x00  ; MDA White - Black (#000000) background
                    db      0x2A, 0x2A, 0x2A  ; Green foreground
                    db      0x15, 0x15, 0x15  ; Dark Green - bright background
                    db      0x3F, 0x3F, 0x3F  ; Bright Green - bright foreground
                    
; TOPLINE             ldx      #VRAM+1
                    ; lda      #$01
; TOPLINE1            lbsr      PRINTDIGIT
                    ; adda      #$01
                    ; daa
                    ; cmpx      #VRAM + COLSPERROW * BYTESPERCHAR 
                    ; ble       TOPLINE1
; ; Print 10's under 1st line

; SECLINE             ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
                    ; lda      #$1
; SECLINE1            lbsr      PRINTDIGIT ; This will increase X by 2
                    ; leax     9*BYTESPERCHAR,X
                    ; adda     #$01
                    ; daa
                    ; cmpx     #VRAM + 2 * COLSPERROW * BYTESPERCHAR ; 2 Bytes per character 0/even is character 1/odd is colour
                    ; ble      SECLINE1


; LOWLINE             ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR 
                    ; lda      #$01
; LOWLINE1            bsr      PRINTDIGIT
                    ; adda     #$01
                    ; daa
                    ; cmpx     #VRAM + COLSPERROW * BYTESPERCHAR * ROWSPERPAGE; 2 Bytes per character 0/even is character 1/odd is colour
                    ; ble      LOWLINE1

; LOWSECLINE          ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - 2 * COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
                    ; lda      #$1
; LOWSECLINE1         bsr      PRINTDIGIT ; This will increase X by 2
                    ; leax     9*BYTESPERCHAR,X
                    ; adda     #$01
                    ; daa
                    ; cmpx     #VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR
                    ; ble      LOWSECLINE1


; LINENO              lda      #$02
                    ; ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR
; LINENO1             bsr      PRINTNUM
                    ; leax     76*BYTESPERCHAR,X  ; Move to end of line
                    ; bsr      PRINTNUM ; X should be start of next line after this
                    ; adda     #$01
                    ; daa
                    ; cmpa     #ROWSPERPAGEBCD
                    ; bne      LINENO1
                    ; ;SWI
                    ; ; +7 (4 char), 16 x 4 char= 64=128 bytes + 25
                    ; ; Add 32 for end of line (16 char)
; CHARDUMP            clra      ; Display two digit character code, character, 1 spaces for 0-255, start at Col 4 (add 7)
                    ; ldx       #VRAM - 32 + 7 + 2 * COLSPERROW * BYTESPERCHAR
; CHARDUMP1           clrb
                    ; leax      16*BYTESPERCHAR,X
; CHARDUMP2           bsr       PRINTHEX
                    ; sta       ,X++
                    ; leax      1*BYTESPERCHAR,X
                    ; inca
                    ; incb
                    ; cmpb      #$10
                    ; bne       CHARDUMP2
                    ; tsta
                    ; bne       CHARDUMP1

                    ; ; +7 (4 char), 16 x 4 char= 64=128 bytes + 25
                    ; ; Add 32 for end of line (16 char)
; COLDUMP             clra      ; Display two digit character code, character, 1 spaces for 0-255, start at Col 4 (add 7)
                    ; ldx       #VRAM - 32 + 7 + 20 * COLSPERROW * BYTESPERCHAR
                    ; ldb       #$20
                    ; leax      32,X
; COLDUMP1            bsr       PRINTHEX
; ;COLDUMP1            leax      4,X
                    ; sta       -1,X
                    ; stb       ,X
                    ; leax      4,X
                    ; inca
                    ; cmpa      #$10
                    ; bne       COLDUMP1

; CRTEND
                    ; jmp       CLRSCREEN
                    
; ; 12345678901234567890
; ; 02       1         2 ..... 02

; ; Take BCD two digit number in A and print at X
; PRINTNUM            pshs     a           ; Save A twice
                    ; pshs     a
                    ; anda     #%11110000  ; Mask LSB
                    ; lsra                 ; Rotate MSB down to LSB
                    ; lsra
                    ; lsra
                    ; lsra
                    ; bsr PRINTDIGIT
                    ; puls     a           ; Restore A
                    ; anda     #%00001111  ; Mask MSB
                    ; bsr PRINTDIGIT
                    ; puls     a,pc

; ; Take Lower 4 bits in A and print ASCII digit at X and increment X
; PRINTDIGIT          pshs     a
                    ; anda     #%00001111  ; Strip any MSB
                    ; adda     #ASCII0     ; Add 48 decimal, 0=$30, 1=$31
                    ; sta      ,X++
                    ; puls     a,pc

; PRINTHEX            pshs     a            ; Save byte value as need to return since used for checksum calc
                    ; pshs     a            ; Save again so we can mask top and lower nibbles
                    ; anda     #%11110000   ; Mask High nibble
                    ; lsra                  ; Shift to Low nibble
                    ; lsra 
                    ; lsra
                    ; lsra
               
                    ; adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal
                    ; daa                    ; 0-9  $30-$39 A-F $41-$46
                    ; adca     #$40
                    ; daa
                    ; sta     ,X++
               
                    ; puls     a 
                    ; anda     #%00001111    ; Mask Low nibble

                    ; adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal
                    ; daa
                    ; adca     #$40
                    ; daa
                    ; sta     ,X++
                    ; puls     a
                    ; rts
                    



                     