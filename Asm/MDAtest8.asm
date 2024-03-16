;=============================================================================
; MDAtest6.asm - Nanocomp 8086 MDA Video Controller Test 8 Attributes & Colour No Blink, POST Test
; nasm -f bin -o MDAtest8.bin -l MDAtest8.lst MDAtest8.asm
; nasm -f srec -o MDAtest8.srec -l MDAtest8.lst MDAtest8.asm

; Version          Date            Description
;  1.0             05/03/24        Based on MDATest7 with Attribute Test Pattern
;                                  After test pattern output 0-255 to POST Port 0x80 
         
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

%define	START		0x0000		; Default Physical Address is 70000 [7000:0000]

%define DATASEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define USERCODESEG 0x7000    ; Data Segment Same as stack until we add setting Segments to Monitor
%define STACKSEG 0x7000   ; Stack Segment is current top of 512K RAM minus 256 bytes for Monitor use
%define MONCODESEG 0xF000 ; Monitor Code Segment
%define GETKEY 0x81be   ; Address in EEPROM of Getkey which calls DISPRESH while waiting for a key

org	START		; Use only upper 32 KiB of ROM

;   Reset Handler
teststart:
CRCCONTROL          EQU  0x038
CRCCSTATUS          EQU  0x038A4
CRTCAR              EQU  0x03b4
CRTCDR              EQU  0x03b5
VRAMSEG             EQU  0xB000
VRAM                EQU  0x0000
VRAMTOP             EQU  0x0FFF

CRTC_DAC_ADD_WR     EQU  0x03b0
CRTC_DAC_COL_VALUE  EQU  0x03b1
CRTC_DAC_PIXEL_MASK EQU  0x03b2
CRTC_DAC_ADD_RD     EQU  0x03b3
CRT_CONTROL         EQU  0x03b8
CRT_STATUS          EQU  0x03bA


ASCII0              EQU  0x30
ASCIISPACE          EQU  0x20
COLSPERROW          EQU  0x50
ROWSPERPAGE         EQU  0x19
ROWSPERPAGEBCD      EQU  0x25
BYTESPERCHAR        EQU  02
CHARATTRIB          EQU  0b00001111 ; Black background, White/Green foreground, No Blink

EOS                 EQU  0x00 ;      end of string

                    ORG  0x0000
                    
CRTStart:
                  mov ax, DATASEG
                  mov ds, ax     ; Set ds to 0x7000
                  mov ax, VRAMSEG
                  mov es, ax     ; Set es to MDA frame buffer 80000 [8000:0000]
                  xor ax, ax     ; A=al, B=ah
                  mov si, CRTCTAB 
                    
CRTCLOOP:         mov al, ah     ;stb      CRTCAR      
                  mov dx, CRTCAR
                  out dx, al     ; Save Rn in CRTC Address Register
                  mov al, [cs:si]; lda      ,X+        ; Get Rn value from CRTC Table single + for +1, ++ is +2 and wrong!
                  mov dx, CRTCDR
                  out dx, al     ;  Save Rn data value in CRTC Data Register
                  inc si 
                  inc ah 
                  cmp ah, 0x10   ; Have we processed all register values                 
                  jne CRTCLOOP

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
                  
                  ;int3
; Set Control Port to B/W=1 Bit=1, Video Enable = 1 Bit = 3 and Blink=1 Bit = 5  00101010 (Blink doesn't affect colour)
                  mov al, 0b00101010
                  mov dx, CRT_CONTROL
                  out dx, al
                  
CLRSCREEN:        xor di, di         ; ldx      #VRAM
                  mov ax, VRAMSEG
                  mov es, ax                  
                  mov al, ASCIISPACE ; lda      #ASCIISPACE
                  mov ah, CHARATTRIB ; ldb      #CHARCOLOUR
CLEARSCREEN1      mov [es:di], ax    ; stb      ,X+
                  add di, 0x02
                  cmp di, VRAMTOP    ; cmpx     #VRAMTOP
                  jbe CLEARSCREEN1   ; ble      CLEARSCREEN1
                  




; For X the 8088 code will use es:di
; Character value is even byte (LSB al), Attribute is odd byte (MSB ah)                    
TOPLINE:
    xor di, di         ; ldx      #VRAM
    mov ax, VRAMSEG
    mov es, ax                  
    mov al, 0x00       
    mov ah, CHARATTRIB ; ldb      #CHARCOLOUR

    mov al, 0x01       ; lda      #$01
TOPLINE1:
    call PRINTDIGIT    ; TOPLINE1            lbsr      PRINTDIGIT
    inc al             ; adda      #$01
    daa                ; daa
    cmp di, VRAM + COLSPERROW * BYTESPERCHAR -1 ; cmpx      #VRAM + COLSPERROW * BYTESPERCHAR 
    jbe TOPLINE1       ; ble       TOPLINE1

; Print 10's under 1st line

SECLINE:
    mov di, VRAM + COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR; SECLINE             ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
    mov al, 0x01       ; lda      #$1
SECLINE1:    
    call PRINTDIGIT    ; SECLINE1            lbsr      PRINTDIGIT ; This will increase X by 2
    add di, 9*BYTESPERCHAR; leax     9*BYTESPERCHAR,X
    inc al             ; adda     #$01
    daa                ; daa
    cmp di, VRAM + 2 * COLSPERROW * BYTESPERCHAR -1 ; cmpx     #VRAM + 2 * COLSPERROW * BYTESPERCHAR ; 2 Bytes per character 0/even is character 1/odd is colour
    jbe SECLINE1       ; ble      SECLINE1


LOWLINE:
    mov di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR ; LOWLINE             ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR 
    mov al, 0x01       ; lda      #$01
LOWLINE1:
    call PRINTDIGIT    ; LOWLINE1            bsr      PRINTDIGIT
    inc al             ; adda     #$01
    daa                ; daa
    cmp di,VRAM + COLSPERROW * BYTESPERCHAR * ROWSPERPAGE -1 ; cmpx     #VRAM + COLSPERROW * BYTESPERCHAR * ROWSPERPAGE; 2 Bytes per character 0/even is character 1/odd is colour
    jbe LOWLINE1       ; ble      LOWLINE1

LOWSECLINE:
    mov di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - 2 * COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR; LOWSECLINE          ldx      #VRAM + 1 + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - 2 * COLSPERROW * BYTESPERCHAR + 9 * BYTESPERCHAR ; Start at 10th Column
    mov al, 0x01       ; lda      #$1
LOWSECLINE1:
   call PRINTDIGIT     ; LOWSECLINE1         bsr      PRINTDIGIT ; This will increase X by 2
   add di, 9*BYTESPERCHAR ; leax     9*BYTESPERCHAR,X
   inc al              ; adda     #$01
   daa                 ; daa
   cmp di, VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR - 1; cmpx     #VRAM + ROWSPERPAGE * COLSPERROW * BYTESPERCHAR - COLSPERROW * BYTESPERCHAR
   jbe LOWSECLINE1     ; ble      LOWSECLINE1

LINENO:
    mov al, 0x02       ; LINENO              lda      #$02
    mov di, VRAM + COLSPERROW * BYTESPERCHAR ; ldx      #VRAM + 1 + COLSPERROW * BYTESPERCHAR
LINENO1:
    call PRINTNUM      ; LINENO1             bsr      PRINTNUM
    add di, 76*BYTESPERCHAR ; leax     76*BYTESPERCHAR,X  ; Move to end of line
    call PRINTNUM      ; bsr      PRINTNUM ; X should be start of next line after this
    inc al             ; adda     #$01
    daa                ; daa
    cmp al, ROWSPERPAGEBCD; cmpa     #ROWSPERPAGEBCD
    jne LINENO1        ; bne      LINENO1

   
attribute:
; Set Control Port to B/W=1 Bit=1, Video Enable = 1 Bit = 3 and Blink=1 Bit = 5  00101010
    mov al, 0b00001000
    mov dx, CRT_CONTROL
    out dx, al
; Blink is enabled 1st time around    
ATTRDUMP:
    xor ax, ax      ;
    ;mov al, ASCIISPACE ; Overwrite original character, Hex digits will show attribute    
    mov di, VRAM + 6 + 2 * COLSPERROW * BYTESPERCHAR  - 32 ; ldx       #VRAM - 32 + 7 + 2 * COLSPERROW * BYTESPERCHAR
ATTRDUMP1:
    xor bx, bx      ; CHARDUMP1           clrb need to use ah for attributes
    add di, 16*BYTESPERCHAR ; leax      16*BYTESPERCHAR,X
ATTRDUMP2:
    call PRINTHEX   ; CHARDUMP2           bsr       PRINTHEX
    mov [es:di], ax ; sta       ,X++
    add di, 0x2
    add di, 1*BYTESPERCHAR; leax      1*BYTESPERCHAR,X
    inc al
    inc ah          ; Next attribute
    inc bl          ; incb
    cmp bl, 0x10    ; cmpb      #$10
    jne ATTRDUMP2   ; bne       CHARDUMP2
    cmp ah, 0x00    ; tsta
    jne ATTRDUMP1   ; bne       CHARDUMP1


COLDUMP:
    xor ax, ax      ;
    ;mov al, ASCIISPACE ; Overwrite original character, Hex digits will show attribute    
COLDUMP1:
    xor bx, bx      ; CHARDUMP1           clrb need to use ah for attributes
    add di, 16*BYTESPERCHAR ; leax      16*BYTESPERCHAR,X
COLDUMP2:
    mov ah, 0x0f
    mov al, bl
    call PRINTHEX   ; CHARDUMP2           bsr       PRINTHEX
    mov al, 0xDB
    mov ah, bl
    mov [es:di], ax ; sta       ,X++
    add di, 0x2
    add di, 1*BYTESPERCHAR; leax      1*BYTESPERCHAR,X
    inc bl          ; incb
    cmp bl, 0x10    ; cmpb      #$10
    jne COLDUMP2   ; bne       CHARDUMP2

POSTTEST:
    mov cx, 0xFF
POSTTEST1:
    nop ; Delay a little so get gap between writes
    nop
    nop
    nop
    nop
    mov al, cl 
    out 0x80, al  ; Write POST code to POST register
    loop POSTTEST1
    
    int3;
    
; ; Take BCD two digit number in al (A) and print at es:di (X) 
PRINTNUM:                       ; PRINTNUM            pshs     a           ; Save A twice
    push ax                     ; pshs     a
    push ax
    and al, 0b11110000          ; anda     #%11110000  ; Mask LSB
    shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    shr al,0x01                 ; lsra                 ; Rotate MSB down to LSB
    call PRINTDIGIT             ; bsr PRINTDIGIT
    pop ax                      ; puls     a           ; Restore A
    and al, 0b00001111          ; anda     #%00001111  ; Mask MSB
    call PRINTDIGIT             ; bsr PRINTDIGIT
    pop ax
    ret                         ; puls     a,pc

; ; Take Lower 4 bits in al (A) attribute in ah and print ASCII digit at es:di (X) and increment di by 2 (char & attribute)
PRINTDIGIT:
    push ax                     ; PRINTDIGIT          pshs     a
    and al, 0b00001111          ; anda     #%00001111  ; Strip any MSB
    add al, ASCII0              ; adda     #ASCII0     ; Add 48 decimal, 0=$30, 1=$31
    mov [es:di], ax             ; sta      ,X++
    add di, 0x02
    pop ax
    ret                         ; puls     a,pc

PRINTHEX:                       ;                  pshs     a            ; Save byte value as need to return since used for checksum calc
    push ax                     ;                  pshs     a            ; Save byte value as need to return since used for checksum calc
    push ax                     ;                  pshs     a            ; Save again so we can mask top and lower nibbles
    ; Must use al as daa only works with this register
    and al, 0b11110000          ;                  anda     #%11110000   ; Mask High nibble
    shr al,0x01                 ;                  lsra                  ; Shift to Low nibble
    shr al,0x01                 ;                  lsra 
    shr al,0x01                 ;                  lsra
    shr al,0x01                 ;                  lsra

    add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal 0-9  $30-$39 A-F $41-$46
    daa                         ;                  daa                    ; DAA on adding $90 sets carry which Makes 0-9 3x and A-F be 4x in adca
    adc al, 0x40                ;                  adca     #$40          ; Add $40 makes 0-9 D0-D9  in Decimal 130-139, A-F A0-A5 +$40 + Carry in decimal 141-146 = $41-$46
    daa                         ;                  daa                    ; Strips 100 from result to give 30-39 and 41-46

    mov [es:di], ax             ; sta      ,X++
    add di, 0x02

    pop ax                      ;                  puls     a 
    and al, 0b00001111          ;                  anda     #%00001111    ; Mask Low nibble

    add al, 0x90                ;                  adda     #$90          ; LSB to ASCII Hex as per page 7-2 of Leventhal, same as above
    daa                         ;                  daa
    adc al, 0x40                ;                  adca     #$40
    daa                         ;                  daa
    ; mov ah, al now using al for A

    mov [es:di], ax             ; sta      ,X++
    add di, 0x02
    pop ax                      ;                  puls     a
    ret                         ;                  rts

  
  
CRTCTAB             db      0x63         ; R0 H 62 to 64 Total 99
                    db      0x50         ; R1 H Displayed 80
                    db      0x54         ; R2 H from x53 to x55 Sync Position 83 0x53
                    db      0x0C         ; R3 H Sync Width 12/0C
                    db      0x1B         ; R4 V Total 27
                    db      0x03         ; R5 V Total Adjust (was 13/$0D)
                    db      0x19         ; R6 V Displayed 25
                    db      0x1A         ; R7 V Sync Position 26
                    db      0x50         ; R8 Interlace mode - Non Interlaced, DISPEN SKEW 1 Char 10, CUR SKEW 1 char 40
                    db      0x0F         ; R9 Maximum Scan Line Address 
                    db      0x0d         ; R10 Cursor Start - No Blink + Line 13 Start  Cursor off $0x20
                    db      0x0f         ; R11 Cursor End - No Blink + Line 15 Finish (Blinking coordinated with char blink by PLD)
                    db      0x00,0x00    ; R12,R13 Start Address
                    db      0x00,0x00    ; R14,R15 Cursor Address

; Palette colors are 0-63 not 0-255 so FF=3F, AA=2A, 55=15
; First 16 are CGA, then 4 MDA with green, 4 MDA with White   P4=0 CGA, P4=1 MDA, P2=0 Green P2=1 White
; Just for for character generator testing need black & White
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

PALETTE             db      0x00, 0x00, 0x00  ; Black (#000000)
;                    db      0x3F, 0x3F, 0x3F  ; White (#FFFFFF) Comment this in for testing using P0 for fore/back, comment out blue
                    db      0x00, 0x00, 0x31  ; Blue (#0000C4)
                    db      0x00, 0x31, 0x00  ; Green (#00C400)
                    db      0x00, 0x31, 0x31  ; Cyan (#00C4C4)
                    db      0x31, 0x00, 0x00  ; Red (#C40000)
                    db      0x31, 0x00, 0x31  ; Magenta (#C400C4)
                    db      0x31, 0x1F, 0x00  ; Brown (#C47E00)
                    db      0x31, 0x31, 0x31  ; Light Gray (#C4C4C4)
                    db      0x13, 0x13, 0x13  ; Dark Gray (#4E4E4E)
                    db      0x13, 0x13, 0x31  ; Light Blue (#4E4EDC)
                    db      0x13, 0x37, 0x13  ; Light Green (#4EDC4E)
                    db      0x13, 0x3c, 0x3c  ; Light Cyan (#4EF3F3)
                    db      0x37, 0x13, 0x13  ; Light Red (#DC4E4E)
                    db      0x3c, 0x13, 0x3c  ; Light Magenta (#F34EF3)
                    db      0x3c, 0x3c, 0x13  ; Yellow  (#F3F34E)
                    db      0x3F, 0x3F, 0x3F  ; White (#FFFFFF)

                    db      0x00, 0x00, 0x00  ; MDA Black (#000000) background
                    db      0x00, 0x31, 0x00  ; Green foreground
                    db      0x00, 0x13, 0x00  ; Dark Green - bright background
                    db      0x00, 0x3F, 0x00  ; Bright Green - bright foreground

                    db      0x00, 0x00, 0x00  ; MDA White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

