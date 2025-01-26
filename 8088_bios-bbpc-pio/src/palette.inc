; palette.inc this is only included for BB PC
; To work with MDA and CGA we need to use the saved CRTC Address Port either MDA 0x03b4 or CGA 0x03d4
CRTC_DAC_ADD_WR     EQU  0x00
CRTC_DAC_COL_VALUE  EQU  0x01
CRTC_DAC_PIXEL_MASK EQU  0x02
CRTC_DAC_PORT_MASK  EQU  0xFFF0

INITPALETTE:
  cli
  push	si
  push	dx
  push	cx
  push	bx
  push	ax
;mov	al,0x99  ; MDA Pallette Init!
;out	post_reg,al

  mov	dx,word [video_port] ; Get current MDA/CGA CRTC port 3b4 or 3d4
  and   dx,CRTC_DAC_PORT_MASK
  add   dx, CRTC_DAC_PIXEL_MASK
;  mov dx, CRTC_DAC_PIXEL_MASK 
  mov al, 0xFF   
  out dx, al                 ; Pixel mask set so disabled

  xor bx, bx
  mov si, PALETTE
  
;  mov cx, 24                 ; 24 Palette entries 16 CGA, 4 MDA Green, 4 MDA White
  mov cx, 32                 ; 32 Palette entries 16 CGA, 16 B&W

  mov	dx,word [video_port] ; Get current MDA/CGA CRTC port 3b4 or 3d4
  and   dx,CRTC_DAC_PORT_MASK ; dx masked will now be ADD_WR port

NEXTPALETTE:

  ;mov dx, CRTC_DAC_ADD_WR 
  mov al, bl                  
  out dx, al                 ; Output Palette address No.

  inc dx                     ; Change dx to CRTC_DAC_COL_VALUE 
  mov al, [cs:si]
  inc si
  out dx, al                 ; Output Red 
  nop
  nop
  mov al, [cs:si]
  inc si
  out dx, al                 ; Output Green 
  nop
  nop
  mov al, [cs:si]
  inc si
  out dx, al                 ; Output Blue 
  nop
  nop
  inc bl
  dec dx                     ; switch back to CRTC_DAC_ADD_WR
  LOOP NEXTPALETTE

	;mov	al,0x9A  ; MDA Palette Init finished!
	;out	post_reg,al
  
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
  sti
  ret
  
; MDA and CGA Palette values for IMS176P VGA Video DAC
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

;                    db      0x00, 0x00, 0x00  ; MDA Green - Black (#000000) background
;                    db      0x00, 0x31, 0x00  ; Green foreground
;                    db      0x00, 0x13, 0x00  ; Dark Green - bright background
;                    db      0x00, 0x3F, 0x00  ; Bright Green - bright foreground

;                    db      0x00, 0x00, 0x00  ; MDA White - Black (#000000) background
;                    db      0x31, 0x31, 0x31  ; Light Grey foreground
;                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
;                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                                              ; Order of greyscale values may need adjusting
                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground

                    db      0x00, 0x00, 0x00  ; CGA Greyscale White - Black (#000000) background
                    db      0x31, 0x31, 0x31  ; Light Grey foreground
                    db      0x13, 0x13, 0x13  ; Dark Grey - bright background
                    db      0x3F, 0x3F, 0x3F  ; White - bright foreground
