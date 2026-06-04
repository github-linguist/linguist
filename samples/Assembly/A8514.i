

;;/*....................Bit Definitions....................................*/
NOERROR	equ	00h
NO8514A		equ 80h
;;/*....................GC250 REGISTERS....................................*/
DISP_STAT		equ 02E8h	;/* Display Status Register		*/
H_TOTAL			equ 02E8h  ;/* Horizontal Total Register	*/
DAC_MASK		equ 02EAh	;/* DAC Mask 					*/
DAC_RINDEX		equ 02EBh	;/* DAC read index register		*/
DAC_WINDEX		equ 02ECh	;/* DAC write index register		*/
DAC_DATA		equ 02EDh	;/* DAC Data register			*/
H_DISPLAYED		equ 06E8h	;/* Horiz Displayed Reg			*/
H_SYNC_STRT		equ 0AE8h	;/* Horiz Sync Start Reg			*/
H_SYNC_WID		equ 0EE8h	;/* Horiz Sync Width Reg			*/
V_TOTAL			equ 12E8h	;/* Vertical Total Reg			*/
V_DISPLAYED		equ 16E8h	;/* Vertical Displayed Reg		*/
V_SYNC_STRT		equ 1AE8h	;/* Vertical Sync Start Reg		*/
V_SYNC_WID		equ 1EE8h	;/* Vertical Sync Width Reg		*/
DISP_CNTL		equ 22E8h	;/* Display Control Register		*/
SUBSYS_CNTL		equ 42E8h	;/* Subsystem Control Reg		*/
SUBSYS_STAT 	equ 42E8h	;/* Subsystem Status Reg			*/
ROM_PAGE_SEL	equ 46E8h	;/* ROM Page Select Reg			*/
ADVFUNC_CNTL	equ 4AE8h	;/* Adv Func Control Reg			*/
CUR_Y_POS		equ 82E8h	;/* Current Y Position			*/
CUR_X_POS		equ 86E8h	;/* Current Y Position			*/
DESTY_AXSTP		equ 8AE8h	;/* Dest Y/Axial Step Reg		*/
DESTX_DIASTP	equ 8EE8h	;/* Dest X/Diagl Step Reg		*/
ERR_TERM		equ 92E8h	;/* Error Term Register			*/
MAJ_AXIS_PCNT	equ 96E8h	;/* Major Axis Pixel Count		*/
COMMAND			equ 9AE8h	;/* Command register				*/
GP_STATUS		equ 9AE8h	;/* Graphics Processor Status	*/
CMD_STATUS		equ 9AE9h	;/* Command status register		*/
SHORT_STROKE	equ 9EE8h	;/* Short Stroke Vector Reg		*/
BKGD_COLOR		equ 0A2E8h	;/* Background Color 			*/
FRGD_COLOR		equ 0A6E8h	;/* Foreground Color 			*/
WRT_MASK		equ 0AAE8h	;/* Write Mask					*/
RD_MASK			equ 0AEE8h	;/* Read Mask					*/
COLOR_CMP		equ 0B2E8h	;/* Color Compare Register		*/
BKGD_MIX		equ 0B6E8h	;/* Background Mix Register		*/
FGRD_MIX		equ 0BAE8h	;/* Foreground Mix Register		*/
MLTFUNC_CNTL	equ 0BEE8h	;/* Multifunction Control		*/
PIX_TRANS		equ 0E2E8h	;/* Pixel Data Transfer Reg.		*/
;/* ..................Bit definitions of Registers .....................*/
CMD_ACTIVE		equ 02h	;/* Command is active? */
DATA_AVAIL		equ 01h	;/* Input Data Available? */
NO8514			equ 40h	;/* No 8514 Monitor present */
MONOCHROME 		equ 10h	;/* Monochrome Monitor? */
PLANE8			equ 80h	;/* 8 plane memory available */
;/* ..................COMMAND mask bits ................................*/
WRITCMD		equ 01h
PLANAR		equ 02h
LSTPELNULL	equ 04h
STROKE_ALG	equ 08h
DRAWCMD		equ 10h
INCX		equ 20h
YMAJAXIS	equ 40h
INCY		equ 80h
PC_TRANS	equ 100h
BIT16		equ 200h
BYTE_SWAP	equ 1000h
NO_FCN		equ 0000h
LINE_DRAW	equ 2000h
FILL_X_RECT	equ 4000h
FILL_Y_RECT	equ 6000h
FILL_RECT	equ 8000h
AXIAL_LINE	equ 0A000h
COPY_RECT	equ 0C000h
HANG		equ 0E000h
;/* ..................MIX Defines ........................................*/
MIX_NOT_DEST		equ 00h
MIX_ZERO			equ 01h
MIX_ONE				equ 02h
MIX_DEST			equ 03h
MIX_NOT_SRC			equ 04h
MIX_SRC_XOR_DEST	equ 05h
MIX_NOT				equ 06h
MIX_SRC				equ 07h
;/* .................MIX Sources .........................................*/
B_CLR_ACTIVE	equ 00h
F_CLR_ACTIVE	equ 20h
PTRANS_ACTIVE	equ 40h
ALL_PLANE_CPY	equ 60h
;/* .................MLTFUNC_CNTL ... high order nibble is an index .......*/
MINOR_AXIS_PCNT	equ 0000h
SCISSOR_T		equ 1000h
SCISSOR_L		equ 2000h
SCISSOR_B		equ 3000h
SCISSOR_R		equ 4000h
MEM_CNTL		equ 5000h
PIX_CNTL		equ 0A000h
;/* Mix operation select */
ONE_PLN_COPY 	equ 0C0h
;/* Write transfers use fgdmix for 1's and bkgdmix for 0's */
WRT_PLN_MODE	equ 080h
;/* Write transfers use fgdmix for 1's and bkgdmix for 0's */
PIX_MIX_SEL		equ 040h
FGD_MIX_ACT		equ 0000h
;/* Misc bit deinitions */
STOP_SEQ		equ 9000h	;/* subsystem cntl reg */
START_SEQ		equ 5000h	;/* subsystem cntl reg */
RESET_QUEUE_FULL equ 04h

VP1024	equ 0		;/* AI Monde 0,2,3		*/
VP644	equ 1		;/* AI Mode 1 (4 plane)  */
VP648	equ 2		;/* AI Mode 1 (8 plane)	*/


;/*.........................CONTROL WORD BITFIELDS...................*/
GP_READ		equ 0
GP_WRITE	equ 1				;/* Writing or reading screen */
GP_PIXEL	equ 0
GP_PLANE	equ (1<<1)			;/* Bitplane or chunky pixels */
GP_SETL		equ 0
GP_SKIPL	equ (1<<2)			;/* Skip last pixel in line? */
GP_LINEL	equ 0
GP_LINES	equ (1<<3)			;/* Short stroke or long line? */
GP_MOVE		equ 0
GP_DRAW		equ (1<<4)			;/* Draw pixels or just move x/y position */
GP_DECX		equ 0
GP_INCX		equ (1<<5)			;/* Increment or decrement x position? */
GP_AXISX	equ 0
GP_AXISY	equ (1<<6)			;/* Is Y or X the major axis? */
GP_DECY		equ 0
GP_INCY		equ (1<<7)			;/* Increment or decrement y position? */
GP_NODATA	equ 0
GP_DATA		equ (1<<8)			;/* Pixel Data Transfer register used? */
GP_BUS8		equ 0
GP_BUS16	equ (1<<9)			;/* 16 or 8 bit buss access */
GP_PIXEL8	equ 0
GP_PIXEL16	equ (1<<10)			;/* Always enabled - 16 bit internal */
GP_RESERVED equ (0<<11)
GP_NOSWAP	equ 0
GP_SWAP		equ (1<<12)			;/* Swap bytes on 16 bit PC transfers */
GPC_NOP		equ (0<<13)			;/* Do nothing */
GPC_LINE	equ (1<<13)			;/* Draw a line */
GPC_RECTX	equ (2<<13)			;/* Rectangle drawn x-wise. Pos updated */
GPC_RECTY	equ (3<<13)			;/* Rectangle drawn y-wise. Pos updated */
GPC_RECTS	equ (4<<13)			;/* Rectangle.  XY Pos not updated at end */
GPC_LINEF	equ (5<<13)			;/* Line for doing poly-fills. */
GPC_COPY	equ (6<<13)			;/* Copy to/from memory */
GPC_HANG	equ (7<<13)			;/* Make adapter hang & need reboot */


Wait_free  MACRO
           LOCAL     WaitLoop

           mov       DX, CMD_STATUS       ; Make sure the board's not busy...
WaitLoop:  in        AX, DX            ; Read the status.
           test      AX, CMD_ACTIVE          ; Is the machine busy?
           jnz       WaitLoop          ; Yes, try again.
           ENDM

Wait_queue macro
	local bloop
	mov dx,GP_STATUS
bloop:
	in al,dx
	test al,02h
	jnz	bloop
	endm





rast8514 struc
	vm_type dw ?
	vm_pdepth dw ?
	vm_lib dd ?
	vm_aspect_dx dw ?
	vm_aspect_dy dw ?
	vm_reserved dd ?
	vm_w dw ?
	vm_h dw ?
	vm_x dw ?
	vm_y dw ?
	vm_xcard dd ?
	vm_ycard dd ?
	vm_screen_ix dd ?
	vm_driver_reserved dd ?
rast8514 ends


;
; WAITQ
;
; Waits until the specified number of entries are available in the
; command queue. AX, BX, and DX are destroyed. No error checking on the
; input parameter is performed, but it must be <= 8.

WAITQ      MACRO     qEntries
           LOCAL     CheckQ

           Mov       DX, GP_STATUS     ; Get the queue status address.
CheckQ:
           In        AL, DX            ; Get the status
           test      AL, 0100H SHR (qEntries) ; Is the queue entry available?
           Jnz       CheckQ            ; Yes if result is Zero.
           ENDM
;
; CLRCMD
;
; Waits until no more commands are active.
; AX, and DX may be destroyed.

CLRCMD     MACRO
           LOCAL     WaitLoop

           mov       DX, CMD_STATUS    ; Make sure the board's not busy...
WaitLoop:  in        AL, DX            ; Read the status.
           test      AL, CMD_ACTIVE    ; Is the machine busy?
           jnz       WaitLoop          ; Yes, try again.
           ENDM

;
; CANC_XFER
;
; If we are in the pixel transfer mode, cancel out of it.

CANC_XFER  MACRO
           LOCAL     OK
           Cmp       xferMode, 0             ; Are we in pixel transfer mode?
           Je        OK                      ; No, all is well.
           OUTPW     FRGD_MIX_REG, foreMix   ; Yes, restore to previous mode.
           OUTPW     BKGD_MIX_REG, backMix   ; Restore all.
           Mov       xferMode, 0       ; Remember that we restored it.
OK:
           ENDM

