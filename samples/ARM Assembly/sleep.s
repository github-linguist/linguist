;   File        : sleep.s
;   Date        : 19-Sep-02
;   Author      : © A.Thoukydides, 1998-2002, 2019
;   Description : Sleeping in a taskwindow for the PsiFS module.
;
;   License     : PsiFS is free software: you can redistribute it and/or
;                 modify it under the terms of the GNU General Public License
;                 as published by the Free Software Foundation, either
;                 version 3 of the License, or (at your option) any later
;                 version.
;   
;                 PsiFS is distributed in the hope that it will be useful,
;                 but WITHOUT ANY WARRANTY; without even the implied warranty
;                 of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
;                 the GNU General Public License for more details.
;   
;                 You should have received a copy of the GNU General Public
;                 License along with PsiFS. If not, see
;                 <http://www.gnu.org/licenses/>.

; Include system header files

        GET     OS:Hdr.OS
        GET     OS:Hdr.TaskWindow

; Include my header files

        GET     AT:Hdr.macros

; Exported symbols

        EXPORT  sleep_taskwindow
        EXPORT  sleep_snooze

; Place in a code area

        AREA    |C$$code|, REL, CODE, READONLY

; os_error *sleep_taskwindow(bool *task)
sleep_taskwindow
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy pointer to result variable
        MOV     r0, #TaskWindowTaskInfo_WindowTask; Reason code to read info
        SWI     XTaskWindow_TaskInfo    ; Check if in a taskwindow
        BVS     err$l                   ; Skip to the end if an error returned
        TEQ     r1, #0                  ; Is task a valid pointer
        STRNE   r0, [r1]                ; Store return r0 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   MOVS    pc, ip                  ; Return from subroutine

; os_error *sleep_snooze(int *poll)
sleep_snooze
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy pointer to poll word
        MOV     r0, #TaskWindowTaskInfo_WindowTask; Reason code to read info
        SWI     XTaskWindow_TaskInfo    ; Check if in a taskwindow
        BVS     done$l                  ; Skip to the end if an error returned
        TEQ     r0, #0                  ; Is it a taskwindow
        BEQ     done$l                  ; Exit if not
        MOV     r0, #UpCall_Sleep       ; Reason code to sleep
        SWI     XOS_UpCall              ; Perform the UpCall
        MOVVC   r0, #0                  ; Clear error pointer if successful
done$l  MOVS    pc, ip                  ; Return from subroutine

        END
