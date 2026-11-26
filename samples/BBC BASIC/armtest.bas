REM > armtest.bas
REN
REM © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
REM
REM Virtualise is free software: you can redistribute it and/or modify it
REM under the terms of the GNU General Public License as published by the Free
REM Software Foundation, either version 3 of the License, or (at your option)
REM any later version.
REM
REM Virtualise is distributed in the hope that it will be useful, but WITHOUT
REM ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
REM FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
REM more details.
REM
REM You should have received a copy of the GNU General Public License along
REM with Virtualise. If not, see <http://www.gnu.org/licenses/>.

dynamic_size% = 1000 * 1024
dynamic_area$ = "VM ARM Test"

REM Delete any old areas
old_dynamic_areas% = 0
SYS "OS_DynamicArea", 3, -1 TO , dynamic_area%
WHILE dynamic_area% <> -1
    SYS "OS_DynamicArea", 2, dynamic_area% TO ,,,,,,,, area_name$
    IF area_name$ = dynamic_area$ THEN
        PRINT "Existing area "; dynamic_area%;" found - delete? (Y/N)";
        IF INSTR("Yy", GET$) THEN
            PRINT " Yes"
            old_dynamic_areas% += 1
            SYS "XVirtualise_End", dynamic_area%
            SYS "OS_DynamicArea", 1, dynamic_area%
            dynamic_area% = -1
        ELSE
            PRINT " No"
        ENDIF
    ENDIF
    SYS "OS_DynamicArea", 3, dynamic_area% TO , dynamic_area%
ENDWHILE
IF 0 < old_dynamic_areas% THEN
    PRINT old_dynamic_areas%; " dynamic areas removed"
    END
ENDIF

REM Reserve space for the code
DIM code% 1024
P% = code%
[OPT 2
STR r0, [r0]
MOV pc, r14
]

REM Create a dynamic area
SYS "OS_DynamicArea", 0, -1, 0, -1, &80, -1, 0, 0, dynamic_area$ TO , dynamic_area%,, base%
PRINT "Created dynamic area "; dynamic_area%; " at &"; ~base%

REM Turn the area into virtual memory
SYS "Virtualise_Start", dynamic_area%, -1, 0
PRINT "Converted to virtual memory"

REM Enlarge to required size
SYS "Virtualise_Change", dynamic_area%, dynamic_size%
PRINT "Area grown"

REM Test the virtual memory
*VirtualStats
PRINT "Perform test? (Y/N)";
key$ = GET$
WHILE INSTR("Yy", key$)
    PRINT " Yes"
    PROCTest
    PRINT "Another test? (Y/N)";
    key$=GET$
ENDWHILE
PRINT " No"

REM Shrink size of area
SYS "Virtualise_Change", dynamic_area%, -dynamic_size%
PRINT "Area shrunk"

REM Return area to proper memory
SYS "Virtualise_End", dynamic_area%
PRINT "Converted back to physical memory"

REM Delete the area
SYS "OS_DynamicArea", 1, dynamic_area%
PRINT "Deleted dynamic area"

END

DEFPROCTest
    PRINT "Starting test..."
    REPEAT
        A% = base% + (RND(4) << 12)
        PRINT "(";
        CALL code%
        PRINT ")";
    UNTIL 0
    FOR loop% = 0 TO 1000
        A% = base% + RND(10 * 4096) + 4096
        B% = RND(1024) * 4 - 2048
        C% = code% + 8
        PRINT "("; loop%; ") Instruction '";
        P% = code%
        CASE RND(6)0 + 1 OF
            WHEN 1
                [OPT 2:LDMIA r0!, {r1, r2}:]
                PRINT "LDR r1, [r0, #"; B%; "]";
                D% = A%:E% = D% + 4
        ENDCASE
        PRINT "', r0 = &"; ~A%;", A = &"; ~D%; "-&"; ~E%;
        code%!8 = 0
        CALL code%
        IF code%!8 = 0 THEN
            PRINT " - no page fault"
        ELSE
            IF (code%!8 = D%) AND (code%!12 = E%) THEN
                PRINT " - complete";
            ELSE
                PRINT " - failed";
                VDU 7
                IF GET
            ENDIF
            PRINT " (&"; ~code%!8; "-&"; ~code%!12; ")"
        ENDIF
    NEXT
    PRINT "Test complete"
ENDPROC
